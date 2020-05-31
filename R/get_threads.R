#' Get threads
#' Returns the thread links
#'
#' @param suffix A character string. The section's suffix.
#' @param thread_start_date A character string of a date in the format
#' "YYYY-MM-DD". Links to threads which were started before this date will not
#' be returned. Defaults to "2000-01-01".
#' @param latest_entry A character string of a date in the format "YYYY-MM-DD".
#' Only links to threads whose latest entry was posted on or after this very
#' date are returned.
#'
#' @return A tibble with three columns. \code{link} contains the thread's link,
#' \code{start_date} the date it was created on, and \code{latest_entry} the
#' date when the latest posting was made within the thread.
#'
#' @examples
#' get_threads(suffix = "/Forum-19-89/", thread_start_date = "2020-04-01", latest_entry = "2020-05-01")
#'
#' @export
get_threads <- function(suffix, thread_start_date = "2000-01-01", latest_entry = "2000-01-01") {
  page <- xml2::read_html(paste0("http://gamla.familjeliv.se", suffix))
  n_pages <- get_n_pages(page)
  links <- build_links_section(suffix, n_pages)

  i <- 0

  thread_links <- vector("list", length = n_pages)
  thread_start_dates <- vector("list", length = n_pages)
  latest_entry_dates <- vector("list", length = n_pages)
  date_ind <- lubridate::today()

  while ((date_ind >= lubridate::ymd(thread_start_date)) &&
         (i < length(links))) {
    i <- i + 1
    page <- xml2::read_html(links[[i]])

    thread_links[[i]] <- get_thread_links(page)
    thread_start_dates[[i]] <- get_start_date(page)
    latest_entry_dates[[i]] <- get_latest_entry(page)

    date_ind <- lubridate::ymd(tail(latest_entry_dates[[i]], 1))#
    if (is.na(date_ind) == TRUE) date_ind <- lubridate::ymd(tail(thread_start_dates[[i]], 1))
  }
  return(tibble::tibble(
    link = thread_links %>% purrr::compact() %>% purrr::reduce(c),
    start_date = thread_start_dates %>% purrr::compact() %>% purrr::reduce(c),
    latest_entry = latest_entry_dates %>% purrr::compact() %>% purrr::reduce(c)
    ) %>%
      dplyr::filter(start_date >= lubridate::ymd(thread_start_date))
  )
}
