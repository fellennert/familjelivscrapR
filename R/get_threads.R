#' get_threads
#' Returns the thread links
#'
#' @param suffix A character string. The section's suffix.
#' @param return_dates A logical vector. If \code{TRUE}, start date and date of
#' latest entry are returned.
#' @param thread_start_date A character string of a date in the format
#' "YYYY-MM-DD". Links to threads which were started before this date will not
#' be returned.
#' @param latest_entry_date A character string of a date in the format
#' "YYYY-MM-DD". Only links to threads whose latest entry was posted on or after
#' this very date are returned.
#'
#' @return A tibble with two columns: \code{thread_link} contains the thread's
#' link, \code{thread_name} the thread name/title. If \code{return_dates} is set
#' to \code{TRUE}, or cut-off dates are provided, \code{start_date} -- the date
#' the thread was created on, and \code{latest_entry}, the date when the latest
#' posting was added to the thread, are returned as well.
#'
#' @examples
#' get_threads(suffix = "/Forum-19-89/", return_dates = TRUE, thread_start_date = "2020-04-01", latest_entry_date = "2020-05-01")
#'
#' @export
get_threads <- function(suffix, return_dates = FALSE, thread_start_date = NULL, latest_entry_date = NULL) {
  section_link <- paste0("http://gamla.familjeliv.se", suffix)

  if (is.null(thread_start_date) == TRUE & is.null(latest_entry_date) == TRUE) pages <- scrape_section_pages(section_link)

  if (is.null(thread_start_date) == TRUE & is.null(latest_entry_date) == TRUE & return_dates == FALSE) {
    return(tibble::tibble(
      thread_link = purrr::map(pages, get_thread_links) %>% purrr::reduce(c),
      thread_name = purrr::map(pages, get_thread_names) %>% purrr::reduce(c)
    ))
  }

  if (is.null(thread_start_date) == TRUE & is.null(latest_entry_date) == TRUE & return_dates == TRUE) {
    return(tibble::tibble(
      thread_link = purrr::map(pages, get_thread_links) %>% purrr::reduce(c),
      thread_name = purrr::map(pages, get_thread_names) %>% purrr::reduce(c),
      start_date = purrr::map(pages, get_start_date) %>% purrr::reduce(c),
      latest_entry = purrr::map(pages, get_latest_entry) %>% purrr::reduce(c)
    ))
  }

  if (is.null(latest_entry_date) == FALSE) {
    pages <- scrape_section_pages_w_cutoff(section_link, cut_off = latest_entry_date, type = "latest_entry")
    return(tibble::tibble(
      thread_link = purrr::map(pages, get_thread_links) %>% purrr::reduce(c),
      thread_name = purrr::map(pages, get_thread_names) %>% purrr::reduce(c),
      start_date = purrr::map(pages, get_start_date) %>% purrr::reduce(c),
      latest_entry = purrr::map(pages, get_latest_entry) %>% purrr::reduce(c)
    ) %>%
      dplyr::filter(latest_entry >= latest_entry_date))
  }

  if (is.null(thread_start_date) == FALSE) {
    pages <- scrape_section_pages_w_cutoff(section_link, cut_off = thread_start_date, type = "start_date")
    return(tibble::tibble(
      thread_link = purrr::map(pages, get_thread_links) %>% purrr::reduce(c),
      thread_name = purrr::map(pages, get_thread_names) %>% purrr::reduce(c),
      start_date = purrr::map(pages, get_start_date) %>% purrr::reduce(c),
      latest_entry = purrr::map(pages, get_latest_entry) %>% purrr::reduce(c)
    ) %>%
      dplyr::filter(start_date >= thread_start_date))
  }
}
