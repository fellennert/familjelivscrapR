#' Get threads
#' Returns the thread links
#'
#' @param mainpage The gamla.familjeliv.se forum page; default value suffices.
#'
#' @return A tibble with two columns: 'name' and 'suffix'. The former indicates
#' the section's name, the latter its link's suffix.
#'
#' @examples
#' get_main_sections()
#'
#' @export
get_thread_links <- function(suffix, thread_start_date = "2000-01-01", latest_entry = "2000-01-01") {
  page <- xml2::read_html(paste0("http://gamla.familjeliv.se", suffix))
  n_pages <- get_n_pages(suffix)
  links <- build_links_section(suffix, n_pages)
  i <- 0

  thread_links <- vector("list", length = n_pages)
  thread_start_dates <- vector("list", length = n_pages)
  latest_entry_dates <- vector("list", length = n_pages)

  while ((date_ind_thread_start_date >= lubridate::ymd(latest_entry)) &&
         (date_ind_latest_entry>= lubridate::ymd(thread_start_date)) &&
         (i < length(links))) {
    i <- i + 1
    page <- xml2::read_html(links[[i]])

    thread_links[[i]] <- get_thread_links(page)
    thread_start_dates[[i]] <-
    latest_entry_dates[[i]] <-

    thread_dates[[i]] <- get_date_links(page)
    date_ind <- tail(thread_dates[[i]], 1)
    thread_links[[i]] <- get_links(page)
    thread_links[[i]] <- thread_links[[i]][check_flyttad(page)]

  }

# start dates
# entry dates
