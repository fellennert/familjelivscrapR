#' Scrape thread
#' Returns the threas' content
#'
#' @param thread_link A character string. The thread's link.
#'
#' @return A tibble with a bunch of columns. \code{link} contains the thread's link,
#' \code{start_date} the date it was created on, and \code{latest_entry} the
#' date when the latest posting was made within the thread.
#'
#' @examples
#' get_threads(suffix = "/Forum-19-89/", thread_start_date = "2020-04-01", latest_entry = "2020-05-01")
#'
#' @export
scrape_thread <- function(thread_link) {
  n_pages <- get_n_pages_thread(thread_link)
  links <- build_links_for_threads(thread_link, n_pages)

#### create REPO!!!!
}
