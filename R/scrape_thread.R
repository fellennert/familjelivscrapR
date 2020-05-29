#' Scrape thread
#' Returns the threads' content
#'
#' @param thread_link A character string. The thread's link.
#'
#' @return A tibble with a bunch of columns. \code{thread} contains the url
#' where you can find the thread containing the posting, \code{date} the date
#' the posting was created on, \code{time} the time it was created at,
#' \code{content} its textual content, and \code{quote_ind} indicates whether
#' it contains quoted content or not. Unfortunately, it is nearly impossible to
#' remove the quotes in a reasonable manner.
#'
#' @examples
#' scrape_thread(thread_link)
#'
#' @export
scrape_thread <- function(thread_link) {
  n_pages <- get_n_pages_thread(thread_link)
  links <- build_links_for_threads(thread_link, n_pages)
  purrr::map_dfr(links, get_output)
}

