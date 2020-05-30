#' Scrape thread
#' Returns the threads' content
#'
#' @param thread_link A character string. The thread's link.
#' @param quotes A logical vector indicating how the function should go across
#' quotes. If set to \code{TRUE}, the default value, they are kept and a column
#' is added with an indicator whether the posting contains a quote or not. If
#' set to \code{FALSE}, the function will try to remove them. This is successful
#' in the majority of cases. Sometimes, however, it fails -- seldom leading the
#' function to crash. Hence, if \code{quotes = FALSE}, it is advised to use some
#' sort of "safety net" like, for instance, \code{purrr::safely()}.
#'
#' @return A tibble with a bunch of columns. \code{thread} contains the url
#' where you can find the thread containing the posting, \code{date} the date
#' the posting was created on, \code{time} the time it was created at,
#' \code{content} its textual content, and \code{quote_ind} indicates whether
#' it contains quoted content or not. Unfortunately, it is nearly impossible to
#' remove the quotes in a reasonable manner. If the function is successful,
#' postings without quotes (either because they did not contain one in the first
#' place or because the function worked properly) can be found in
#' \code{content_wo_quote}. If the function was not successful, the entries
#' where it failed at are also in this column -- probably still contaning the
#' citation -- and devtoolsstart with "!!!flawed citation!!!".
#'
#' @examples
#' scrape_thread("/Forum-27-260/m49908859.html", quote = FALSE)
#'
#' @export
scrape_thread <- function(thread_link, quotes = TRUE) {
  if (stringr::str_detect(thread_link, "^\/Forum") == TRUE) {
    thread_link <- paste0("gamla.familjeliv.se", thread_link)
  }
  n_pages <- get_n_pages_thread(thread_link)
  links <- build_links_for_threads(thread_link, n_pages)
  output_tbl <- purrr::map_dfr(links, get_output)
  if (quotes == TRUE) return(output_tbl)
  if (quotes == FALSE) {
    quote_vec <- purrr::map(links, ~{
      xml2::read_html(.x) %>% get_quotes
    }) %>%
      purrr::reduce(c)
    return(remove_quotes(quote_vec, output_tbl))
  }
}


