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
#' scrape_thread(suffix = "/Forum-19-421/m57293216.html", folder_name = "sandbox", file_name = "1")
#'
#' @export
scrape_thread <- function(suffix, save_it = FALSE, file_name = NULL, folder_name = NULL) {

  if (stringr::str_detect(suffix, "^/Medlemsgrupper")) stop("Suffix links to group instead of thread.")

  thread_link <- paste0("http://gamla.familjeliv.se", suffix)
  thread_pages <- get_pages(thread_link)

  if(thread_pages[[1]] %>%
       rvest::html_nodes("#f-header-title") %>%
       rvest::html_text() %>%
       stringr::str_detect("Hela forumet") == TRUE) stop("Apparently, thread does not exist anymore.")

  output_tbl <- dplyr::bind_rows(build_top_post(thread_link),
                                 purrr::map2_dfr(thread_pages, thread_link, ~{
                                   build_output_tibble(thread_page = .x,
                                                       thread_link = .y)
                                   })) %>%
  mutate(quoted_user = clean_quoted_user(posting, author_name),
         quoted_user = case_when(posting == posting_wo_quote ~ NA_character_,
                                 TRUE ~ quoted_user))

  if (is.null(file_name) == FALSE || is.null(folder_name) == FALSE) save_it(folder_name, file_name, output_tbl)

  return(output_tbl)
}
