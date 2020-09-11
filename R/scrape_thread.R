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
  thread_link <- paste0("http://gamla.familjeliv.se", suffix)
  thread_pages <- get_pages(thread_link)
  output <- vector(mode = "list", length = length(thread_pages$url))

  for (i in seq_along(output)){
    date <- get_date_time(thread_page = thread_pages[[1]][[i]], url = thread_pages[[2]][[i]]) %>% purrr::pluck(1)
    time <- get_date_time(thread_page = thread_pages[[1]][[i]], url = thread_pages[[2]][[i]]) %>% purrr::pluck(2)
    author <- get_author(thread_page = thread_pages[[1]][[i]], url = thread_pages[[2]][[i]])
    content <- get_textual_content(thread_page = thread_pages[[1]][[i]], url = thread_pages[[2]][[i]], length = length(date))
    quoted_user <- get_quoted_user(thread_page = thread_pages[[1]][[i]], url = thread_pages[[2]][[i]], length = length(date))
    required_length <- max(c(length(date), length(author), length(content)))
    length(date) <- required_length
    length(time) <- required_length
    length(author) <- required_length
    length(content) <- required_length

    content_no_quote <- remove_quotes(content, thread_pages[[1]][[i]])

    output[[i]] <- tibble::tibble(
      url = thread_link,
      date = date,
      time = time,
      author_name = author,
      quoted_user = quoted_user,
      posting = content,
      posting_wo_quote = content_no_quote
    )
  }

  output_tbl <- dplyr::bind_rows(output)

  if (is.null(file_name) == FALSE || is.null(folder_name) == FALSE) save_it(folder_name, file_name, output_tbl)

  return(output_tbl)
}
