#' Scrape thread
#' Returns the threads' content
#'
#' @param suffix A character string containing a thread's suffix (which can be
#' obtained using \code{\link{get_thread_links()}}). Suffixes need to start with
#' \code{/}.
#' @param folder_name A character string which specifies the name of the folder
#' the output should be saved in. The folder's name is added to the path of the
#' current working directory which can be obtained using \code{getwd()} and
#' modified with \code{setwd()}. If nothing is specified and
#' \code{export_csv = TRUE}, the function will export the csv file straight into
#' the working directory.
#' @param file_name A character string which specifies the name of the output
#' file. It is not necessary to add `.csv`. If no file name is provided,
#' \code{file_name} defaults to \code{scrape_[YYYY-MM-DD].csv}.
#' @param delay A logical vector, defaults to \code{TRUE}. flashback.org's
#' robots.txt-file asks for putting a five second delay between each iteration.
#' You can deliberately ignore this by setting \code{delay = FALSE}. Note that
#' THIS IS NOT RECOMMENDED!
#'
#' @return A tibble with the following columns: \code{url} contains the thread's
#' URL suffix, \code{date} the date the posting was made on, \code{time} the
#' time the posting was made at, \code{author_name} the respective author's user
#' name, \code{author_url} the link to their profile (can be scraped using
#' \code{scrape_user_profile()}), \code{quoted_user} the user name of the user
#' that is quoted in a posting (\code{NA} if the posting does not contain a
#' quote), \code{posting} the posting *as is*, i.e., with potential quotes,
#' \code{posting_wo_quote} the posting with all quotes removed.
#'
#' @examples
#' scrape_thread(suffix = "/Forum-19-421/m57293216.html", folder_name = "sandbox", file_name = "1")
#'
#' @export
scrape_thread <- function(suffix, file_name = NULL, folder_name = NULL) {

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
