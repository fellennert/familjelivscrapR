#' Get main sections
#'
#' Returns the main sections' names and suffixes
#'
#' @param mainpage The gamla.familjeliv.se forum page; default value suffices.
#'
#' @return A tibble with two columns: 'name' and 'suffix'. The former indicates
#' the section's name, the latter its link's suffix.
#'
#' @examples
#' get_sections()
#'
#' @export
get_sections <-
  function (mainpage = "http://gamla.familjeliv.se/Forum-5-0/") {

    page <- xml2::read_html(mainpage)

    main_section <-
      rvest::html_nodes(page, ".level4") %>%
      rvest::html_text()
    main_section <- main_section[!stringr::str_detect(main_section, "^\\n")]
    main_section <- main_section[c(TRUE, FALSE)]

    main_urls <-
      rvest::html_nodes(page, ".level4") %>%
      rvest::html_attr("href")
    main_urls <- main_urls[!is.na(main_urls)]

    return(tibble::tibble(
      name = main_section,
      suffix = main_urls
      )
    )
}
