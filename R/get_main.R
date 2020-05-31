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
#' get_main_sections()
#'
#' @export
get_main_sections <-
  function (mainpage = "http://gamla.familjeliv.se/Forum-0-0/") {

    page <- xml2::read_html(mainpage)

    main_section <-
      rvest::html_nodes(page, ".level2") %>%
      rvest::html_text()
    main_section <- main_section[!stringr::str_detect(main_section, "^\\n")]
    main_section <- main_section[which(main_section %in% "Kategorier")+1:length(main_section)]
    main_section <- unique(main_section)
    main_section <- main_section[!is.na(main_section)]

    main_urls <-
      rvest::html_nodes(page, ".level2") %>%
      rvest::html_attr("href")
    main_urls <- main_urls[!is.na(main_urls)]
    main_urls <- main_urls[which(main_urls %in% "/Forum-5-0/"):length(main_urls)]

    return(tibble::tibble(
      name = main_section,
      suffix = main_urls
      )
    )
}
