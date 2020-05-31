#' Get sub sections
#'
#' Returns the sub sections' names and suffixes
#'
#' @param suffix A character string. The sections suffix. Needs to start and end
#' with a slash.
#'
#' @return A tibble with two columns: 'name' and 'suffix'. The former indicates
#' the section's name, the latter its link's suffix.
#'
#' @examples
#' get_sub_sections(suffix = "/Forum-4-0/")
#'
#' @export
get_sub_sections <-
  function (suffix) {

    if (stringr::str_detect(suffix, "^/.*/$") == FALSE) stop("Suffix needs to start and end with a slash ('/')")

    if (suffix %in% c("/Forum-5-0/", "/Forum-15-0/", "/Forum-11-0/")) {
      level <- ".level4"
    } else {
      level <- ".level3, .level4"
    }

    page <- xml2::read_html(paste0("http://gamla.familjeliv.se", suffix))

    sub_section <-
      rvest::html_nodes(page, level) %>%
      rvest::html_text()
    sub_section <- sub_section[!stringr::str_detect(sub_section, "^\\n")]
    sub_section <- sub_section[c(TRUE, FALSE)]

    sub_urls <-
      rvest::html_nodes(page, level) %>%
      rvest::html_attr("href")
    sub_urls <- sub_urls[!is.na(sub_urls)]

    return(tibble::tibble(
      name = sub_section,
      suffix = sub_urls
      )
    )
}
