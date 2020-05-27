### auxiliary functions

# scraping thread links and name

# get n_pages

get_n_pages <- function(page) {
  n_threads <- rvest::html_nodes(page, ".threads strong") %>%
    rvest::html_text() %>%
    as.numeric()
  n_pages <- floor(n_threads/20+1)
  return(n_pages)
}

# build links within section

build_links_section <- function(suffix, n_pages) {
  n <- 1:n_pages
  temp_link <- stringr::str_sub(suffix, end=-2)
  links <- character(length = length(n))
  for (i in seq_along(n)) {
    links[i] <- paste0("http://gamla.familjeliv.se", temp_link, "-", n[i], "/")
  }
  return(links)
}

# get threads' links

get_thread_links <- function(page) {
  links <- rvest::html_nodes(page, ".forumListing-thread a") %>%
    rvest::html_attr("href")
  return(links)
}

# get threads' start date

get_start_date <- function(page) {

}
