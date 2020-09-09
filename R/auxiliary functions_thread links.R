# auxiliary functions -- scraping thread links ----------------------------

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
  rvest::html_nodes(page, ".forumListing-thread a") %>%
    rvest::html_attr("href")
}

# get threads' name

get_thread_names <- function(page) {
  rvest::html_nodes(page, ".forumListing-thread a") %>%
    rvest::html_text()
}

# get threads' start date

get_start_date <- function(page) {
  start_date <- rvest::html_nodes(page, ".forumListing-threadStarted .stamp") %>%
    rvest::html_text()
  for (i in seq_along(start_date)) {
    if (stringr::str_detect(start_date[[i]], "Idag") == TRUE) start_date[[i]] <- as.character(lubridate::today())
  }
  return(lubridate::ymd(start_date))
}

# get threads' latest entry

get_latest_entry <- function(page) {
  latest_entry <- rvest::html_nodes(page, ".forumListing-latestReply .stamp") %>%
    rvest::html_text()
  for (i in seq_along(latest_entry)) {
    if (stringr::str_detect(latest_entry[[i]], "Idag") == TRUE) latest_entry[[i]] <- as.character(lubridate::today())
    if (stringr::str_detect(latest_entry[[i]], "Obesvarat") == TRUE) latest_entry[[i]] <- NA_character_
  }
  return(lubridate::ymd(latest_entry))
}
