# auxiliary functions -- scraping thread links ----------------------------

# get pages

scrape_section_pages <- function(section_link) {
  base_link <- stringr::str_sub(section_link, end=-2)
  pages <- vector(mode = "list", length = 10000000L)
  i <- 1
  indicator <- TRUE

  while (indicator == TRUE) {
    pages[[i]] <- xml2::read_html(paste0(base_link, "-", i, "/"))
    indicator <- pages[[i]] %>%
      rvest::html_nodes(".forumListing-thread a") %>%
      rvest::html_text() %>%
      length(.) > 0
    i <- i + 1
  }

  return(pages %>% purrr::compact())
}

### w/ cut-off

scrape_section_pages_w_cutoff <- function(section_link, cut_off, type) {
  cut_off_date <- lubridate::ymd(cut_off)
  base_link <- stringr::str_sub(section_link, end=-2)
  pages <- vector(mode = "list", length = 10000000L)
  i <- 1
  indicator <- TRUE
  indicator_date <- lubridate::today()

  if (type == "start_date") {
    while (cut_off_date <= indicator_date & indicator == TRUE) {
      pages[[i]] <- xml2::read_html(paste0(base_link, "-", i, "/"))
      indicator <- pages[[i]] %>%
        rvest::html_nodes(".forumListing-thread a") %>%
        rvest::html_text() %>%
        length(.) > 0
      indicator_date <- get_start_date(pages[[i]]) %>%
        min(., na.rm = TRUE)
      i <- i + 1
    }
  }

  if (type == "latest_entry") {
    while (cut_off_date <= indicator_date & indicator == TRUE) {
      pages[[i]] <- xml2::read_html(paste0(base_link, "-", i, "/"))
      indicator <- pages[[i]] %>%
        rvest::html_nodes(".forumListing-thread a") %>%
        rvest::html_text() %>%
        length(.) > 0
      indicator_date <- get_latest_entry(pages[[i]]) %>%
        min(., na.rm = TRUE)
      i <- i + 1
    }
  }
  return(pages %>% purrr::compact())
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
    if (is.na(start_date[[i]]) & i > 1) start_date[[i]] <- start_date[[i-1]]
    if (is.na(start_date[[i]]) & i == 1) start_date[[i]] <- start_date[[i+1]]
  }

  return(lubridate::ymd(start_date))
}

# get threads' latest entry

get_latest_entry <- function(page) {
  latest_entry <- rvest::html_nodes(page, ".forumListing-latestReply .stamp") %>%
    rvest::html_text()
  for (i in seq_along(latest_entry)) {
    if (stringr::str_detect(latest_entry[[i]], "Idag") == TRUE) latest_entry[[i]] <- as.character(lubridate::today())
    if (is.na(latest_entry[[i]]) & i > 1) latest_entry[[i]] <- latest_entry[[i-1]]
    if (is.na(latest_entry[[i]]) & i == 1) latest_entry[[i]] <- latest_entry[[i+1]]
    if (stringr::str_detect(latest_entry[[i]], "Obesvarat") == TRUE) latest_entry[[i]] <- NA_character_
  }
  return(lubridate::ymd(latest_entry))
}
