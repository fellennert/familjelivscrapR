### auxiliary functions

# scraping thread links

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


### scrape threads' content

# get thread's n pages

get_n_pages_thread <- function(thread_link) {
  xml2::read_html(thread_link) %>%
    rvest::html_node("#formupdate .selected a") %>%
    rvest::html_text()
}

# create list of singular thread-pages
build_links_for_threads <- function(thread_link, n_pages) {
  n <- 1:n_pages
  temp_link <- stringr::str_sub(thread_link, end=-6)
  links <- character(length = length(n))
  for (i in seq_along(n)) {
    links[i] <- paste0(temp_link, "-", n[[i]], ".html")
  }
  return(links)
}


# get content
# date and time
get_date_time <- function(thread_page, url) {
  today <- as.character(lubridate::today())
  yesterday <- as.character(lubridate::today()-1)
  day_before_yesterday <- as.character(lubridate::today()-2)
  months_tbl <- tibble::tibble(
    months_chr = c("jan", "feb", "mar", "apr", "maj", "jun",
                  "jul", "aug", "sep", "okt", "nov", "dec", "xyz"),
    months_num = c(1:12, 0)
  )
  month_pattern <- paste(months_tbl$months_chr, collapse = "|")

  date <- rvest::html_nodes(thread_page, ".date") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed("×", 2)

  date <- date[, 1]

  top_date <- rvest::html_nodes(thread_page, ".forum-top-date") %>%
    rvest::html_text()

  if (stringr::str_detect(url, "-1.html$")) {
    date <- c(top_date, date)
  }

  date_tbl <- tibble::enframe(date) %>%
    dplyr::select(value) %>%
    dplyr::mutate(V1 = paste0(value, "#0"))%>%
    dplyr::mutate(time = stringr::str_extract(V1, "([0-2][0-9][:][0-5][0-9])"),
           post_number = purrr::map_chr(stringr::str_split(V1, "#"), 2),
           date = purrr::map_chr(stringr::str_split(V1, "([0-2][0-9][:][0-5][0-9])"), 1),
           date_numeric = dplyr::case_when(stringr::str_detect(date, "Idag") ~ today,
                                    stringr::str_detect(date, "Igår") ~ yesterday,
                                    stringr::str_detect(date, "I förrgår") ~ day_before_yesterday),
           date_day = stringr::str_extract(date, "([0-3][0-9])|([0-9])"),
           date_year = dplyr::if_else(stringr::str_detect(date, "([2][0][0-2][0-9])"),
                               stringr::str_extract(date, "([2][0][0-2][0-9])"),
                               "2020"),
           date_numeric = lubridate::ymd(date_numeric),
           date_month_temp = dplyr::if_else(is.na(date_numeric) == TRUE,
                                     stringr::str_extract(date, month_pattern),
                                     "0")) %>%
    dplyr::left_join(months_tbl, by = c("date_month_temp" = "months_chr")) %>%
    dplyr::mutate(date_month = dplyr::if_else(date_month_temp == 0,
                                lubridate::month(date_numeric),
                                months_num),
           date_day = dplyr::if_else(is.na(date_day) == TRUE,
                              lubridate::day(date_numeric),
                              as.integer(date_day)),
           date = paste(date_year, date_month, date_day, sep = "-")) %>%
    dplyr::select(date, time) %>%
    dplyr::filter(!is.na(date) & !is.na(time)) %>%
    dplyr::mutate(date = lubridate::ymd(date))

  return(date_tbl)
}


# author's name

get_author <- function(thread_page, url) {
  author <- rvest::html_nodes(thread_page, ".compose_avatar_nick") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_trim()
  author <- author[author != ""]

  if (stringr::str_detect(url, "-1.html$") == FALSE) {
    author <- author[-1]
  }
  return(author)
}

# content

get_textual_content <- function(thread_page, url, length) {
  text <- rvest::html_nodes(thread_page, ".message") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_squish()

  if (stringr::str_detect(url, "-1.html$") == FALSE) {
    text <- text[-1]
  }
  if (length(text) != length) {
    text <- text[1:length]
  }
  return(text)
}

# output list

get_output <- function(thread_link) {
  thread_page <- xml2::read_html(thread_link)
  date <- get_date_time(thread_page = thread_page, url = thread_link) %>% purrr::pluck(1)
  tibble::tibble(
      thread = thread_link,
      date = date,
      time = get_date_time(thread_page = thread_page, url = thread_link) %>% purrr::pluck(2),
      author = get_author(thread_page = thread_page, url = thread_link),
      content = get_textual_content(thread_page = thread_page, url = thread_link, length = length(date))
    ) %>%
    dplyr::mutate(quote_ind = dplyr::if_else(stringr::str_detect(content, "skrev.....................följande"),
                                             1,
                                             0),
                  author = dplyr::if_else(stringr::str_detect(author, "Anonym \\("),
                                          NA_character_,
                                          author))
}

### remove quotes

# get quotes

get_quotes <- function (thread_page) {
  rvest::html_nodes(thread_page, ".quote") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_remove_all("\\t") %>%
    stringr::str_squish()
}

# remove'em
remove_quotes <- function(quotes, output_tbl) {

  quotes_new <- purrr::map_chr(quotes, ~{
    stringr::str_sub(.x, start = -(2/3*stringr::str_length(.x)))
  })

  output_w_quote <- output_tbl %>%
    dplyr::filter(quote_ind == 1)
  output_wo_quote <- output_tbl %>%
    dplyr::filter(quote_ind == 0)
  pattern <- paste(quotes_new, collapse = '|')
  output_w_quote$content_wo_quote <- character(length = nrow(output_w_quote))

  if (nrow(output_w_quote) > 0) {
    for (i in 1:nrow(output_w_quote)) {
      if (length(stringr::str_split(output_w_quote$content[i], pattern = fixed(pattern), n = 2))[[1]] == 2) {
        output_w_quote$content_wo_quote[i] <- stringr::str_split(output_w_quote$content[i], pattern = fixed(pattern), n = 2)[[1]][[2]]
      } else {
        output_w_quote$content_wo_quote[i] <- paste0("!!!flawed citation!!!", output_w_quote$content[i])
      }
    }
  }

  output_wo_quote$content_wo_quote <- output_wo_quote$content
  output_tbl <- dplyr::bind_rows(output_w_quote, output_wo_quote) %>%
    dplyr::distinct(content_wo_quote, .keep_all = TRUE) %>%
    dplyr::arrange(date, time)
  return(output_tbl)
}
