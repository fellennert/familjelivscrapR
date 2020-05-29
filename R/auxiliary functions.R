### auxiliary functions

# scraping thread links

# get n_pages

get_n_pages <- function(page) {
  n_threads <- rvest::html_nodes(page, ".threads stringr::strong") %>%
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
  start_date <- rvest::html_nodes(page, ".forumListing-threadStarted .stamp") %>%
    rvest::html_text()
  for (i in seq_along(start_date)) {
    if (stringr::str_detect(start_date[[i]], "Idag") == TRUE) start_date[[i]] <- as.character(lubridate::today())
  }
  return(lubridate::ymd(start_date))
}

# get threads' latest entry

get_latest_entry <- function(page) {
  latest_entry <- rvest::html_nodes(page, ".forumListing-latestreply .stamp") %>%
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

# (1) create list of singular thread-pages
build_links_for_threads <- function(thread_link, n_pages) {
  n <- 1:n_pages
  temp_link <- stringr::str_sub(thread_link, end=-6)
  links <- character(length = length(n))
  for (i in seq_along(n)) {
    links[i] <- paste0("http://gamla.familjeliv.se", temp_link, "-", n[[i]], ".html")
  }
  return(links)
}


# (3) get content
# (3.1) date
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


# (3.2) author's name

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

# (3.3) content

get_textual_content <- function(thread_page, url) {
  text <- rvest::html_nodes(thread_page, ".message") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()

  if (stringr::str_detect(url, "-1.html$") == FALSE) {
    text <- text[-1]
  }
  return(text)
}

# (3.5) quotes

get_quotes <- function (thread_page) {
  quotes <- rvest::html_nodes(thread_page, ".quote") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
  return(quotes)
}

# (4.) bind it together

#output_tbl <- bind_rows(output_tbl, temp_tbl)
#quotes <- quotes_list %>% unlist()

# (5.) remove quotes

remove_quotes <- function(quotes, output_tbl) {

  for (j in seq_along(quotes)) {
    quotes[j] <- stringr::str_sub(quotes[j], start=-(2/3*stringr::str_length(quotes[j])))
  }

  output_tbl %<>%
    dplyr::mutate(quote_bin = dplyr::if_else(stringr::str_detect(content, "skrev.....................följande"),
                               1,
                               0),
           author = dplyr::if_else(stringr::str_detect(author, "Anonym \\("),
                            NA_character_,
                            author)) %>%
    dplyr::distinct()
  output_w_quote <- output_tbl %>%
    dplyr::filter(quote_bin == 1)
  output_wo_quote <- output_tbl %>%
    dplyr::filter(quote_bin == 0)
  pattern <- paste(quotes, collapse = '|')
  output_w_quote$content_wo_quote <- character(length = nrow(output_w_quote))
  if (nrow(output_w_quote) > 0) {
    for (i in 1:nrow(output_w_quote)) {
      if (length(stringr::str_split(output_w_quote$content[i], pattern = pattern, n = 2)[[1]]) == 2) {
        output_w_quote$content_wo_quote <- stringr::str_split(output_w_quote$content[i], pattern = pattern, n = 2)[[1]][[2]]
      } else {
        output_w_quote$content_wo_quote[i] <- paste0("flawed citation", output_w_quote$content[i])
      }
    }
  }

  output_wo_quote$content_wo_quote <- output_wo_quote$content
  output_tbl <- dplyr::bind_rows(output_w_quote, output_wo_quote) %>%
    dplyr::distinct(content_wo_quote, .keep_all = TRUE) %>%
    dplyr::arrange(date, time)
  return(output_tbl)
}


### final scrape function ###

scrape_thread <- function(thread_link, n_pages) {

  url_list <- build_links_for_threads(thread_link = thread_link, n_pages = n_pages)

  output_list <- list()
  quotes_list <- list()

  for (i in seq_along(url_list)) {
    thread_page <- xml2::read_html(url_list[[i]])
    output_list[[i]] <- tibble(
      thread = thread_link,
      date = get_date(thread_page = thread_page, url = url_list[[i]]),
      time = get_time(thread_page = thread_page, url = url_list[[i]]),
      author = get_author(thread_page = thread_page, url = url_list[[i]]),
      content = get_textual_content(thread_page = thread_page, url = url_list[[i]])
    )
    quotes_list[[i]] <- get_quotes(thread_page = thread_page)
  }

  output_tbl <- bind_rows(output_list)
  quotes <- quotes_list %>% unlist()
  quotes <- quotes[quotes != ""]

  if (length(quotes) == 0) {
    return(output_tbl)
  } else {
    return(remove_quotes(quotes = quotes, output_tbl = output_tbl))
  }
}



### scrape thread for badly-behaved threads
unify_vector_length <- function(date, time, author, content) {
  list <- list(date, time, author, content)
  max <- max(lengths(list))
  for (i in seq_along(1:4)) {
    length(list[[i]]) <- max
  }
  return(list)
}

scrape_bad_thread <- function(thread_link, n_pages) {

  pb$tick()$print()

  url_list <- build_links_for_threads(thread_link = thread_link, n_pages = n_pages)

  output_list <- list()
  quotes_list <- list()

  for (i in seq_along(url_list)) {
    thread_page <- xml2::read_html(url_list[[i]])

    date <- get_date(thread_page = thread_page, url = url_list[[i]])
    time <- get_time(thread_page = thread_page, url = url_list[[i]])
    author <- get_author(thread_page = thread_page, url = url_list[[i]])
    content <- get_textual_content(thread_page = thread_page, url = url_list[[i]])

    uni_list <- unify_vector_length(date, time, author, content)

    output_list[[i]] <- tibble(
      thread = thread_link,
      date = uni_list[[1]],
      time = uni_list[[2]],
      author = uni_list[[3]],
      content = uni_list[[4]]
    )
    quotes_list[[i]] <- get_quotes(thread_page = thread_page)
  }

  output_tbl <- bind_rows(output_list)
  quotes <- quotes_list %>% unlist()

  if (length(quotes) == 0) {
    return(output_tbl)
  } else {
    return(remove_quotes(quotes = quotes, output_tbl = output_tbl))
  }
}
