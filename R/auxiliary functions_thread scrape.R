# auxiliary functions -- scraping threads' content ------------------------

# scrape_pages
get_pages <- function(thread_link){
  n_pages <- xml2::read_html(thread_link) %>%
    rvest::html_node("#formupdate .selected a") %>%
    rvest::html_text()
  links <- character(length = n_pages)
  for (i in seq_along(links)){
    links[[i]] <- paste0(stringr::str_sub(thread_link, end = -6), "-", i, ".html")
  }
  return(list(pages = purrr::map(links, xml2::read_html), url = links))
}

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

  date <- rvest::html_nodes(thread_page, ".entry-info .date") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed("×", 2)

  date <- date[, 1]

  reply_number <- rvest::html_nodes(thread_page, ".reply-number") %>%
    rvest::html_text() %>%
    readr::parse_number()

  reference <- reply_number[[1]]

  if (length(reply_number) > length(date)) reply_number <- reply_number[-order(reply_number)[1]]
  top_date <- rvest::html_nodes(thread_page, ".forum-top-date") %>%
    rvest::html_text()

  if (stringr::str_detect(url, "-1.html$")) {
    date <- c(top_date, date)
    reply_number <- c(1, reply_number)
  }

  date_tbl <- tibble::tibble(date = date, reply_number = reply_number) %>%
    dplyr::mutate(date = paste0(date, "#0"))%>%
    dplyr::mutate(time = stringr::str_extract(date, "([0-2][0-9][:][0-5][0-9])"),
           post_number = purrr::map_chr(stringr::str_split(date, "#"), 2),
           date = purrr::map_chr(stringr::str_split(date, "([0-2][0-9][:][0-5][0-9])"), 1),
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
    dplyr::select(date, time, reply_number) %>%
    dplyr::filter(!is.na(date) & !is.na(time))

  if (stringr::str_detect(url, "-1.html$") == FALSE) {
    date_tbl <- dplyr::filter(date_tbl, reply_number >= reference)
  }

  return(date_tbl)
}

# author's name
get_author <- function(thread_page, url) {
  author <- rvest::html_nodes(thread_page, ".compose_avatar_image .compose_avatar_nick") %>%
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
  content <- rvest::html_nodes(thread_page, ".message") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()

  if (stringr::str_detect(url, "-1.html$") == FALSE) {
    content <- content[-1]
  }
  if (length(content) != length) {
    content <- content[1:length]
  }
  return(content)
}

get_quoted_user <- function(thread_page, url, length){
  text <- rvest::html_nodes(thread_page, ".message") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t")
  if (stringr::str_detect(url, "-1.html$") == FALSE) {
    content <- content[-1]
  }
  if (length(text) != length) {
    text <- text[1:length]
  }
  temp <- stringr::str_split(text, "^* skrev [2][0][0-2][0-9]", 2)
  output <- character(length = length(temp))
  for (i in seq_along(temp)) {
    if (length(temp[[i]]) == 1) output[[i]] <- NA
    if (length(temp[[i]]) == 2) output[[i]] <- temp[[i]][[1]]
  }
  output
  }

# remove quotes

remove_quotes <- function(content, thread_page){
  quotes <- rvest::html_nodes(thread_page, ".quote") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_c(collapse = "|")

  for_extract <- stringr::str_locate_all(content, quotes)
  for_removal <- purrr::map2(content, for_extract, ~{
    stringr::str_sub(.x, start = .y[,1], end = .y[, 2])
  })
  purrr::map2(content, for_removal, ~{
    if (length(.y) == 0) {
    return(.x)
    } else {
    stringr::str_remove_all(.x, pattern = stringr::str_c(.y, collapse = "|"))
    }
  }
  ) %>%
  purrr::reduce(c) %>%
    stringr::str_squish()
}

# save output

save_it <- function(folder_name, file_name, output_tbl) {

  if (is.null(file_name) == TRUE) {
    file_name <- paste0("scrape-", as.character(lubridate::today()))
  }
  readr::write_csv(output_tbl, file.path(folder_name, paste0(file_name, ".csv")))
}
