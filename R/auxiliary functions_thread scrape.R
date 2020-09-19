# auxiliary functions -- scraping threads' content ------------------------

# scrape_pages
get_pages <- function(thread_link){
  n_pages <- xml2::read_html(thread_link) %>%
    rvest::html_node("#formupdate .selected a") %>%
    rvest::html_text()
  if (is.na(n_pages)) n_pages <- 1
  links <- character(length = n_pages)
  for (i in seq_along(links)){
    links[[i]] <- paste0(stringr::str_sub(thread_link, end = -6), "-", i, ".html")
  }
  return(purrr::map(links, ~{
    tryCatch(
      xml2::read_html(.x),
      error = function(e){
        tryCatch(
          xml2::read_html(.x, options = "HUGE"),
          error = function(e) return("flawed"))
      })
    }
  ))
}

# date and time
# date helper functions
extract_day <- function(date){
  day <- date %>%
    stringr::str_sub(end = 2L) %>%
    stringr::str_remove(" ")
  if (stringr::str_length(day) == 1) day <- paste0("0", day)
  return(day)
}

extract_month <- function(date){
  months_tbl <- tibble::tibble(
    months_chr = c("jan", "feb", "mar", "apr", "maj", "jun",
                  "jul", "aug", "sep", "okt", "nov", "dec"),
    months_num = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  )
  date %>%
    stringr::str_extract_all(pattern = paste(months_tbl$months_chr, collapse = "|")) %>%
    as.character() %>%
    tibble::enframe(name = NULL, value = "months_chr") %>%
    dplyr::left_join(months_tbl, by = "months_chr") %>%
    dplyr::pull(2)
}

extract_year <- function(date){
  raw_year <- date %>%
    stringr::str_sub(start = 3L)
  if (stringr::str_detect(raw_year, "[:digit:]") == TRUE) {
    return(stringr::str_extract_all(raw_year, "[:digit:]"))
  }else{
    return(lubridate::today() %>%
             lubridate::year() %>%
             as.character())
  }
}

# get dates
get_top_date <- function(thread_page){
  date <- rvest::html_nodes(thread_page, ".forum-top-date") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("Mån|Tis|Ons|Tors|Fre|Lör|Sön") %>%
    stringr::str_sub(end = -7L) %>%
    stringr::str_squish()

  return(paste(extract_year(date),
               extract_month(date),
               extract_day(date),
               collapse = " ")  %>%
    stringr::str_extract_all("[:digit:]") %>%
    purrr::map_chr(~{
      year <- paste0(.x[1], .x[2], .x[3], .x[4], collapse = "")
      month <- paste0(.x[5], .x[6], collapse = "")
      day <- paste0(.x[7], .x[8], collapse = "")
      paste(year, month, day, sep = "-")
    }) %>%
    lubridate::ymd())
}

get_date <- function(thread_page){
  rvest::html_nodes(thread_page, ".date") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n|\t") %>%
    stringr::str_remove_all("Mån|Tis|Ons|Tors|Fre|Lör|Sön") %>%
    stringr::str_squish() %>%
    stringr::str_sub(end = -7L) %>%
    purrr::map_chr(~{
      paste(extract_year(.x),
            extract_month(.x),
            extract_day(.x),
            collapse = " ")
    }) %>%
    stringr::str_extract_all("[:digit:]") %>%
    purrr::map_chr(~{
      year <- paste0(.x[1], .x[2], .x[3], .x[4], collapse = "")
      month <- paste0(.x[5], .x[6], collapse = "")
      day <- paste0(.x[7], .x[8], collapse = "")
      paste(year, month, day, sep = "-")
    }) %>%
    lubridate::ymd()
}

# get times
get_top_time <- function(thread_page){
  rvest::html_nodes(thread_page, ".forum-top-date") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_trim() %>%
    stringr::str_extract(pattern = "[0-2][0-9][:][0-5][0-9]") %>%
    hms::parse_hm()
}

get_time <- function(thread_page){
  rvest::html_nodes(thread_page, ".date") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_trim() %>%
    stringr::str_extract(pattern = "[0-2][0-9][:][0-5][0-9]") %>%
    hms::parse_hm()
}

# author's name
get_top_author <- function(thread_page) {
  rvest::html_nodes(thread_page, ".entry-bottom .compose_avatar_nick") %>%
    rvest::html_text()
}

get_author <- function(thread_page) {
  rvest::html_nodes(thread_page, ".reply .compose_avatar_nick") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_trim()
}

# content

get_top_content <- function(thread_page){
  rvest::html_nodes(thread_page, ".entry-bottom .message") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()
}

get_textual_content <- function(thread_page) {
  text <- rvest::html_nodes(thread_page, ".reply .message") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\n") %>%
    stringr::str_remove_all("\t") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^[:alnum:]]", " ") %>%
    stringr::str_squish()

  if (length(text) > 10) return(unique(text))

  return(text)
}

# quotes

# quoted users

get_quoted_user <- function(posting){
  user <- stringr::str_split_fixed(posting, "^* skrev [2][0][0-2][0-9]", 2) %>% purrr::compact()
  if (user[[2]] == "") return(NA_character_)
  user[[1]]
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

  if (quotes == "") return(content)

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

## summarizing functions

build_top_post <- function(thread_link){
  page <- xml2::read_html(thread_link)
  tibble::tibble(
    url = thread_link,
    date = get_top_date(page),
    time = get_top_time(page),
    author_name = get_top_author(page),
    quoted_user = NA_character_,
    posting = get_top_content(page),
    posting_wo_quote = get_top_content(page)
  )
}

build_output_tibble <- function(thread_page, thread_link){
  tryCatch(
    tibble::tibble(
      url = thread_link,
      date = get_date(thread_page),
      time = get_time(thread_page),
      author_name = get_author(thread_page),
      quoted_user = NA_character_,
      posting = get_textual_content(thread_page),
      posting_wo_quote = remove_quotes(content = get_textual_content(thread_page), thread_page = thread_page)
  ),
  error = function(e){
    tibble::tibble(
      url = thread_link,
      date = lubridate::ymd("1970-01-01"),
      time = hms::parse_hm("00:00"),
      author_name = NA_character_,
      quoted_user = NA_character_,
      posting = "broken thread page, approximately 10 postings are missing",
      posting_wo_quote = NA_character_
    )
  }
  )
}

