library(dplyr)
library(readr)
library(rvest)

expand <- 
  list.files(path = '../raw_data/', pattern = "\\.csv$", full.names=TRUE) |> 
  purrr::map_df(~read_csv(.)) |>
  mutate(repertory = strsplit(repertory, split=', ', fixed=TRUE)) |>
  tidyr::unnest(c(repertory)) |>
  mutate(repertory = coalesce(repertory, snakecase::to_sentence_case(`program name`)))

url_ext <- 
  expand |>
  distinct(repertory) |>
  mutate(
    repertory = gsub("[.()’]", "",  repertory),
    repertory = stringr::str_replace_all(repertory, " |/", "-")) |>
  pull()

cols <- c("rep", "description_short","description", "choreography", "length", "music", "music by", "premiere", "original cast", "num_dancers", "costumes", "set", "lighting", "photo_url")
df <- cols %>% purrr::map_dfc(setNames, object = list(character()))
df$num_dancers <- integer()

fill_empty <- function(chr) {
  ifelse(length(chr)==0, '-', chr)
}
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

for (rep in url_ext) {
  print(rep)
  
  # url exceptions
  if (rep == 'Errante') rep <- 'errante-formerly-titled-tzigane'
  if (rep == 'Dig-the-Say') rep <- 'new-j-peck'
  if (rep == 'Bitter-Earth') rep <- 'this-bitter-earth'
  if (rep == 'Scenes-de-Ballet') rep <- 'scenes-de-ballet-wheeldon'
  if (rep == 'The-Concert') rep <- 'the-concert-or-the-perils-of-everybody'
  
  page <- read_html(paste0("https://www.nycballet.com/discover/ballet-repertory/", tolower(rep)))
  
  # description + run time
  desc_time <-
    page |> 
    html_nodes("div.well")  |>
    html_elements("p") |>
    html_text()
  
  desc <- desc_time[1:length(desc_time)-1]
  length <- desc_time[length(desc_time)]
  if (nchar(length)>15) {length <- page |> html_nodes("p.timer") |> html_text()}
  
  
  music_choreo <- 
    page |>
    html_nodes("div.ballet-header__list") |>
    html_nodes("div.ballet-header__detail") |>
    html_text2() 
  
  music_by <- tryCatch(strsplit(music_choreo[stringr::str_starts(music_choreo, 'Music')], '\n')[[1]][2], error = function(e) music_by <- '')
  choreo <- strsplit(music_choreo[stringr::str_starts(music_choreo, 'Choreography')], '\n')[[1]][2]
  
  # photo
  url_chunk <- page %>% html_node('picture.media.media-image') %>% html_node('source') %>% as.character()
  
  # premiere, original cast, costumes, set, lighting
  titles <- 
    page |> 
    html_node("div.aside__body.s-prose") |>
    html_elements("strong") |>
    html_text()
  titles <- purrr::map(titles, ~ purrr::discard(.x, nchar(.x) == 0)) |> unlist()
  
  text <- page |> 
    html_node("div.aside__body.s-prose") |>
    html_elements("p") |>
    html_text()
  
  extract_text <- strsplit(paste0(text, collapse=' '), paste0('^', paste0(titles, collapse = '|')))
  
  clean_text <- 
    purrr::map(extract_text, stringr::str_trim) %>%
    purrr::map(., ~ purrr::discard(.x, nchar(.x) == 0)) %>%
    .[[1]]
  
  og_cast <- tryCatch(clean_text[stringr::str_starts(titles, 'Original')], error = function(e) og_cast <- '')
  num_dancers <- tryCatch(length(strsplit(og_cast, ',|;')[[1]]), error = function(e) num_dancers <- NA)
  
  df <- 
    df %>%  
    add_row(tibble::tibble_row(
      rep = rep,
      description_short = desc[1],
      description = paste(desc, collapse = '\n'),
      choreography = choreo,
      length = fill_empty(length),
      music = fill_empty(clean_text[titles=='Music'][length(clean_text[titles=='Music'])]),
      `music by` = music_by,
      premiere = clean_text[titles=='Premiere'],
      `original cast` = fill_empty(og_cast),
      num_dancers = fill_empty(num_dancers),
      costumes = fill_empty(clean_text[titles=='Costumes']),
      set = fill_empty(clean_text[titles=='Set']),
      lighting = fill_empty(clean_text[titles=='Lighting']),
      photo_url = stringr::str_extract(url_chunk, url_pattern)
    ))
  
}


df$premiere_date <- purrr::map_chr(df$premiere, ~stringr::str_extract(.x, "(.*?)[0-9]{4}")) 
df$premiere_place <-purrr::map_chr(df$premiere, ~substring(sub("(.*?)[0-9]{4}", "", .x), first=3)) 

## need to save and figure out where list is
write.csv(df, file='rep_detail.csv' ,row.names = F)

#page-content > header > div > figure > picture > source:nth-child(1)