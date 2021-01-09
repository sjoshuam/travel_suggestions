##########==========##########==========##########==========##########==========

## SET-UP ==========

## Meta-information
## Author: Joshua Mendelsohn
## Creation: 2020-01-03
## Version: 4.0.3
## Description: retrieve current travel advisories from state department website

## environment set-up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6)
library(jsonlite)
library(rvest)
library(tidyverse)

## QUERY STATE DEPT RSS FEED FOR ADVISORIES ==========

## read in RSS feed
dos_rss <- read_html(url("https://travel.state.gov/_res/rss/TAsTWs.xml"))
factbook_json <- fromJSON(url(paste0(
  "https://raw.githubusercontent.com/",
  "iancoleman/cia_world_factbook_api/master/data/factbook.json")
  ))

## PARSE ADVISORIES ==========

## extract level/country data
dos_level <- dos_rss %>%
  html_nodes("item") %>%
  html_nodes("title") %>%
  html_text()

## parse country and level
dos_level <- dos_level %>%
  str_split(pattern = " - ") %>%
  simplify2array() %>%
  t()

## convert to tibble format
colnames(dos_level) <- c("country", "level")
dos_level <- as_tibble(dos_level) %>%
  mutate("level_num" =  str_replace_all(level, "[^0-9]", ""))

dos_level$level_num <- as.integer(dos_level$level_num)

## PARSE JUSTIFICATIONS ==========

## extract justification (for level) data
dos_reason <- dos_rss %>%
  html_nodes("item") %>%
  html_nodes("description") %>%
  html_text() %>%
  str_replace("due\\sto\\s", ". ") %>%
  str_split("\\.\\s") %>%
  map_chr(nth, 2) %>%
  str_replace("\\s", " ") %>%
  str_replace("(^\\s+)|(\\s+$)", "") %>%
  enframe(value = "reasons")

## incorporate into primary data object
dos_advice <- bind_cols(dos_level, select(dos_reason, reasons)) %>%
  mutate("reasons" = if_else(level_num == 1, "(none)", reasons)) %>%
 mutate("covid_adj" = if_else(reasons == "COVID-19", level_num - 1L, level_num))
remove(dos_reason, dos_level)

## PATCH DATA IDEOSYNCRACIES ==========
dos_advice <- dos_advice %>%
  add_row(country = "USA", level_num = 1, covid_adj = 1, reasons = "(none)") %>%
  mutate(country = str_replace(country, "^Israel.*", "Israel"))

## DOWNLOAD JSON-SCRAPE OF THE CIA WORLD FACTBOOK ==========

## extract language data from json object
extract_language <- function(x) {
  x <- x$data$people$languages$language
  if (class(x) != "data.frame") {x <- data.frame(
    "name" = NA, "percent" = NA, "note" = NA)}
  if (!("note" %in% colnames(x))) {x$note <- as.character(NA)}
  if (!("percent" %in% colnames(x))) {x$percent <- as.character(NA)}
  x[, c("name", "percent", "note")]
  }
factbook_json <- factbook_json$countries
factbook_json <- lapply(factbook_json, extract_language)
factbook_json <- data.frame(
  "country" = rep(names(factbook_json), sapply(factbook_json, nrow)),
  do.call(what = rbind, args = factbook_json)
  )
factbook_json <- as_tibble(factbook_json)

## repair missing data
language_missing <- tibble(
  "official" = str_detect(factbook_json$note, "official"),
  "auxiliary" = str_detect(factbook_json$note, "lingua franca"),
  ) %>%
  mutate("official"  = if_else(is.na(official), FALSE, official)) %>%
  mutate("auxiliary" = if_else(is.na(auxiliary), FALSE, auxiliary)) %>%
  mutate("repair" = as.numeric(official | auxiliary)) %>%
  mutate("country" = pull(factbook_json, country)) %>%
  mutate("denominator" = tapply(repair, country, sum)[country]) %>%
  mutate("denominator" = pmax(denominator, 1)) %>%
  mutate("repair" = (repair / denominator) * 100)

factbook_json <- factbook_json %>%
  mutate("repair" = language_missing$repair) %>%
  mutate("percent" = if_else(is.na(percent), repair, as.numeric(percent))) %>%
  select(-repair) %>%
  mutate("percent" = round(percent / 100, 2)) %>%
  filter(!(country %in% c("world", "european_union"))) %>%
  mutate("country" = str_replace_all(country, "_", " "))

remove(language_missing)

## EXPORT FINAL DATASET ==========

common_languages <- factbook_json

saveRDS(dos_advice, file = "B_Intermediates/dos_advice.RData")
saveRDS(common_languages, file = "B_Intermediates/common_languages.RData")

##########==========##########==========##########==========##########==========
