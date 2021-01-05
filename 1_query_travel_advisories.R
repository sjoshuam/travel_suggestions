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
library(rvest)
library(tidyverse)


## QUERY STATE DEPT RSS FEED FOR ADVISORIES ==========

## read in RSS feed
dos_rss <- read_html("https://travel.state.gov/_res/rss/TAsTWs.xml") 

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
  enframe(value= "reasons")

## incorporate into primary data object
dos_advice <- bind_cols(dos_level, select(dos_reason, reasons)) %>%
  mutate("reasons" = if_else(level_num == 1, "(none)", reasons)) %>%
  mutate("covid_adj"= if_else(reasons == "COVID-19", level_num - 1L, level_num))
remove(dos_reason, dos_level)

## PATCH DATA IDEOSYNCRACIES ==========
dos_advice <- dos_advice %>%
  add_row(country = "USA", level_num = 1, covid_adj = 1, reasons = "(none)") %>%
  mutate(country = str_replace(country, "^Israel.*", "Israel"))
  
## EXPORT FINAL DATASET ==========
saveRDS(dos_advice, file= "B_Intermediates/dos_advice.RData")


##########==========##########==========##########==========##########==========
