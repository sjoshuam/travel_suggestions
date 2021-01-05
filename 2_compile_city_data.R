##########==========##########==========##########==========##########==========

## SET-UP ==========

## meta-information
## author: Joshua Mendelsohn
## creation: 2021-01-02
## R version: 4.0.3
## purpose: generate geo-coded dataset of world cities, combining world
   ## heritage site and population information.

## environment set-up
remove(list = objects())
options(load_cached_geocode = TRUE, width = 80, digits = 6)
library(sp)
library(readxl)
library(ggmap)
library(tidyverse)

## load Google API key
source("~/Documents/Coding/api_keys/GoogleAPI.R")

## READ IN DATA ==========

city_data <- read_xlsx("A_Inputs/city_population.xlsx",
  sheet = 1, skip = 7, col_names = FALSE)
heritage_sites <- read_xlsx("A_Inputs/world_heritage.xlsx")

## GENERATE PRIMARY KEYS ==========
heritage_sites <- mutate(heritage_sites,
  prim_key = paste0("site_", pull(heritage_sites, unique_number))
  )
city_data <- mutate(city_data,
  prim_key = paste0("city_", seq(nrow(city_data)))
  )

## SHAPE CITY DATASET ==========

## filter to relevant columns and rename
city_data <- city_data %>%
  rename("city" = ...1, "population" = ...10, 'proper' = ...2,
    "continent" = ...18) %>%
  select(city, population, proper, continent, prim_key)

## fill in missing metro population statistics with city proper statistics
city_data$population <- as.numeric(city_data$population)
city_data$proper <- as.numeric(city_data$proper)
city_data <- city_data %>%
  mutate(population = if_else(is.na(population), proper, population)) %>%
  select(-proper)

## filter non-city rows and note capitols
city_data <- city_data %>%
  replace_na(list("continent" = FALSE)) %>%
  mutate("source" = str_detect(city, "[0-9]{4,4}[*]* [(][A-Z]{4,4}[)]")) %>%
  filter(!continent, !source) %>%
  select(-continent, -source) %>%
  mutate("capitol" = if_else(city == toupper(city), TRUE, FALSE))

## refine country names and generate full place names
city_data <- city_data %>%
  mutate(city = str_replace(city, '[0-9]+$', '')) %>%
  mutate(country = if_else(is.na(population), city, as.character(NA))) %>%
  mutate("city" = str_replace(city, " [(][^)]+[)]", "")) %>%
  fill(country, .direction = "down") %>%
  mutate(country = str_replace(country, " - .+", "")) %>%
  filter(!is.na(population)) %>%
  mutate("name" = paste(city, country, sep = ", "))

## GEOCODE CITIES ==========

## use Google API to generate coordinates for cities
if (!options()$load_cached_geocode) {
  geocode_cache <- city_data %>% pull(name) %>% geocode(output = "more")
  saveRDS(geocode_cache, file = "B_Intermediates/geocode_cache.RData")
  remove(geocode_cache)
}

## quality check API data
city_geocodes <- readRDS("B_Intermediates/geocode_cache.RData")
city_geocodes <- city_geocodes %>%
  bind_cols(select(city_data, population)) %>%
  mutate(valid = population >= 10^6 | type == "locality") %>%
  select(address, lon, lat, valid)

city_data <- bind_cols(city_data, city_geocodes) %>%
  filter(valid) %>%
  select(-valid) %>%
  drop_na(lon, lat)
remove(city_geocodes)

## FIND HERITAGE SITES NEAR EACH CITY ==========

## do basic cleaning
heritage_sites <- heritage_sites %>%
  mutate("country" = udnp_code) %>%
  select(name_en, category, longitude, latitude, country, prim_key) %>%
  drop_na(longitude, latitude)

## calculate distances
nearby_sites <- spDists(
  as.matrix(select(city_data, lon, lat)),
  as.matrix(select(heritage_sites, longitude, latitude)),
  longlat = TRUE
  )

## find world heritage sites near each city
nearby_sites <- apply(nearby_sites <= 50, 1, which) %>%
  lapply(function(x) {heritage_sites[x, "prim_key"]}) %>%
  enframe(value = "heritage_key") %>%
  select(heritage_key) %>%
  mutate(heritage_sites = sapply(heritage_key, nrow))

## incorporate data into city_data
city_data <- bind_cols(city_data, nearby_sites)
remove(nearby_sites)

## EXPORT REFINED DATASETS ==========
saveRDS(city_data, file = "B_Intermediates/city_data.RData")
saveRDS(heritage_sites, file = "B_Intermediates/heritage_sites.RData")

##########==========##########==========##########==========##########==========
