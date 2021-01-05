##########==========##########==========##########==========##########==========

warning("TODO: ensure that colonies inherit hdi of parent country (script #3)")
warning("TODO: create fallback for the pre-screen (script #4)")


## SET UP ==========

## meta-information
## Author: Joshua Mendelsohn
## Creation: 2021-01-05
## Version: 4.0.3
## Description: Generate a static map pdf with 'standard' recommendations
   ## (dynamic recommendation tool in script #5)

## environment set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6)
library(lintr) # lint("4_map_standard_suggestion.R")
library(tidyverse)

## READ IN DATA ==========
load("B_Intermediates/processed_data.RData")

## CALCULATE REGIONAL AVERAGES AND PRE-SCREEN COUNTRIES ACCCORDINGLY ==========

above_median <- select(country_data, region, country_key)
notable_cities <- unnest(country_data, cols = notable_cities) %>%
  select(region, country_key, city, capitol, population, heritage_sites,
    lon, lat)

## determine regional median human development index and state depart advice
country_level <-  country_data %>%
  group_by(region) %>%
  summarize(dos_level = median(dos_level), hdi = median(hdi))
above_median <- left_join(above_median, country_level, by = "region")
remove(country_level)

## determine regional max/mean for population/heritage sites in notable cities
city_level_country <- notable_cities %>%
  group_by(country_key) %>%
  summarize(
    population = max(population),
    heritage_sites = mean(heritage_sites)
    )
country_data <- bind_cols(country_data,
  select(city_level_country, population, heritage_sites))

city_level <- left_join(
  select(notable_cities, region, country_key),
  city_level_country, by = "country_key") %>%
  group_by(region) %>%
  summarize(
    "population" = median(population),
    "heritage_sites" = median(heritage_sites)
    )
above_median <- left_join(above_median, city_level, by = "region")

remove(city_level, city_level_country, notable_cities)

## determine which countries are above the regional average on all measures
above_median <- country_data %>%
  mutate(
    dos_level = dos_level <= pull(above_median, dos_level),
    hdi = hdi >= pull(above_median, hdi),
    population = population >= pull(above_median, population),
    heritage_sites = heritage_sites >= pull(above_median, heritage_sites),
    ) %>%
  select(region, country_key, hdi, dos_level, population, heritage_sites) %>%
  mutate("qualified" = hdi & dos_level & population & heritage_sites)

## confirm that at least one country from each region passes pre-screening
quality_check <- above_median %>%
  group_by(region) %>%
  summarize("qualifiers" = sum(qualified)) %>%
  mutate("qualified" = qualifiers == 0)
if (any(pull(quality_check, qualified))) {
  warning("Pre-screening eliminated all candidates for > 0 regions")
  print(filter(quality_check, qualified))
} else {
    remove(quality_check)
  }
above_median <- filter(above_median, qualified)

## DECIDE AMONG FINALISTS ==========

## drop country_data for non-finalists
finalists <- filter(country_data,
  country_key %in% pull(above_median, country_key))
remove(above_median)

## rank by hdi bracket, then population
finalists <- finalists %>%
  mutate(hdi_bracket = floor(hdi / 0.1) * 0.1) %>%
  arrange(1 - hdi_bracket, 1 - population) %>%
  filter(!duplicated(region))

finalists
## PLOT WORLD MAP ==========

## PLOT SUGGESTED COUNTRIES FOR EACH REGION ==========

## PLOT SUGGESTED CITIES FOR EACH COUNTRY ==========

##########==========##########==========##########==========##########==========
