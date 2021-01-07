##########==========##########==========##########==========##########==========

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

## CREATE PARSIMONIOUS STRUCTURED REGION LABELS ==========
region_labels <- tribble(
  ~region, ~region_short,
  "Southern Asia",             "Asia South",
  "Northern Europe",           "Europe North",
  "Southern Europe",           "Europe South",
  "Northern Africa",           "Africa North",
  "Polynesia",                 "Oceania Polynesia",
  "Middle Africa",             "Africa Middle",
  "Caribbean",                 "America Caribbean",
  "South America",             "America South",
  "Western Asia",              "Asia West",
  "Australia and New Zealand", "Oceania AUS/NZL",
  "Western Europe",            "Europe West",
  "Eastern Europe",            "Europe East",
  "Central America",           "America Central",
  "Western Africa",            "Africa West",
  "Northern America",          "America North",
  "Southern Africa",           "Africa South",
  "South-eastern Asia",        "Asia Southeast",
  "Eastern Africa",            "Africa East",
  "Eastern Asia",              "Asia East",
  "Micronesia",                "Oceania Micronesia",
  "Melanesia",                 "Oceania Melanesia",
  "Central Asia",              "Asia Central",
  )
country_data <- country_data %>%
  left_join(region_labels, by = "region") %>%
  mutate(region_name = region, region = region_short) %>%
  select(-region_short)
remove(region_labels)


## CALCULATE REGIONAL MEDIANS AND SCORE COUNTRIES AGAINST REGION ==========

## score region norms for city_level indicators
city_level <- country_data %>%
  unnest(notable_cities) %>%
  select(city, heritage_sites, population, country_key, region) %>%
  group_by(country_key) %>%
  summarize(
    region = unique(region),
    population = max(population, na.rm = TRUE),
    heritage_sites = sum(heritage_sites)
    )

country_data <- left_join(country_data,
  select(city_level, country_key, population, heritage_sites),
  by = "country_key"
  )
remove(city_level)

## temporally modify key variables to facilitate scoring
country_data <- country_data %>%
  mutate(
    dos_level = 5 - dos_level,
    population = log2(population)
    )

## score region norms for country-level indicators
regional_norms <- country_data %>%
  group_by(region) %>%
  summarize(
    region = unique(region),
    hdi_score = median(hdi),
    hdi_var = mad(hdi),
    dos_score = median(dos_level),
    dos_var = max(mad(dos_level), 0.5),
    population_score = median(population),
    population_var = mad(population),
    heritage_score = median(heritage_sites),
    heritage_var = max(mad(heritage_sites), 0.5)
    )

country_data <- left_join(country_data, regional_norms, by = "region")
remove(regional_norms)

## score each country against regional norms
truncate_score <- function(x) {
  pmin(pmax(x, -2), 2) / 2
  }
country_data <- country_data %>%
  mutate(
    hdi_score = truncate_score((hdi - hdi_score) / hdi_var),
    dos_score = truncate_score((dos_level - dos_score) / dos_var),
  population_score = truncate_score(
    (population - population_score) / population_var),
    heritage_score = truncate_score(
      (heritage_sites - heritage_score) / heritage_var)
    ) %>%
  select(-hdi_var, -dos_var, -population_var, -heritage_var) %>%
  mutate(
    composite_score = round(
      hdi_score + dos_score + population_score + heritage_score, 3))
remove(truncate_score)

## restore variables to unmodified states
country_data <- country_data %>%
  mutate(
    dos_level = 5 - dos_level,
    population = 2^population
    )

## find top scoring countries for each region
suggested_countries <- country_data %>%
  group_by(region) %>%
  slice_max(composite_score, n = 3) %>%
  arrange(region) %>%
  select(region, country_key, composite_score,
    hdi, dos_level, population, heritage_sites, country_iso)

## PLOT WORLD MAP ==========

## PLOT SUGGESTED COUNTRIES FOR EACH REGION ==========

## PLOT SUGGESTED CITIES FOR EACH COUNTRY ==========

##########==========##########==========##########==========##########==========
