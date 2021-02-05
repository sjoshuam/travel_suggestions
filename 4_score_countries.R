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
library(tidyverse)

## READ IN DATA ==========
load("B_Intermediates/processed_data.RData")

## CALCULATE REGIONAL MEDIANS AND SCORE COUNTRIES AGAINST REGION ===============

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
    heritage_var = max(mad(heritage_sites), 0.5),
    rights_score = median(human_rights),
    rights_var = max(mad(human_rights), 0.5),
    english_score = median(speaks_english),
    english_var = max(mad(speaks_english), 0.5)
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
      (heritage_sites - heritage_score) / heritage_var),
    rights_score = truncate_score(
      (human_rights - rights_score) / rights_var),
    english_score = truncate_score(
      (speaks_english - english_score) / english_var)
    ) %>%
  select(-hdi_var, -dos_var, -population_var, -heritage_var, -rights_var,
    -english_var) %>%
  mutate(
    composite_score = round(
      population_score * 0.25 +
      heritage_score   * 0.25 +
      dos_score        * 0.15 +
      rights_score     * 0.15 +
      hdi_score        * 0.10 +
      english_score    * 0.10,
      3))
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
  slice_max(composite_score, n = 1) %>%
  arrange(region) %>%
  select(country_key, composite_score,
    population, heritage_sites,
    hdi, dos_level, human_rights,
    region, country_iso
    )

## SAVE SCORING RESULTS ==========
score_data <- country_data
save(score_data, suggested_countries, file = "B_Intermediates/score_data.RData")


##########==========##########==========##########==========##########==========
