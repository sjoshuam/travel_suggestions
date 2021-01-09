##########==========##########==========##########==========##########==========

## SET UP ==========

## Meta-information
## Author: Joshua Mendelsohn
## Creation: 2021-01-04
## Version: 4.0.3
## Description: Compiles country-level data and generates a final dataset that
   ## can provide travel suggestions based on multiple considerations

## environmental set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, load_cached_geocode = TRUE,
  adjust_advice_for_covid = FALSE)
library(ggmap)
library(readxl)
library(tidyverse)

## load API key
source("~/Documents/Coding/api_keys/GoogleAPI.R")

## READ IN DATA ==========

## read in new data
country_development <- read_xlsx("A_Inputs/county_development.xlsx", skip = 4)
country_codes <- read_xlsx("A_Inputs/country_codes.xlsx")
human_rights <-  read_xlsx("A_Inputs/human_rights.xlsx", skip = 1)

## load processed data from previous scripts
city_data <- readRDS("B_Intermediates/city_data.RData")
dos_advice <- readRDS("B_Intermediates/dos_advice.RData")
heritage_sites <- readRDS("B_Intermediates/heritage_sites.RData")

## RESHAPE NEW COUNTRY DATASETS ==========

## simplify human rights database to most recent relevant data
human_rights <- human_rights %>%
  rename(country = `Country/Territory`, human_rights = Total) %>%
  select(country, Edition, human_rights) %>%
  pivot_wider(names_from = Edition, values_from = human_rights)

average_score <- round(rowMeans(select(human_rights, -country), na.rm = TRUE))
human_rights <- human_rights %>%
  mutate(`human_rights` = if_else(is.na(`2020`), average_score, `2020`)) %>%
  select(country, human_rights)
remove(average_score)

## simplify hdi dataset to most recent, relevant data
backup_hdi <- country_development %>%
  select(-`HDI Rank`, -`Country`) %>%
  apply(2, as.numeric) %>%
  rowMeans(na.rm = TRUE)


country_development <- country_development %>%
  select(Country, "2017", "2018", "2019", "HDI Rank") %>%
  rename("country" = Country, "yr2017" = `2017`, "yr2018" = `2018`,
    "yr2019" = `2019`) %>%
  add_column(backup = backup_hdi) %>%
  mutate(hdi = if_else(yr2019 == "..", yr2018, yr2019)) %>%
  mutate(hdi = if_else(hdi == "..", yr2017, hdi)) %>%
  mutate(hdi = if_else(hdi == "..", as.character(backup), hdi)) %>%
  mutate(hdi = round(as.numeric(hdi), 3)) %>%
  filter(!is.na(hdi)) %>%
  select(country, hdi)


## simplify country codes dataset to relevant data
country_codes <- country_codes %>%
  rename("region1" = `Sub-region Name`, "region" = `Intermediate Region Name`,
    "country_name" = `Country or Area`, "country_iso" = `ISO-alpha3 Code`) %>%
  select(region1, region, country_name, country_iso) %>%
  mutate(region = if_else(region == "Channel Islands",
    as.character(NA), region)) %>%
  mutate(region = ifelse(is.na(region), region1, region)) %>%
  select(-region1)

## ADJUST STATE DEPARTMENT ADVISORIES FOR COVID ==========
## note: this adjustment assumes that travel will be post-covid and
  ## improves advisory levels if covid is the only reason that
  ## travel is discouraged.

if (options()$adjust_advice_for_covid) {
  dos_advice <-  mutate(dos_advice, dos_level = covid_adj, dos_reason = reasons)
  } else {
  dos_advice <-  mutate(dos_advice, dos_level = level_num, dos_reason = reasons)
  }

dos_advice <- select(dos_advice, -covid_adj, -reasons, -level_num)

## STANDARDIZE COUNTRY PRIMARY KEYS AND COMPILE COUNTRY DATA ==========

if (!options()$load_cached_geocode) {

## compile all unique country strings
all_countries <- c(
  pull(country_development, country),
  pull(country_codes, country_name),
  pull(dos_advice, country),
  pull(city_data, country),
  pull(human_rights, country)
  )
all_countries <- unique(sort(all_countries))

## clean out string detritus
all_countries <- bind_cols(
  "keys" = all_countries,
  "queries" = str_replace(all_countries, "[^A-Za-z ]", "")
)

## query google API to resolve names
all_countries <- bind_cols(all_countries,
  geocode(pull(all_countries, queries), output = "latlona")) %>%
  select(-lon, -lat) %>%
  mutate(country_key = str_replace_all(address, ".*, ", ""))

## cache results
saveRDS(all_countries, file = "B_Intermediates/countries_geocode_cache.RData")
remove(all_countries)

}

## load cache
all_countries <- readRDS("B_Intermediates/countries_geocode_cache.RData") %>%
  select(-address, -queries)

## fix mis-codes
recode_list <- tribble(
  ~find, ~replace,
  "Curaçao",  "curacao",
  "Georgia",  "georgia",
  "Holy See", "vatican",
  "Jordan",   "jordan",
  "Namibia",  "namibia",
  "Réunion",  "reunion",
  "Togo",     "togo"
  )
recode_list <- all_countries %>%
  select(keys) %>%
  left_join(recode_list, by= c("keys" = "find"))

all_countries <- all_countries %>%
  bind_cols(select(recode_list, replace)) %>%
  mutate("country_key" = if_else(is.na(replace), country_key, replace)) %>%
  select(-replace)




## match google country keys to all country datasets
country_development <- country_development %>%
  left_join(all_countries, by = c("country" = "keys"))
country_codes <- country_codes %>%
  left_join(all_countries, by = c("country_name" = "keys"))
dos_advice <- dos_advice %>%
  left_join(all_countries, by = c("country" = "keys"))
city_data <- city_data %>%
  left_join(all_countries, by = c("country" = "keys"))
human_rights <- human_rights %>%
  left_join(all_countries, by = c("country" = "keys"))
remove(all_countries)

## merge country-wise datasets
country_codes <- country_codes %>%
  left_join(select(country_development, -country), by = "country_key") %>%
  left_join(select(dos_advice, -country), by = "country_key") %>%
  left_join(select(human_rights, -country), by = "country_key") %>%
  select(region, country_iso, hdi, dos_level, dos_reason, human_rights,
    country_key)

##  transfer missing travel advisories for territories from parent country
territories <- tribble(
  ~territory, ~new_key,
  "PRI", "USA", "VIR", "USA", "GUM", "USA", "MNP", "USA", "ASM", "USA",
  "MTQ", "FRA", "SPM", "FRA", "PYF", "FRA", "WLF", "FRA", "GUF", "FRA",
                "NCL", "FRA",
  "JEY", "GBR", "IMN", "GBR", "PCN", "GBR", "BMU", "GBR", "CYM", "GBR",
                "AIA", "GBR",
  "GRL", "DNK", "FRO", "DNK",
  "NIU", "NZL", "FLK", "ARG", "ALA", "FIN", "SMR", "ITA", "ABW", "NLD"
  )

territories <- left_join(country_codes, territories,
  by = c("country_iso" = "territory")) %>%
  select("country_iso", "new_key") %>%
  left_join(country_codes, by = c("new_key" = "country_iso")) %>%
  select(country_iso, new_key, dos_level, hdi, human_rights) %>%
  rename("dos2" = dos_level, "hdi2" = hdi, "hr2" = human_rights) %>%
  mutate(hdi2 = if_else(is.na(new_key), as.numeric(NA), hdi2))

country_codes <- country_codes %>%
  add_column(select(territories, dos2, hdi2, hr2)) %>%
  mutate(dos_level = if_else(is.na(dos_level), dos2, dos_level)) %>%
  mutate(hdi = if_else(is.na(hdi), hdi2, hdi)) %>%
  mutate(human_rights = if_else(is.na(human_rights), hr2, human_rights)) %>%
  select(-dos2, -hdi2, -hr2)

remove(territories)

## interpolate missing hdi codes to just below the regional median
regional_hdi <- country_codes %>%
  select(region, hdi) %>%
  group_by(region) %>%
  summarize("regional_hdi" = median(hdi, na.rm = TRUE) - 0.05)

country_codes <- country_codes %>%
  left_join(regional_hdi, by = "region") %>%
  mutate(hdi = if_else(is.na(hdi), regional_hdi, hdi)) %>%
  select(-regional_hdi)
remove(regional_hdi)

## drop incomplete records
country_data <- drop_na(country_codes, dos_level, hdi)
remove(country_development, dos_advice, country_codes)

## INCORPORATE CITY DATA ==========

## drop cities that do not have corresponding entries in country data
city_data <- semi_join(city_data, country_data, by = "country_key") %>%
  group_by(country_key)

## find notable cities in each country by three criteria
capitol_city <- city_data %>%
  arrange(population) %>%
  arrange(capitol) %>%
  summarize("capitol" = last(address))

largest_city <- city_data %>%
  arrange(population) %>%
  summarize("largest" = last(address))

culture_city <- city_data %>%
  arrange(population) %>%
  arrange(heritage_sites) %>%
  summarize("cultural" = last(address))

top_cities <- capitol_city %>%
  left_join(largest_city, by = "country_key") %>%
  left_join(culture_city, by = "country_key")
remove(largest_city, capitol_city, culture_city)

## extract data for each notable city and package for the country dataset
top_cities <- top_cities %>%
  pivot_longer(c("capitol", "largest", "cultural"), names_to = "criteria") %>%
  left_join(
    select(ungroup(city_data), city, capitol, population, heritage_sites, lon,
      lat, address, heritage_key),
    by = c("value" = "address")
    ) %>%
  filter(!duplicated(value)) %>%
  select(-value) %>%
  group_by(country_key) %>%
  nest() %>%
  rename("notable_cities" = "data")

country_data <- left_join(country_data, top_cities, by = "country_key") %>%
  filter(!sapply(notable_cities, is.null)) %>%
  arrange(country_key) %>%
  filter(!duplicated(country_key))
remove(top_cities)

save(city_data, country_data, heritage_sites,
  file = "B_Intermediates/processed_data.RData")

##########==========##########==========##########==========##########==========
