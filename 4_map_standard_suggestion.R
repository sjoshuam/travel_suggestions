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
library(mapproj)
library(tidyverse)
library(ggmap)

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

## PREPARE MAP FOR MAPPING ==========

## extract world map data; drop polar regions
world_map <- map_data("world") %>%
  as_tibble() %>%
  filter(
    region != "Antarctica",
    tapply(lat, group, min)[group] < 70
  ) %>%
  rename(map_country_name = region) %>%
  mutate(country_key = str_to_lower(map_country_name))

world_map$country_key <- recode(world_map$country_key ,
  "swaziland" = "eswatini", "czech republic" = "czechia",
  "ivory coast" = "côte d'ivoire",
  "falkland islands" = "falkland islands (islas malvinas)",
  "macedonia" = "north macedonia", "uk" = "united kingdom",
  "bahamas" = "the bahamas", "tobago" = "trinidad and tobago",
  "trinidad" = "trinidad and tobago", "gambia" = "the gambia",
  "sao tome and principe" = "são tomé and príncipe",
  "saint lucia" = "st lucia", "myanmar" = "myanmar (burma)",
  "virgin islands" = "u.s. virgin islands", "usa" = "united states",
  "micronesia" = "federated states of micronesia",
  "saint pierre and miquelon" = "st pierre and miquelon"
)

world_map <- world_map %>%
  left_join(select(country_data, country_key, region), by= "country_key") %>%
  mutate(region = if_else(is.na(region), "Missing Data", region))

## temporarily patch missing countries
warning("This is a temporary patch")
i <- match(
  c("central african republic", "democratic republic of the congo",
    "republic of congo", "libya", "sudan", "taiwan"), 
  world_map$country_key)
world_map[i, "region"] <- c(rep("Africa Middle", 3), rep("Africa North", 2),
  rep("Asia East", 1))


## project map to winkel-tripel
winkel_tripel <- function(dat) {
  
  aitoff <- mapproject(x = dat$long, y= dat$lat,
    projection = "aitoff", orientation = c(90, 0, 0))[c("x", "y")]

  cylindrical <- mapproject(x = dat$long, y= dat$lat,
    projection = "rectangular", orientation = c(90, 0, 0),
    parameters = 50)[c("x", "y")]

  wt <- (simplify2array(aitoff) + simplify2array(cylindrical)) / 2
  colnames(wt) <- c("wt_long", "wt_lat")
  dat <- bind_cols(dat, as_tibble(wt))
  dat
  }

world_map <- winkel_tripel(world_map)

## render map
generic_map <- ggplot(data = world_map) +
  coord_fixed(xlim = c(-2, 2), ratio = 1) +
  geom_polygon(
    data = world_map,
    mapping = aes(x = wt_long, y = wt_lat, group = group, fill = region,
      color = region),
    size = 0.08
    )

## adjust plot theme elements
generic_map <- generic_map + theme(
    panel.background = element_rect(
      fill = hsv(h = 0.6, v = 0, s = 0.1),
      color = hsv(h = 0.6, v = 0, s = 0.5)
      ),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
    )

## add color palette
color_palette <- tibble(
  "region"= c(sort(unique(country_data$region)), "Missing Data"),
  "hue" = rep(seq(from = 0, to = 0.8, by = 0.2), times = 5)[1:23],
  "sat" = 0.7,
  "fill_val" = rep(c(0.2, 0.3, 0.3, 0.2, 0.3), times = 5)[1:23],
  ) %>%
  mutate(fill_color = hsv(hue, sat, fill_val)) %>%
  mutate(color_color = hsv(hue, 1, 1)) %>%
  mutate(fill_color =
      if_else(region == "Missing Data", gray(0.2), fill_color)) %>%
  mutate(color_color =
      if_else(region == "Missing Data", gray(0.8), color_color))

i <- which(color_palette$region %in% c("Asia Central", "America North"))
color_palette[i, "region"] <- color_palette[rev(i), "region"]
remove(i)

generic_map <- generic_map +
  scale_fill_manual(
    values = set_names(color_palette$fill_color, color_palette$region),
    guide = FALSE) +
  scale_color_manual(
    values = set_names(color_palette$color_color, color_palette$region),
    guide = FALSE)

## add labels for selected countries
selected_labels <- world_map %>%
  group_by(group) %>%
  summarize(
    "country_key" = unique(country_key),
    long_max = max(wt_long),
    long_min = min(wt_long),
    lat_max = max(wt_lat),
    lat_min = min(wt_lat),
    points = length(wt_long)
    ) %>%
  arrange(country_key, -points) %>%
  filter(!duplicated(country_key)) %>%
  mutate(
    long = (long_max + long_min) / 2,
    lat  = (lat_max + lat_min) / 2,
    ) %>%
  select(country_key, long, lat) %>%
  filter(country_key %in% suggested_countries$country_key) %>%
  left_join(select(suggested_countries, country_key, region),
    by = "country_key") %>%
  left_join(select(color_palette, region, color_color), by = "region")
  

i <- match(c("netherlands", "poland"),
  selected_labels$country_key)
selected_labels[i, "long"] <- selected_labels[i, "long"] + c(-0.1, 0.1)
remove(i)

generic_map <- generic_map + geom_label(
  data = selected_labels,
  mapping = aes(long, lat, label = str_to_title(country_key)),
  size = 2.0,
  label.padding = unit(0.2, "lines"),
  color = selected_labels$color_color,
  fill = "#000000CC"
  )
#remove(selected_labels)

##
pdf("C_Outputs/travel_suggestions_generic.pdf", width = 6, height = 6 / 1.9)
generic_map
graphics.off()

save(generic_map, file = "B_Intermediates/generic_map.RData")
remove(winkel_tripel, selected_labels, color_palette, generic_map, world_map)

## PACKAGE SELECTED COUNTRIES FOR RESULTS TABLE

## extract selected countries from country_data
output_table <- country_data %>%
  filter(country_key %in% suggested_countries$country_key) %>%
  arrange(region) %>%
  select(region, country_key, notable_cities)

## extract heritage sites
output_table <- output_table %>%
  mutate("heritage_sites" = notable_cities)
output_table$heritage_sites <- output_table$heritage_sites %>%
  lapply(pull, heritage_key)

## extract city names
output_table$notable_cities <- output_table$notable_cities %>%
  lapply(pull, city) %>%
  sapply(paste, collapse = " • ") %>%
  str_to_title() #%>%
  #str_replace_all("&Nbsp;", "&nbsp;")

output_table$country_key <- str_to_title(output_table$country_key)

## add heritage sites
heritage_output <-  setNames(output_table$heritage_sites,
  nm = output_table$country_key) %>%
  lapply(unlist)

heritage_output <- tibble(
  "country_key" = rep(names(heritage_output), sapply(heritage_output, length)),
  "prim_key" = unlist(heritage_output)
  ) %>%
  left_join(select(heritage_sites, prim_key, name_en))

heritage_output <-  tapply(heritage_output$name_en,
  heritage_output$country_key,
  paste, collapse = " • ") %>%
  enframe(name = "country_key", value = "heritage_sites")

output_table <- output_table %>%
  select(-heritage_sites) %>%
  left_join(heritage_output, by = "country_key")

## write table in raw markdown format
output_table <- paste(
  output_table$region,
  output_table$country_key,
  output_table$notable_cities,
  output_table$heritage_sites,
  sep = "|"
  )

output_table <- c("Region|Country|Cities|Heritage Sites",
  "|:-|:-|:-|:-",
  output_table)
write_lines(output_table, file = "C_Outputs/travel_suggestions_generic.txt")
  

##########==========##########==========##########==========##########==========
