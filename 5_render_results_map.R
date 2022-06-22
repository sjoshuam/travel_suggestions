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
library(mapproj)
library(readxl)
library(tidyverse)

## READ IN DATA ================================================================

load("B_Intermediates/score_data.RData")
poster_dims <- read_xlsx("A_Inputs/poster_dims.xlsx")
heritage_sites <- readRDS("B_Intermediates/heritage_sites.RData")

## PREP POSTER DIMENSIONS FILE =================================================

## convert to easier format for bracket look-ups
poster_dims <- as.data.frame(poster_dims)
rownames(poster_dims) <- poster_dims$element

## generate ggplot file
travel_poster <- ggplot() +
  coord_fixed(xlim = c(0, 36), ylim = c(0, 24), ratio = 1, expand = FALSE)

## PREPARE GEOGRAPHIC DATA =====================================================

## extract world map data; drop polar regions
world_map <- map_data("world") %>%
  as_tibble() %>%
  filter(
    region != "Antarctica",
    tapply(lat, group, min)[group] < 70
  ) %>%
  rename(map_country_name = region) %>%
  mutate(country_key = str_to_lower(map_country_name))

world_map$country_key <- recode(world_map$country_key,
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
  left_join(select(score_data, country_key, region), by = "country_key") %>%
  mutate(region = if_else(is.na(region), "Missing Data", region))

## temporarily patch missing countries
warning("This is a temporary patch")
i <- match(
  c("central african republic", "democratic republic of the congo",
    "republic of congo", "libya", "sudan", "taiwan"),
  world_map$country_key)
world_map[i, "region"] <- c(rep("Africa Middle", 3), rep("Africa North", 2),
  rep("Asia East", 1))
remove(i)

## project map to winkel-tripel
winkel_tripel <- function(dat, normalize = FALSE) {

  aitoff <- mapproject(x = dat$long, y = dat$lat,
    projection = "aitoff", orientation = c(90, 0, 0))[c("x", "y")]

  cylindrical <- mapproject(x = dat$long, y = dat$lat,
    projection = "rectangular", orientation = c(90, 0, 0),
    parameters = 50)[c("x", "y")]

  wt <- (simplify2array(aitoff) + simplify2array(cylindrical)) / 2
  colnames(wt) <- c("wt_long", "wt_lat")

  if (normalize) {
    wt <- wt - apply(wt, 2, min)[col(wt)]
    wt <- wt / max(wt)
    }

  dat <- bind_cols(dat, as_tibble(wt))
  dat
}

world_map <- winkel_tripel(world_map, normalize = TRUE)
world_map$long <- world_map$lat <- NULL
remove(winkel_tripel)

## MAP PANEL ===================================================================

## scale coordinates to plot dimension
world_map$wt_long <- (world_map$wt_long * poster_dims["map_panel", "x_size"]
  ) + poster_dims["map_panel", "x_start"]
world_map$wt_lat <- (world_map$wt_lat * poster_dims["map_panel", "x_size"]
  ) + poster_dims["map_panel", "y_start"]

## define M49 region colors
recycle_vector <- function(x, n) {
  x <- rep(x, ceiling(n / length(x)))
  x <- x[1:n]
  }

region_legend <- select(world_map, region) %>%
  unique() %>%
  arrange(region) %>%
  mutate("hue" = recycle_vector(
    seq(from = 1 / 24, to = 23 / 24, by = 1 / 6), length(region))) %>%
  mutate("fill"  = hsv(h = hue, s = 0.5, v = 1 / 6)) %>%
  mutate("color" = hsv(h = hue, s = 0.5, v = 1.0)) %>%
  mutate(
    "fill" = if_else(region == "Missing Data", hsv(s = 0, v = 1 / 6), fill)) %>%
  mutate(
    "color" = if_else(region == "Missing Data", hsv(s = 0, v = 0.7), color))
remove(recycle_vector)

## define country centroids
world_map <- world_map %>%
  mutate(subregion = if_else(is.na(subregion), "", subregion)) %>%
  filter(subregion != "Pitt Island", subregion != "Chatham Island") %>%
  filter(
    !((subregion %in% c("12", "13") & country_key == "new zealand"))
    )

range_mean <- function(x) {
  mean(range(x))
  }
country_centroids <- tapply(
  unlist(world_map[, c("wt_long", "wt_lat")]),
  list(
    unlist(world_map[, c("country_key", "country_key")]),
    c("wt_long", "wt_lat")[col(world_map[, c("wt_long", "wt_lat")])]
    ),
  range_mean
  ) %>%
  as.data.frame()
country_centroids$country_key <- rownames(country_centroids)
suggested_countries <- left_join(suggested_countries, country_centroids)
remove(country_centroids, range_mean)

## manually nudge labels as needed
warning("This is a manual intervention (auto solution not worth coding")
i <- c("American Samoa", "Germany", "Poland")
i <- match(i, str_to_title(suggested_countries$country_key))
suggested_countries[i, "wt_long"] <- suggested_countries[i, "wt_long"] +
  c(1, -1, 1) * 0.4
remove(i)

## render countries
travel_poster <- travel_poster +
  geom_polygon(
    data = world_map,
    mapping = aes(x = wt_long, y = wt_lat, group = group, color = region,
      fill = region),
    size = 0.7
    ) +
  scale_color_manual(
    values = setNames(region_legend$color, region_legend$region)) +
  scale_fill_manual(
    values = setNames(region_legend$fill, region_legend$region))

## render country labels and title
travel_poster <- travel_poster +
  geom_label(
    data = suggested_countries,
    mapping = aes(x = wt_long, y = wt_lat, group = region,
      label = str_to_title(country_key), color = region),
    size = 2^3,
    label.padding = unit(0.5, units = "lines"),
    label.r = unit(0.5, units = "lines"),
    label.size = 0.7,
    fill = "black"
    ) +
  geom_text(
    data = tibble(NA),
    x = mean(unlist(poster_dims["map_title", c("x_start", "x_end")])),
    y = poster_dims["map_title", "y_end"],
    label = "Twenty-Two Travel Suggestions For The Aspiring World Traveler",
    color = "gray90",
    size = 10, vjust = 1
    )

## METHODS PANEL ===============================================================

## Generate explanatory text
explanatory_text <- c(
  "This project divides the world ",
  "into twenty-two regions and suggests places to visit in each region.  The ",
  "regions are based on the United Nation's \"M49\" regional categorization ",
  "system.  For each region, the project selects up to three metro ",
  "areas in one country, based on points of interest and ease of travel.  ",
  "The map on the left depicts the selected countries (the text) and the ",
  "regions (the colors).  The bottom panel lists up to three cities selected ",
  "for each country and all of the UNESCO World Cultural Heritage Sites ",
  "within a 50-kilometer (31-mile) radius of each city (indented below each ",
  "city).  The rest of this panel lists the decision criteria for selecting ",
  "travel destinations and how much each counted towards the final selection. "
  )
explanatory_text <- explanatory_text %>%
  paste0(collapse = "") %>%
  strwrap(73) %>%
  paste0(collapse = "\n") %>%
  str_replace_all("∆", "\n") %>%
  enframe() %>%
  pivot_wider() %>%
  magrittr::set_colnames("label")
explanatory_text <- bind_cols(
  explanatory_text,
  as_tibble(poster_dims["method_panel", c("x_start", "y_end")])
  )

## generate selection criteria text

selection_criteria <- tribble(
  ~type, ~criteria, ~source, ~weight,

  "City", "City is the country's capital",
  "Source: United Nations (UN)", 33,

  "City", "City is the country's largest",
  "S: United Nations", 33,

  "City", "City is near the most cultural sites",
  "S: UN World Heritage Sites", 33,

  "Point Of Interest", "Population of largest city",
  "Source: United Nations", 25,

  "Point Of Interest", "Cultural sites near selected cities",
  "S: UN World Heritage Sites", 25,

  "Ease Of Travel", "Safety for Americans",
  "S: United States State Department", 15,

  "Ease Of Travel", "Respect for human rights",
  "S: Freedom House", 15,

  "Ease Of Travel", "English speakers in population",
  "S: Compilation", 10,

  "Ease Of Travel", "Societal infrastructure",
  "S: UN Human Development Index", 10
  ) %>%
  mutate(
    "label" = paste(criteria, source, sep = "\n  "),
    "type" = if_else(type == "City", "City", "Country")
    )

## calculate selection criteria coordinates
selection_criteria <- selection_criteria %>%
  mutate(
    "x_left" = (poster_dims["method_panel", "x_start"] +
        poster_dims["method_panel", "x_end"]) / 2,
    "y_top" = poster_dims["method_panel", "y_end"] - 3.4 - seq(length(x_left)) *
      (0.5 * 2)
    ) %>%
  mutate("y_top" = y_top + cumsum(!duplicated(type)) * -0.5)

## calculate weight bars and colors
selection_criteria <- selection_criteria %>%
  mutate(
    y_bottom = y_top - 0.55,
    x_right = (weight / 100) * 10 + x_left,
    fill = type == "City"
    ) %>%
  mutate(
    color = if_else(fill,
      hsv(h = 0.25, s = 0.5, v = 1.0),
      hsv(h = 0.75, s = 0.5, v = 1.0)),
    fill = if_else(fill,
      hsv(h = 0.25, s = 0.5, v = 1 / 6),
      hsv(h = 0.75, s = 0.5, v = 1 / 6))
  )
  
## calculate title characteristics
selection_titles <- selection_criteria %>%
  group_by(type) %>%
  summarize(
    x = mean(x_left),
    y = max(y_top) + 0.35,
    color = unique(color)
    ) %>%
  mutate(
    "label" = c("City Selection Criteria", "Country Criteria")
    )

## render selection criteria weight bars
travel_poster <- travel_poster + geom_rect(
  data = selection_criteria,
  mapping = aes(
    xmin = x_left + 0.2,
    xmax = x_right,
    ymin = y_bottom,
    ymax = y_top
    ),
  fill = selection_criteria$fill,
  color = selection_criteria$color
  ) +
  geom_text(
    data = selection_titles[1, ],
    mapping = aes(x = x + 0.2, y = y),
    label = "Influence On Final Decision",
    color = "gray90",
    hjust = 0,
    size = 6
    )

## render selection criteria text and percentages
travel_poster <- travel_poster +
  geom_text(
    data = selection_criteria,
    mapping = aes(x = x_left, y = y_top, label = label),
  color = selection_criteria$color,
    size = 6, hjust = 1, vjust = 1
    ) +
  geom_text(
    data = selection_titles,
    mapping = aes(x = x, y = y, label = label),
    color = selection_titles$color,
    size = 10, hjust = 1
    ) +
  geom_text(
    data = selection_criteria,
    mapping = aes(x = x_left + 0.25, y = y_top - 0.16,
      label = paste0(weight, "%")),
  color = selection_criteria$color,
    size = 8, hjust = 0, vjust = 1
    )

## render panel title and explanatory text
travel_poster <- travel_poster +
  geom_text(
    data = tibble(NA),
    x = mean(unlist(poster_dims["method_title", c("x_start", "x_end")])),
    y = poster_dims["method_title", "y_end"],
    label = "Selection Criteria for the Suggestions",
    color = "gray90",
    size = 10, vjust = 1,
    ) +
  geom_text(
    data = explanatory_text,
    mapping = aes(x = x_start, y = y_end - 0.1, label = label),
    col = "gray90", hjust = 0, vjust = 1, size = 6
    )

remove(selection_criteria, selection_titles, explanatory_text)

## SITE PANEL ==================================================================

## extract heritage site data
country_sites <- suggested_countries %>%
  select(country_key) %>%
  left_join(
    select(score_data, country_key, notable_cities),
    by = "country_key"
    ) %>%
  unnest(notable_cities) %>%
  unnest(heritage_key, keep_empty = TRUE) %>%
  select(country_key, region, city, prim_key) %>%
  ungroup() %>%
  mutate(
    "country" = str_to_title(country_key),
    "city" = str_to_title(city)
    ) %>%
  left_join(
    select(heritage_sites, name_en, prim_key),
    by = "prim_key"
    ) %>%
  rename("site" = name_en) %>%
  select(country_key, region, country, city, site) %>%
  arrange(region, country, city, site) %>%
  mutate("site" = if_else(is.na(site), "", site)) %>%
  filter(!nzchar(site) | !duplicated(site))
remove(heritage_sites)

## truncate site names
i <- nchar(country_sites$site) > 34
country_sites$site[i] <- paste0(
  str_sub(country_sites$site[i], 1, 31),
  "..."
  )
remove(i)

## convert site data to a simpler text format
compile_data <- function(x) {
  y <- x[nzchar(str_replace_all(x, "\t", ""))]
  y <- y %>% unique() %>% paste(collapse = "\n")
  str_replace_all(y, "\n\n", "\n")
  }

country_sites$site <- paste0("\t\t", country_sites$site)
country_sites$city <- paste0("\t",   country_sites$city)
country_sites$site <- tapply(country_sites$site, country_sites$city,
  compile_data)[country_sites$city]
country_sites$city <- paste(country_sites$city, country_sites$site, sep = "\n")
country_sites$city <- tapply(country_sites$city, country_sites$country,
  compile_data)[country_sites$country]
country_sites <- distinct(country_sites, country_key, region, country, city)
country_sites$city <- str_replace_all(country_sites$city, "\t", "  ")
country_sites$city <- iconv(country_sites$city, to = "UTF-8", sub = "")

## order sites by size and name
country_sites$city <- str_replace_all(country_sites$city, "\n+$", "")
country_sites$lines <- country_sites$city %>%
  str_replace_all("[^\n]", "") %>%
  nchar() + 1

country_sites <- arrange(country_sites, lines)

## make extra room for two longest entries
swap_elements <- function(x, i = 17:22, j = c(22:17)) {
  y <- x[i]
  x[i] <- x[j]
  x[j] <- y
  x
}
country_sites$country_key <- swap_elements(country_sites$country_key)
country_sites$region <- swap_elements(country_sites$region)
country_sites$country <- swap_elements(country_sites$country)
country_sites$city <- swap_elements(country_sites$city)
country_sites$lines <- swap_elements(country_sites$lines)

country_sites$country_key <- swap_elements(country_sites$country_key,
  i = 1:2, j = 4:3)
country_sites$region <- swap_elements(country_sites$region,
  i = 1:2, j = 4:3)
country_sites$country <- swap_elements(country_sites$country,
  i = 1:2, j = 4:3)
country_sites$city <- swap_elements(country_sites$city,
  i = 1:2, j = 4:3)
country_sites$lines <- swap_elements(country_sites$lines,
  i = 1:2, j = 4:3)

remove(swap_elements)

## add dummy entries to complete grid
country_sites <- bind_rows(
  country_sites,
  country_sites[10:9, ]
  )
country_sites[9:10, ] <- country_sites[1:2, ]
country_sites[9:10, "lines"] <- 0
country_sites[9:10, "city"] <- ""

## determine x coordinates
country_sites$x <- seq(
  from = poster_dims["site_panel", "x_start"],
  to = poster_dims["site_panel", "x_end"],
  length.out = 9
  ) %>%
  magrittr::extract(seq(8)) %>%
  rep(times = 3) %>%
  magrittr::extract(seq(nrow(country_sites)))

## create default y coordinates
str_height <- function(x, n = 0.2) {
  x <- pull(x, 1)
  x <- str_remove_all(x, "[^\n]")
  x <- nchar(x) + 1
  x * 0.35
  }

country_sites$y_title <- 23
country_sites$y_sites <- 23

## generate y coordinates for bottom row
country_sites[17:24, "y_sites"] <- poster_dims["site_panel", "y_start"]
country_sites[17:24, "y_title"] <- poster_dims["site_panel", "y_start"] +
      str_height(country_sites[17:24, "city"])

## generate y coordinates for top row
country_sites[1:8, "y_title"] <- poster_dims["site_panel", "y_end"] - 0.05
country_sites[1:8, "y_sites"]  <- country_sites[1:8, "y_title"] - 0.45

## generate y coordinates for middle row
from_above <- country_sites[1:8, "y_title"]  +
  str_height(country_sites[1:8, "city"]) + 0.45

from_below <- country_sites[17:24, "y_sites"] +
  str_height(country_sites[17:24, "city"]) + 0.45

country_sites[9:16, "y_title"] <- (from_above + from_below) / 2
country_sites[9:16, "y_sites"] <-  country_sites[9:16, "y_title"] - 0.45

## manually adjust middle row
warning("This is a manual intervention (auto solution not worth coding")
i <- c(13, 14)
country_sites[i, "y_title"] <- country_sites[i, "y_title"] - 0.5
country_sites[i, "y_sites"] <- country_sites[i, "y_sites"] - 0.5
remove(i)

## render panel title and sites
travel_poster <- travel_poster +
  geom_text(
    data = tibble(NA),
    x = mean(unlist(poster_dims["site_title", c("x_start", "x_end")])),
    y = poster_dims["site_title", "y_end"],
    label = paste0("Suggested Cities And World Cultural Heritage Sites To Visit",
    " In Each Country"),
    color = "gray90",
    size = 10, vjust = 1
    )

## render bottom row
travel_poster <- travel_poster +
  geom_text(
    data = country_sites[17:24, ],
    mapping = aes(x = x, label = city, color = region, y = y_sites),
    hjust = 0, vjust = 0, size = 6
    ) +
  geom_text(
    data = country_sites[17:24, ],
    mapping = aes(x = x, label = country, color = region, y = y_title),
    hjust = 0, vjust = 0, size = 10
    )

## render top two rows
travel_poster <- travel_poster +
  geom_text(
    data = country_sites[c(1:8, 11:16), ],
    mapping = aes(x = x, label = city, color = region, y = y_sites),
    hjust = 0, vjust = 1, size = 6
    ) +
  geom_text(
    data = country_sites[c(1:8, 11:16), ],
    mapping = aes(x = x, label = country, color = region, y = y_title),
    hjust = 0, vjust = 1, size = 10
    )

## RENDER POSTER ===============================================================

## specify theme elements
travel_poster <- travel_poster +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), units = "in")
    )

## render dividing lines
travel_poster <- travel_poster +
  geom_segment(
    data = filter(poster_dims, str_detect(element, "horizontal_divider")),
    mapping = aes(
      x = x_start, xend = x_end,
      y = (y_start + y_end) / 2, yend = (y_start + y_end) / 2
      ),
    color = "gray50",
    size = 1
    ) +
  geom_segment(
    data = filter(poster_dims, str_detect(element, "vertical_divider")),
    mapping = aes(
      x = (x_start + x_end) / 2, xend = (x_start + x_end) / 2,
      y = y_start, yend = y_end
      ),
    color = "gray50",
    size = 1
    )

## write visualization to storage in pdf and RData formats
pdf("C_Outputs/travel_suggestions_poster.pdf", width = 36, height = 24)
travel_poster
graphics.off()

png("C_Outputs/travel_suggestions.png", width = 36, height = 24, units = "in",
  res = 72)
travel_poster
graphics.off()

##########==========##########==========##########==========##########==========
