### Description
This project generates a slate of travel suggestions that collectively provide a well-rounded sample of the world.  The world is a big place, so the slate is woefully small.  However, a 22-destination slate means that a traveler can realistically visit all locations at a pace of one vacation per year over the course of a couple of decades.

The project considers multiple “point of interest” and “ease of travel” factors in generating the recommendations, using a three step selection procedure.

First, the procedure selects three areas (centered around cities) to represent each country.  Since many countries are quite large, this ensures that a traveler can easily see all recommended sites in one trip.  The three city selection criteria are:

1.	City is the country’s capital.  Source: United Nations Statistical Division via file download.
2.	City is the country’s most populous metropolitan area (aside from the capital city where applicable).  Source: United Nations Statistical Division via file download.
3.	City is near the most UNESCO World Heritage Sites.  Source: United Nations Educational, Scientific and Cultural Organization (UNESCO) via file download.  Google’s Geocoding API provided the coordinates for distance calculations.

Second, the procedure scores countries based on the points of interest near/in the three cities, as well as country-wide ease of travel considerations.  The five scoring criteria are:

1.	Population of the largest city.  This accounts for 25% of the final score.  Source: United Nations Statistical Division via file download.
2.	Total UNESCO World Heritage Sites within 50 kilometers of the selected cities for each country.  This accounts for 25% of the final score.  Source: United Nations Educational, Scientific and Cultural Organization (UNESCO) via file download.  Google’s Geocoding API provided the coordinates for distance calculations.
3.	Safety for citizens of the United States.  This accounts for 15% of the final score.  Source: United States Department of State Travel Advisories via RSS feed.
4.	Respect for human rights.  This accounts for 15% of the final score.  Source:  Freedom House Global Freedom Scores via file download.
5.	English speakers in the country’s population.  This accounts for 10% of the final score.  Source:  Wikipedia compilation of academic statistics via web scrape.  Better, non-Wikipedia source needed.
6.	Societal infrastructure, as reflected in inequality-adjusted Human Development Index scores.  The logic here is that a country that can keep citizens healthy, educated and not impoverished is probably able to provide a safer, less frustrating experience for tourists.  There are other schools of thought.  This factor accounts for 10% of the final score.  Source:  United Nations Development Programme.

See the code and source file “Source” tabs for direct links to sources.

Third, the selection procedure divides countries into 22 regions, according to the United Nations M49 region codes.  For each region, the procedure selects the top-scoring country

### Repository Layout

This repository contains 3-4 directories.  For convenience, each directory name has a unique letter prefix, such as "X_".

+ **A_Inputs** - Holds all source data files.  For coding purposes, I treat this directory as read only.
+ **B_Intermediates** – Holds data files created as byproducts of the computations performed.  This can include cached API pulls, temporary data caches for large computations, and data objects to be passed from one script to the next.
+ **C_Outputs** – Holds the final products for the project.
+ **D_Misc** – If needed, this directory will contain any other useful materials for the project, such as source data documentation.

The scripts containing computer code are in the top level of the repository. Each script has a unique numeric prefix, such as "2_", which indicates the order in which to execute the scripts.

### Scripts (Actions, Inputs, Outputs)

**1_scrape_websites.R** - Pulls travel advisory data state.gov via RSS and English-speaker data from wikipedia.org via web scrape.  Shapes data into standard formats.

Inputs: None.  All data acquired from the Internet.

Outputs to B_Intermediates:
+ `dos_advice.RData` - country-level travel advisory data
+ `speaks_english.Rdata` - country-level data on prevalence of English speakers

**2_compile_city_data.R** - Reads in and cleans messy datasets of cities and world heritage sites from the UN.  Geocodes the locations in them using Google's API.  Links sites to cities that are within 50 kilometers. NOTE:
1. Google's API requires a key.  Key not included in the repository code.
2. Geocoding all of the world's urban areas takes a little time.  This data is cached so that the script can be rerun without doing all of those geocoding queries every time.  Set `options(load_cached_geocode = TRUE)` to use the cache instead of querying the geocoding server.

Inputs from A_Inputs:
+ `city_population.xlsx` - Spreadsheet of urban areas around the world and their population, courtesy of the United Nations Statistical Division.  Warning: dataset is not ideal for machine reading.  It uses white space to differentiate cities from other rows, capitalization to denote capital cities, and footnote markers in cells to denote data source.
+ `world_heritage.xlsx` - Spreadsheet of World Heritage Sites from the United Nations Educational, Social, and Cultural Organization (UNESCO).

Outputs to B_Intermediates:
+ `city_data.RData` - data on population, capital city status, location and nearby heritage sites for every large urban area in the world.
+ `heritage_sites.RData` - data on the location and category (Cultural / Natural / Mixed) of every UNESCO World Heritage Site.

**3_compile_country_data.R** - Merges seven datasets into a complete package of all of the data
necessary to scores countries as travel destinations.  Country names vary from dataset to dataset,
so this script uses the Google geocoder API to standardize country names to a reliable merge key.  I manually inspected the results and hardcoded some country standardizations into the dataset to fix mistakes. NOTE:
1. Google's API requires a key.  Key not included in the repository code.
2. Geocoding all of the world's urban areas takes a little time.  This data is cached so that the script can be rerun without doing all of those geocoding queries every time.  Set `options(load_cached_geocode = TRUE)` to use the cache instead of querying the geocoding server.
3.  The hardcoding is implemented in a way that makes failure harmless.  Either it finds a known mistake and fixes it or detects nothing and does nothing.

Inputs from A_Inputs:
+ `country_development.xlsx` – Inequality-adjusted Human Development Index scores for each country, courtesy of the United Nations Development Programme (UNDP)
+ `country_codes.xlsx` – The United Nation's M49 geographic categorization codes.  I use this system to divide the world into 22 regions, generating a travel recommendation for each one.
+ `human_rights.xlsx` – Freedom House's Global Freedom Scores, used to measure a country's respect for human rights.

Inputs from B_Intermediates:
+ `dos.advice.RData` - Travel advisories from the US State Department; product of script #1.
+ `speaks_english.RData` – Whether English is commonly spoken in each country; product of script #1
+ `city_data.RData` – Data on the world's cities; product of script #2.
+ `heritage_sites.RData` – Data on UNESCO World Heritage sites; product of script #2.

Outputs to B_Intermediates:
+ `processed_data.RData` -  A package of three objects.  `county_data` is a dataset of country-level scores.  `city_data` and `heritage_sites` are datasets of cities and World Heritage Sites, respectively.  All objects have reliable primary keys for cross-referencing.

**4_score_countries.R** - From the previous script, this script receives a dataset that scores countries against six evaluation criteria – size of largest city, number of nearby world heritage sites, State Department travel advisories, respect for human rights, human development and prevalence of English speakers. Normalizes the scores for each country relative to the scores of other countries in its regions.  Calculates a weighted composite score from the evaluation criteria to serve as the final arbiter of which country should be recommended for each region.

Inputs from B_Intermediates: `processed_data.Rdata` from script #3.

Outputs to B_Intermediates: `score_data.RData`.

**5_render_results_map.R** - Renders a poster-sized visualization of the results.

Inputs from A_Inputs: `poster_dims.xlsx`, which defines the key measurements for each element of the poster.

Inputs from B_Intermediates: `score_data.RData` and `heritage_sites.RData`, containing the results from the previous scripts.

Outputs to C_Outputs: `travel_suggestions_poster.pdf` and `travel_suggestions.png`.  Both files visualize the results in identical ways.  The PDF version provides a high-resolution vector-graphic version, while the PNG version provides a quicker-loading small file version.

### Project Status and To-Dos

More work could be done to improve this project but the skill-building goals inspiring it have already been thoroughly met.  I will probably re-run it when the US State Department takes down the COVID-related travel advisories and then call the project inactive.  If I were to continue improving it, my first priorities would be:

1. Converting all input datasets to draw data directly from the Internet, instead of relying on downloaded files.
2. Searching for higher quality datasets and investigating missing data further.
3. Conducting sensitivity analysis on the scoring process, especially the normalization and composite scoring components.
4. Building an interactive  scoring tool, so that users can make the results more closely reflect their travel priorities.

However, since my skill-building goals have really been thoroughly met, my remaining to-dos are:

- [ ] Make sure the poster, README, and gallery text are fully aligned.

- [ ] Proof-read and improve the text on the poster, gallery text, and this README file.

- [ ] Regenerate results after the State Department clears its COVID-related travel advisories.
