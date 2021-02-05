#### Opportunities for Future Development

This project is a work in progress.  However, except for to-do item #1, I have already met my goals for this project as a practice exercise, so these to-dos are low priority.  To-dos (in priority order) include:

1. SCRIPT #6 - Develop an Rshiny tool so that users can generate customized recommendations.

2. PROJECT SUMMARY - Flesh out the README file with more detail; make sure it reflects the latest wave of improvements.

3. HARD CODING - Evaluate manual hard-coded interventions in some scripts (especially #5) to determine if better coding will be a worthwhile use of time.

4. PROOF READING - Proof-read visualization poster and README file.

### Repository Guide

This project consists of five .R scripts.

**1_scrape_website.R** - Refines data on State Department travel advisories and percentage of English-speakers in each country.  

+ Scrapes source data from the State Department's travel advisory RSS feed and from a table on the relevant Wikipedia^[TODO: Find a better source if worth the time] page.  Saves refined data to dos_advice.RData and speaks_english.RData in the B_Intermediates directory.

**2_compile_city_data.R** - Refines United Nations data on city population and world heritage site locations.

+ Reads in city_population.xlsx and world_heritage.xlsx data files from the A_Inputs directory and saves refined data to B_Intermediates as city_data.RData and heritage_sites.RData.

+ Queries Google's geocoding API to determine the geographic coordinates of the cities.  If options(load_cached_geocode) is set to TRUE (default), script use a cached version of that query, rather than re-querying the server.  The cache saves at B_Intermediates/geocode_cache.RData.  Google's API requires a key.  The script uses source() to register this key using a script not included in this repository.  You will need to replace this line with your own key to run this project.

**3_compile_country_data.R** - Refines country-level datasets, including United Nations' M49 geographic region codes, United Nations' Inequality-Adjusted Human Development Index countries scores and Freedom House's Human / Civil Rights scores.  Also merges these datasets and the datasets refined in the previous two scripts into a unified whole.

+ Reads in country_codes.xlsx, country_development.xlsx, and human_rights.xlsx from the A_Inputs directory, as well as dos_advice.RData, speaks_english.RData, heritage_sites.RDdata, city_data.RData from B_Intermediates.  Outputs B_Intermediates/processed_data.RData, which contains the final country_data, city_data, and heritage_sites datasets

+ Queries Google's geocoding API to generate standardized country names for the country-level datasets, so that the script can merge them.  If options(load_cached_geocode) is set to TRUE (default), script use a cached version of that query, rather than re-querying the server.  The cache is saved as B_Intermediates/countries_geocode_cache.RData.  Google's API requires a key.  The script uses source() to register this key using a script not included in this repository.  You will need to replace this line with your own key to run this project.

**4_score_countries.R** - Scores each country according to the criteria described in table 1; visualizes the results as the figure shown at the start of this document.

+ Reads in B_Intermediates/processed_data.RData and outputs B_Intermediates/score_data.RData.  Both are packages of the previous clean datasets, but score_data adds the results of the country selection process.

**5_render_results_amp.R** - Visualizes the results as the figure shown at the start of this document.

+ Reads in B_Intermediates/score_data.RData and outputs the suggested travel itinerary as a 2ft x 3ft poster PDF file visualization: C_Outputs/travel_suggestions_generic.pdf.  Also saves the underlying ggplot object to B_Intermediates/generic_map.RData (visualization ggplot object).

**6_travel_suggestion_tool.R** - NOT YET WRITTEN.  Tool will enable users to generate custom travel recommendations.

### Data Sources

See the "Sources" tab of each dataset in the A_Inputs directory.
