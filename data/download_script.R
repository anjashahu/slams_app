
# SET UP ---------------------------------------------------------------

# Load libraries
library(tidyverse)
library(rvest)
library(stringi)
library(stringr)

# DOWNLOAD WTA/ATP DATA ------------------------------------------------

# Get links for WTA and ATP csvs 
base_url <- "https://raw.githubusercontent.com/JeffSackmann/"
years <- 1968:(as.numeric(format(Sys.Date(), "%Y"))-1)
wta_urls <- rep(NA, length(years))
atp_urls <- rep(NA, length(years))
for (i in 1:length(years)) {
  wta_urls[i] <- paste0(base_url, "tennis_wta/master/wta_matches_", years[i], ".csv")
  atp_urls[i] <- paste0(base_url, "tennis_atp/master/atp_matches_", years[i], ".csv")
}

# Create function to read in WTA and ATP grand slam data
read_slam_data <- function(url){
  # Read in WTA or ATP match data 
  read_csv(url, 
           col_types = cols(
             tourney_id = col_character(),
             tourney_name = col_character(),
             draw_size = col_double(),
             tourney_level = col_character(),
             tourney_date = col_date(format = "%Y%m%d"),
             match_num = col_double(),
             winner_name = col_character(),
             winner_ioc = col_character(),
             loser_name = col_character(),
             loser_ioc = col_character(),
             round = col_character()
             )
           ) %>%
    # Filter down match data to just include grand slam matches
    filter(tourney_level == "G") %>% 
    # Select columns of interest 
    select(
      tourney_id, tourney_name, draw_size, tourney_level,
      tourney_date, match_num, winner_name, winner_ioc,
      loser_name, loser_ioc, round
    ) %>%
    # Standardize grand slam names 
    mutate(tourney_name = case_when(
      tourney_name == "Australian Championships" | tourney_name == "Australian Open" | 
        tourney_name == "Australian Open 2" | tourney_name == "Australian Chps." |
        tourney_name == "Australian Open-2" ~ "Australian Open",
      tourney_name == "French Open" | tourney_name == "Roland Garros" ~ "French Open",
      tourney_name == "Us Open" | tourney_name == "US Open" ~ "US Open",
      tourney_name == "Wimbledon" ~ "Wimbledon"
      )
    )
}

# Read in WTA and ATP grand slam data 
wta_slams <- map_df(wta_urls, read_slam_data) %>% 
  # Remove inaccurate row for nonexistent Q4 round
  filter(round != "Q4")
atp_slams <- map_df(atp_urls, read_slam_data) %>% 
  # Fix unknown (UNK) ioc codes
  mutate(loser_ioc = case_when(
    loser_name == "L Palman" ~ "URS",
    loser_name == "Luis Garcia" ~ "MEX",
    TRUE ~ loser_ioc
    )
  )

# DOWNLOAD COUNTRY MAPPING DATA ----------------------------------------

# Read in country codes data from wikipedia 
url <- "https://en.wikipedia.org/wiki/List_of_IOC_country_codes"
h <- read_html(url)
# Read in the first table of country codes
ioc_codes <- h %>% 
  # Select table with main codes from wikipedia site
  html_nodes("table") %>% 
  .[1] %>% 
  html_table %>% 
  .[[1]] %>% 
  # Set column names
  setNames(c("code", "country", "other_codes", "link")) %>% 
  # Select columns of interest
  select(code, country) %>%
  # Remove any accents
  mutate_if(is.character, ~stri_trans_general(., id = "Latin-ASCII")) %>%
  # Remove "bracketed six" pattern at the end of Chinese Tapei in country column
  mutate(country = str_replace(country, "\\[6\\]", "")) %>%
  # Remove ".mw-parser-output .monospaced{font-family:monospace,monospace}" pattern in front of AFG in code column
  mutate(code = str_replace(code, ".mw-parser-output .monospaced\\{font-family:monospace,monospace\\}", ""))
# Read in the second table with extra country codes
extra_ioc_codes <- h %>% 
  # Select table with extra codes from wikipedia site
  html_nodes("table") %>% 
  .[3] %>% 
  html_table %>% 
  .[[1]] %>% 
  # Set column names
  setNames(c("code", "country", "other_codes")) %>% 
  # Select columns of interest
  select(code, country)
# Bind together main and extra country codes data frames
ioc_codes <- ioc_codes %>% bind_rows(extra_ioc_codes)

# Check to see that all ioc codes in the WTA and ATP data frames are covered
unique(unique(c(wta_slams$winner_ioc, wta_slams$loser_ioc)) %in% ioc_codes$code)
unique(unique(c(atp_slams$winner_ioc, atp_slams$loser_ioc)) %in% ioc_codes$code)

# Download data connecting countries to their continent 
url <- "https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv"
country_by_continent <- read_csv(url) %>% 
  rename(continent = Continent, country = Country)

# Create country mapping data by combining ioc codes data with data connecting countries to their continent 
country_mapping <- ioc_codes %>% 
  # Join data connecting countries to their continent to ioc codes data
  left_join(country_by_continent, by = "country") %>%
  # Manually add information for countries missing continent data 
  mutate(continent = case_when(
    country == "Aruba" ~ "South America",
    country == "American Samoa" ~ "Oceania",
    country == "Bermuda" ~ "North America",
    country == "Burkina Faso" ~ "Africa",
    country == "Cayman Islands" ~ "North America",
    country == "Republic of the Congo" ~ "Africa",
    country == "Democratic Republic of the Congo" ~ "Africa",
    country == "Cook Islands" ~ "Oceania",
    country == "Czech Republic" ~ "Europe",
    country == "Federated States of Micronesia" ~ "Oceania",
    country == "The Gambia" ~ "Africa",
    country == "Great Britain" ~ "Europe",
    country == "Guam" ~ "Oceania",
    country == "Hong Kong, China" ~ "Asia",
    country == "Virgin Islands" ~ "North America",
    country == "British Virgin Islands" ~ "North America",
    country == "South Korea" ~ "Asia",
    country == "Kosovo" ~ "Europe",
    country == "North Macedonia" ~ "Europe",
    country == "Myanmar" ~ "Asia",
    country == "Palestine" ~ "Asia",
    country == "North Korea" ~ "Asia",
    country == "Puerto Rico" ~ "North America",
    country == "Russia" ~ "Europe",
    country == "Eswatini" ~ "Africa",
    country == "Chinese Taipei" ~ "Asia",
    country == "United States" ~ "North America",
    country == "Netherlands Antilles" ~ "North America",
    country == "Australasia" ~ "Oceania",
    country == "Bohemia" ~ "Europe",
    country == "British West Indies" ~ "North America",
    country == "United Team of Germany" ~ "Europe",
    country == "Unified Team" ~ "Europe",
    country == "West Germany" ~ "Europe",
    country == "East Germany" ~ "Europe",
    country == "Russian Empire" ~ "Europe",
    country == "Serbia and Montenegro" ~ "Europe",
    country == "Czechoslovakia" ~ "Europe",
    country == "Soviet Union" ~ "Europe",
    country == "Yugoslavia" ~ "Europe",
    country == "Mixed team" ~ "Europe",
    TRUE ~ continent
    )
  ) 

# WRITE CSVs -----------------------------------------------------------
write_csv(wta_slams, "data/cleaned/wta_slams_cleaned.csv")
write_csv(atp_slams, "data/cleaned/atp_slams_cleaned.csv")
write_csv(country_mapping, "data/cleaned/country_mapping_cleaned.csv")
