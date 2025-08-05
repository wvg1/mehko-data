library(tidygeocoder)
library(tidyverse)
library(tigris)
library(sf)
library(rlang)
library(readxl)

#make sure to update path to relevant xlsx file
complaint_data <- read_excel("~/Desktop/MEHKO/mehko_data.xlsx")

#loading in RUCA data
ruca_data <- read_csv("~/Desktop/MEHKO/RUCA2010zipcode.csv")

#categorize RUCA zones
ruca_data <- ruca_data %>%
  mutate(ruca_level = case_when(
    RUCA1 <= 1 ~ "Urban",
    RUCA1 <= 6 ~ "Suburban",
    TRUE       ~ "Rural"
  ))

#join RUCA to permit data
complaint_data <- complaint_data %>%
  mutate(zip = as.character(zip),
         zip = str_extract(as.character(zip), "^\\d+"),
         zip = str_pad(zip, width = 5, pad = "0")) %>%
  left_join(ruca_data, by = c("zip" = "ZIP_CODE"))

#geocode permit data
complaint_data <- complaint_data %>%
  mutate(full_address = paste(address, city, state, zip, sep = ", ")) %>%
  geocode(address = full_address, method = "osm", lat = latitude_osm, long = longitude_osm) %>%
  geocode(address = full_address, method = "census", lat = latitude_cen, long = longitude_cen)

geocoding_failed <- complaint_data %>%
  filter(is.na(latitude_osm) | is.na(longitude_osm)) %>%
  filter(is.na(latitude_cen) | is.na(longitude_cen))

complaint_data <- complaint_data %>%
  mutate(latitude = if_else(is.na(latitude_osm), latitude_cen, latitude_osm)) %>%
  mutate(longitude = if_else(is.na(longitude_osm), longitude_cen, longitude_osm))

#convert to spatial format
tracts <- tracts(state = "CA", year = 2020, cb = TRUE)
tracts <- st_transform(tracts, crs = 4326)

complaint_data_spatial <- complaint_data %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

complaint_data_spatial <- st_join(complaint_data_spatial, tracts, join = st_within)

complaint_data <- complaint_data_spatial %>%
  st_drop_geometry() %>%
  mutate(census_tract = GEOID)

#loading in nces data, and align with coordinate system from complaint data
nces_data <- st_read("~/Desktop/MEHKO/edge_locale24_nces_CA/edge_locale24_nces_CA.shp")
nces_data <- nces_data %>%
  st_transform(nces_data, crs = 4326)
nces_data <- st_make_valid(nces_data)
complaint_data_with_nces_locale <- st_join(complaint_data_spatial, nces_data, left = TRUE)
complaint_data_with_nces_locale <- complaint_data_with_nces_locale %>%
  mutate(locale_description = case_when(
    LOCALE == 11 ~ "City: Large",
    LOCALE == 12 ~ "City: Midsize",
    LOCALE == 13 ~ "City: Small",
    LOCALE == 21 ~ "Suburb: Large",
    LOCALE == 22 ~ "Suburb: Midsize",
    LOCALE == 23 ~ "Suburb: Small",
    LOCALE == 31 ~ "Town: Fringe",
    LOCALE == 32 ~ "Town: Distant",
    LOCALE == 33 ~ "Town: Remote",
    LOCALE == 41 ~ "Rural: Fringe",
    LOCALE == 42 ~ "Rural: Distant",
    LOCALE == 43 ~ "Rural: Remote",
    TRUE ~ "Unknown"
  ))

#loading in opportunity zone data
opp_zones <- st_read("~/Desktop/MEHKO/8764oz.shp")
st_crs(opp_zones)
opp_zones <- st_transform(opp_zones, crs = 4326)

#load HUD QCT data
qct_data <- read_csv("~/Desktop/MEHKO/QCT2023.csv")
qct_data <- qct_data %>%
  mutate(tract = as.character(tract))

#add QCT data including QCT flag
complaint_data <- complaint_data %>%
  left_join(qct_data, by = c("census_tract" = "tract")) %>%
  mutate(is_qct = census_tract %in% qct_data$fips)

#how many permits and what percent are in QCTs by jurisdiction
#how many permits and what percent are in opportunity zones by jurisdiction
#how many permits are in different RUCAs (3 categories)
#how many permits are in different RUCAs (4 categories)

#identify ruca levels for specific addresses
#506 = tres fuegos, 510 = warung rie rie, 615 = smoke & peppers, 381 = pampas empanadas, 441 = carnitas valdez, 337 = cooking by benz

complaint_data %>%
  filter(mehko_id %in% c(506,510,615,381,384,441,337)) %>%
  select(mehko_id, ruca_level, RUCA1, RUCA2)
