library(tidytransit)
library(tidyverse)
library(lubridate)
library(rstudioapi)
library(sf)
# library(raster)
library(spData)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

# Reference: https://developers.google.com/transit/gtfs/reference/

ctbus <- read_gtfs("https://www.cttransit.com/sites/default/files/gtfs/googlect_transit.zip")

# Find beginning, end, and duration of trips
tripTime <- ctbus$stop_times %>%
  group_by(trip_id) %>%
  filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence)) %>%
  ## Check if arrival and departure times are different (only 1 is)
  # mutate(stop_duration = hms(arrival_time) - hms(departure_time)) %>%
  mutate(key = ifelse(stop_sequence == min(stop_sequence), "start", "end"), distance = max(shape_dist_traveled)) %>%
  select(trip_id, distance, arrival_time, key) %>%
  spread(key, arrival_time) %>%
  mutate(trip_duration = period_to_seconds(hms(end)) - period_to_seconds(hms(start)))

write.csv(tripTime, paste(dirname(current_path),"/data/bus/tripTime.csv", sep = ""))

# Calculate route dwell time, daily driving time, and daily driving distance
routes <- ctbus$trips %>%
  left_join(tripTime, by = "trip_id") %>%
  group_by(route_id, service_id) %>%
  mutate(dwell_minutes = 1440 - (max(period_to_seconds(hms(end))) - min(period_to_seconds(hms(start)))) / 60, driving_minutes = sum(trip_duration/60), total_dist = sum(distance))

write.csv(routes, paste(dirname(current_path),"/data/bus/routes.csv", sep = ""))

# Join route geometries to block groups
routeGeom <- ctbus$shapes %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS
routeGeom <- st_transform(routeGeom, 2234)

ct = st_read(paste(dirname(current_path), "/data/Census_Race/nad83/blockgroupct_37800_0000_2010_s100_census_1_shp_nad83_feet.shp", sep = ""))

routeGeom <- st_join(routeGeom, ct["GEOID10"])

# Join air pollution data to routes
air <- read_csv(paste(dirname(current_path),"/data/air/uwc1572557891185ca36ca1813cf38da15acced115e1f976.csv", sep = ""))
no2 <- air %>%
  filter(pollutant=="no2") %>%
  select(fips, pred_wght) %>%
  rename(no2 = pred_wght)

routeGeom <- routeGeom %>%
  left_join(no2, by = c("GEOID10" = "fips"))
  
# Join population data to routes
pop <- read_csv(paste(dirname(current_path),"/data/Census_Race/ACS_17_5YR_B02001_with_ann.csv", sep = ""))
pop <- pop[-1,]
pop <- pop %>%
  select(GEO.id2, HD01_VD01) %>%
  mutate(totalpop = as.numeric(HD01_VD01))

routeGeom <- routeGeom %>%
  left_join(pop, by = c("GEOID10" = "GEO.id2"))

# Calculate population-weighted no2 by route geometry
routeNO2 <- routeGeom %>%
  mutate(no2Xpop = no2*totalpop) %>%
  select(shape_id, totalpop, no2Xpop) %>%
  group_by(shape_id) %>%
  summarize_at(vars(-geometry), funs(sum)) %>%
  mutate(avgNO2 = no2Xpop / totalpop)
