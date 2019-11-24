library(tidytransit)
library(tidyverse)
library(lubridate)
library(rstudioapi)
library(sf)
# library(raster)
library(spData)
library(tmap)
library(stringr)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

'%ni%' <- Negate('%in%')

# Reference: https://developers.google.com/transit/gtfs/reference/

stop_times <- read_csv(paste(dirname(current_path),"/data/bus/stop_times.txt", sep = ""))
# Remove trips without arrival time info
stop_times_nona <- stop_times[!is.na(stop_times$arrival_time), ]
trips <- read_csv(paste(dirname(current_path),"/data/bus/trips.txt", sep = ""))
shapes <- read_csv(paste(dirname(current_path),"/data/bus/shapes.txt", sep = ""))

# Find beginning, end, and duration of trips
tripTime <- stop_times_nona %>%
  group_by(trip_id) %>%
  filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence)) %>%
  ## Check if arrival and departure times are different (only 1 is)
  # mutate(stop_duration = hms(arrival_time) - hms(departure_time)) %>%
  mutate(key = ifelse(stop_sequence == min(stop_sequence), "start", "end"), distance = max(shape_dist_traveled*0.621371)) %>%
  dplyr::select(trip_id, distance, arrival_time, key) %>%
  spread(key, arrival_time) %>%
  mutate(trip_duration = period_to_seconds(hms(end)) - period_to_seconds(hms(start)))
tripTime <- tripTime[!is.na(tripTime$trip_duration), ]


write.csv(tripTime, paste(dirname(current_path),"/data/bus/tripTime.csv", sep = ""))

# Calculate route dwell time, daily driving time, and daily driving distance
## Note: multiple buses may be running the same route at the same time (see block_id)
routes <- trips %>%
  inner_join(tripTime, by = "trip_id") %>%
  group_by(service_id, block_id) %>%
  mutate(start_seconds = min(period_to_seconds(hms(start))), end_seconds = max(period_to_seconds(hms(end))), dwell_minutes = 1440 - (max(period_to_seconds(hms(end))) - min(period_to_seconds(hms(start)))) / 60, total_driving_minutes = sum(trip_duration/60), total_miles = sum(distance)) %>% 
  distinct(service_id, block_id, start_seconds, end_seconds, dwell_minutes, total_driving_minutes, total_miles)

routes$service_id <- recode(routes$service_id, "1"="wk", "10"="wk", "14"="wk", "22"="wk", 
                            "6"="wk", "11"="sat", "15"="sat", "2"="sat", "23"="sat", "7"="sat",
                            "12"="sun", "16"="sun", "24"="sun", "3"="sun", "8"="sun",
                            "13"="none", "17"="none", "25"="none", "4"="none", "9"="none")
# Remove rows that have no service on any day
routes <- routes %>% filter(service_id!="none")

write.csv(routes, paste(dirname(current_path),"/data/bus/routes.csv", sep = ""))

# Join route geometries to block groups 
routeGeom <- shapes %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS
routeGeom <- st_transform(routeGeom, 2234)

ct = st_read(paste(dirname(current_path), "/data/Census_Race/nad83/blockgroupct_37800_0000_2010_s100_census_1_shp_nad83_feet.shp", sep = ""))

routeGeom <- st_join(routeGeom, ct["GEOID10"])

# Join air pollution data to routes
air <- read_csv(paste(dirname(current_path),"/data/air/uwc1572557891185ca36ca1813cf38da15acced115e1f976.csv", sep = ""))
no2 <- air %>%
  filter(pollutant=="no2") %>%
  dplyr::select(fips, pred_wght) %>%
  rename(no2 = pred_wght)

routeGeom <- routeGeom %>%
  left_join(no2, by = c("GEOID10" = "fips"))
  
# Join population data to routes
pop <- read_csv(paste(dirname(current_path),"/data/Census_Race/2017_blck_grp.csv", sep = ""))
pop <- pop %>%
  dplyr::select(STATEA, COUNTYA, TRACTA, BLKGRPA, AHZAE001, AHZAE003) %>%
  mutate(GISJOIN = paste(STATEA, COUNTYA, TRACTA, BLKGRPA, sep = ""),
         white = as.numeric(AHZAE003), 
         nonwhite = as.numeric(AHZAE001) - as.numeric(AHZAE003), 
         totalpop = as.numeric(AHZAE001),
         pctnonwhite = nonwhite/totalpop) %>%
  dplyr::select(GISJOIN, totalpop, white, nonwhite, pctnonwhite)  

routeGeom <- routeGeom %>%
  left_join(pop, by = c("GEOID10" = "GISJOIN"))

# Calculate population-weighted average no2 by route geometry
routeNO2 <- routeGeom %>%
  mutate(no2Xpop = no2*totalpop) %>%
  dplyr::select(shape_id, totalpop, no2Xpop) %>%
  group_by(shape_id) %>%
  summarize_at(vars(-geometry), funs(sum)) %>%
  mutate(avgNO2 = no2Xpop / totalpop) %>%
  arrange(desc(avgNO2)) %>%
  mutate(airpolRank = 1:length(unique(routeGeom$shape_id)))

# Calculate population-weighted average nonwhite fraction by route geometry
routeNW <- routeGeom %>%
  mutate(pctnwXpop = pctnonwhite*totalpop) %>%
  dplyr::select(shape_id, totalpop, pctnwXpop) %>%
  group_by(shape_id) %>%
  summarize_at(vars(-geometry), funs(sum)) %>%
  mutate(avgpctNW = pctnwXpop / totalpop) %>%
  arrange(desc(avgpctNW)) %>%
  mutate(pctNWRank = 1:length(unique(routeGeom$shape_id)))

# Calculate population-weighted no2 by block group
popNO2 <- ct %>%
  dplyr::select(GEOID10) %>%
  left_join(pop, by = c("GEOID10" = "GISJOIN")) %>%
  left_join(no2, by = c("GEOID10" = "fips"))

tm_shape(popNO2) + tm_fill(col = "pctnonwhite")
tm_shape(popNO2) + tm_fill(col = "no2")

# Import and stack locations of vulnerable populations
childcare <- read_csv(paste(dirname(current_path),"/data/locationData/Child_Day_Care_Centers_And_Group_Day_Care_Homes.csv", sep = "")) %>%
  mutate(type="childcare", location=Location, name=Name) %>%
  distinct(Name, Address, .keep_all = TRUE) %>%
  dplyr::select(type, name, location)
hospitals <- read_csv(paste(dirname(current_path),"/data/locationData/Connecticut_Acute_Care_Hospitals.csv", sep = "")) %>%
  mutate(type="hospitals", location=Address, name=`Hospital Name`) %>%
  distinct(`Hospital Name`, Town, .keep_all = TRUE) %>%
  dplyr::select(type, name, location)
schools <- read_csv(paste(dirname(current_path),"/data/locationData/Education_Directory.csv", sep = "")) %>%
  mutate(type="schools", location=Location, name=`School Name`) %>%
  filter(`Organization Type` %ni% c("Public School Districts", "State Agency Facilities", "State Agencies", "Regional Education Service Center School Districts")) %>%
  distinct(Address, Town, .keep_all = TRUE) %>%
  dplyr::select(type, name, location)
seniorcenters <- read_csv(paste(dirname(current_path),"/data/locationData/Listing_of_Senior_Centers.csv", sep = "")) %>%
  mutate(type="seniorcenters", location=`Location 1`, name=Agency) %>%
  distinct(Agency, ST, .keep_all = TRUE) %>%
  dplyr::select(type, name, location)
nursing <- read_csv(paste(dirname(current_path),"/data/locationData/Nursing_Facility_Registry.csv", sep = "")) %>%
  mutate(type="nursing", location=`Location 1`, name=`FACILITY NAME`) %>%
  distinct(`FACILITY NAME`, `TOWN`, .keep_all = TRUE) %>%
  dplyr::select(type, name, location)
vulnLocs <- rbind(childcare, hospitals, schools, seniorcenters, nursing) %>%
  mutate(id=1:n(), coords=str_remove_all(str_extract(location, "(\\([:digit:].*[:digit:]\\))"), "[()]")) %>%
  separate(coords, c("lat", "lon"), sep = ", ") %>%
  st_as_sf(coords = c("lon", "lat")) %>% # set coordinates
  st_set_crs(4326) %>% # set geographic CRS
  st_transform(2234) %>%
  dplyr::select(-location)


# Convert route points to route lines
routePoints<- shapes %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% # set coordinates
  st_set_crs(4326) %>% # set geographic CRS
  arrange(shape_id, shape_pt_sequence) %>%
  mutate(group = match(shape_id, unique(shape_id)))
routePoints <- st_transform(routePoints, 2234)

for (i in unique(routePoints$group)) {
  x_coords = st_coordinates(filter(routePoints, group==i))[,1]
  y_coords = st_coordinates(filter(routePoints, group==i))[,2]
  m = matrix(append(x_coords, y_coords), ncol=2)
  linestring = st_cast(st_multipoint(m), "LINESTRING")
  if (i==1) {
    routeLines <- st_sf(geom = st_sfc(linestring, crs = 2234))
  }
  if (i>1) {
    routeLines <- rbind(routeLines,st_sf(geom = st_sfc(linestring, crs = 2234)))
  }
}

routeLines <- routeLines %>%
  mutate(group=1:length(routeLines$geom)) %>%
  cbind(unique(routePoints$shape_id)) %>%
  rename(shape_id=unique.routePoints.shape_id.)

routes_vulnLocs_join <- routeLines %>%
  st_join(vulnLocs, join=st_is_within_distance, dist=1640.42) # within distance of 500 meters

# Tally up nearby vulnerable locations for each route
routes_vulnLocs_tally <- routes_vulnLocs_join %>%
  group_by(shape_id, type) %>%
  tally() %>%
  spread(type, n) %>%
  replace_na(list(childcare=0, hospitals=0, nursing=0, schools=0, seniorcenters=0)) %>%
  select(shape_id, childcare, hospitals, nursing, schools, seniorcenters) %>%
  mutate(total=childcare+hospitals+nursing+schools+seniorcenters)
