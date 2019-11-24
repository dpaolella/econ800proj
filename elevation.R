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

shapes <- read_csv(paste(dirname(current_path),"/data/bus/shapes.txt", sep = ""))


# Join route geometries to block groups 
routeGeom <- shapes %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS
routeGeom <- st_transform(routeGeom, 2234)

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
