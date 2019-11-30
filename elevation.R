#install.packages(c("tidytransit","tidyverse","lubridate",
#                   "rstudioapi","sf","spData","tmap",
#                   "stringr","elevatr"))
library(elevatr)
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

#reorder so coordinates are first for elavatr
shapes %>% 
  rename(
    y = shape_pt_lat,
    x = shape_pt_lon ) %>%
  select(
    x, y, everything())

#convert to data frame so elevatr works
shapes <- data.frame(shapes)
# set projection
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#add elevation as "z" coordinate to points using USGS Elevation Point Query Service
#note: need to do it in batches so server doesn't time out and crash
i <- 0
while i < nrow(shapes):
  
shapes_elev <- get_elev_point(shapes, prj = prj_dd, src = "epqs")

# Convert lat/lons to points and set CRS
routeGeom <- shapes %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS
routeGeom <- st_transform(routeGeom, 2234)

# Convert route points to route lines
routePoints<- shapes %>% 
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% # set coordinates
  st_set_crs(4326) %>% # set geographic CRS
  arrange(shape_id, pt_sequence) %>%
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


