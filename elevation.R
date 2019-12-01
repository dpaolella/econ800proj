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
shapes <- shapes %>% 
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
elevation_list <- list()
#shapes_elev <- data.frame(get_elev_point(shapes[0:500,],prj = prj_dd, src = "epqs"))
i <- 0#501
while (i < nrow(shapes)/100) {
  tryCatch(
    expr = {
      temp <- data.frame(get_elev_point(shapes[(i*100):(i*100+100),], prj = prj_dd, src = "epqs"))
      elevation_list[[i+1]] <- temp
      i <- i + 1
      message("Appended round to list: ", i)
    },
    error = function(e){
      message("Caught an error on iteration: ", i)
    }
)
}

shapes_elev = data.table::rbindlist(elevation_list)
shapes_elev %>% distinct()
# Group by route 
route_elev <- shapes_elev %>% 
  arrange(shape_id, pt_sequence) %>%
  mutate(delta = elevation - lag(elevation, default = first(elevation)))

write.csv(route_elev, paste(dirname(current_path),"/data/bus/elevation.csv", sep = ""))

elev_sum <- route_elev %>%
  group_by(shape_id) %>%
  summarise(Positive = sum(delta[delta>0]), Negative = sum(delta[delta <0]))


write.csv(elev_sum, paste(dirname(current_path),"/data/bus/elevationsummary.csv", sep = ""))

boxplot(elev_sum['Positive'], col="#69b3a2", ylab = "Meters", main = "Elevation Gain by Route (Meters)")



