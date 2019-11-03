library('tidytransit')
library('dplyr')
library('lubridate')
library('tidyr')

# Reference: https://developers.google.com/transit/gtfs/reference/

ct <- read_gtfs("https://www.cttransit.com/sites/default/files/gtfs/googlect_transit.zip")

# Find beginning, end, and duration of trips
tripTime <- ct$stop_times %>%
  group_by(trip_id) %>%
  filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence)) %>%
  ## Check if arrival and departure times are different (only 1 is)
  # mutate(stop_duration = hms(arrival_time) - hms(departure_time)) %>%
  mutate(key = ifelse(stop_sequence == min(stop_sequence), "start", "end"), distance = max(shape_dist_traveled)) %>%
  select(trip_id, distance, arrival_time, key) %>%
  spread(key, arrival_time) %>%
  mutate(trip_duration = period_to_seconds(hms(end)) - period_to_seconds(hms(start)))

write.csv(tripTime, "tripTime.csv")

routes <- ct$trips %>%
  left_join(tripTime, by = "trip_id") %>%
  group_by(route_id, service_id) %>%
  mutate(dwell_minutes = 1440 - (max(period_to_seconds(hms(end))) - min(period_to_seconds(hms(start)))) / 60, driving_minutes = sum(trip_duration/60), total_dist = sum(distance))

write.csv(routes, "routes.csv")
