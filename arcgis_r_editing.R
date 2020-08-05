# part editing for ArcGIS based data

# This analysis is to try and identify hospital shift workers by analysing usage statistics from
# the Transit App.

# loading required packages:
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)

# Get all work locations from Transit App usage data.
# Create a subset of device_ids for work locations falling within hospital tracts

# Loading all favorite work locations data: ####
work_hosp <- st_read("hosp_workers_locs/hosp_workers_locs.shp")

# work_locs <- read.csv("work_locs.csv", stringsAsFactors = FALSE)
# 
# work_locs <- work_locs %>% select(device_id, address, favorite_type, latitude, longitude, locality) %>% 
#   distinct() %>% 
#   filter(latitude < 43.3192 & latitude > 41.0484) %>% 
#   filter(longitude < -69.799 & longitude > -72.0842)
# 
# work_locs <- st_as_sf(work_locs, coords = c("longitude", "latitude"), crs = 4269)

# Loading all favorite home locations data: ####

home_locs <- read.csv("home_locs.csv", stringsAsFactors = FALSE)

home_locs <- home_locs %>% select(device_id, address, favorite_type, latitude, longitude, locality) %>%
  filter(latitude < 43.3192 & latitude > 41.0484) %>% 
  filter(longitude < -69.799 & longitude > -72.0842) %>% 
  distinct()

home_locs <- st_as_sf(home_locs, coords = c("longitude", "latitude"), crs = 4269)

# Loading Massachusetts Region hospital data:
# hosp <- read_sf("shapefiles/hospitals/HOSPITALS_PT.shp")
# 
# hosp <- hosp %>% select(IDNUMBER, NAME, SHORTNAME, BEDCOUNT, geometry)
# 
# hosp <- st_transform(hosp, crs=4269)

# Getting home locations of hospital workers:
home_hosp <- inner_join(home_locs %>% as.data.frame() %>% select(device_id, 
                                                                 address, 
                                                                 favorite_type,
                                                                 locality,
                                                                 geometry), 
                        work_hosp %>% as.data.frame() %>% select(device_id, SHORTNAME) , 
                        by = "device_id") %>% drop_na(SHORTNAME)

home_hosp <- st_as_sf(home_hosp, sf_column_name = "geometry")

uq_home_hosp <- home_hosp %>% unique()

ggplot()+geom_sf(data = uq_home_hosp, aes(color = SHORTNAME))

# Getting hospital locations, and identifying clusters:
hosp <- read_sf("shapefiles/hospitals/HOSPITALS_PT.shp")

hosp <- st_transform(hosp, crs=4269)

pop_hosps <- inner_join(hosp %>% as.data.frame(),
                        uq_home_hosp %>% as.data.frame() %>% select(SHORTNAME),
                        by = "SHORTNAME") %>% 
  distinct() %>% 
  st_as_sf(sf_column_name = "geometry")



pop_hosps$hosp_area <- ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2128"), "Worcester",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2841"), "Worcester",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2311"), "Brockton",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2114"), "Norwood",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2054"), "Needham-Newton",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2075"), "Needham-Newton",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2227"), "Dorchester",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2003"), "Dorchester",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2048"), "Faulkner B&W",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2099"), "Lawrence",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2007"), "Salem-Beverly",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2014"), "Salem-Beverly",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2008"), "Lynn",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2094"), "Winchester-Malden",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2058"), "Winchester-Malden",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2342"), "Burlington",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2085"), "Brighton",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2071"), "Cambridge",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2108"), "Cambridge",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2299"), "Tufts",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2059"), "NE Baptist",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2092"), "Longwood",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2335"), "Longwood",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2341"), "Longwood",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2139"), "Longwood",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2069"), "Longwood",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2307"), "BMC",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2084"), "BMC",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2316"), "Mass Gen", 
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2168"), "Mass Gen",
                              ifelse(str_detect(string = pop_hosps$IDNUMBER, pattern = "2167"), "Mass Gen", NA
                              )))))))))))))))))))))))))))))))
write_sf(pop_hosps, "pop_hosps/pop_hosps.shp")

# Joining with worker home locs to get hospital area attached to them:

uq_home_hosp <- left_join(uq_home_hosp %>% as.data.frame(),
                                             pop_hosps %>% as.data.frame() %>% select(SHORTNAME, hosp_area),
                                             by = "SHORTNAME") %>% 
  distinct() %>% 
  st_as_sf(sf_column_name = "geometry")

write_sf(uq_home_hosp, "hosp_worker_homes/hosp_worker_homes.shp", delete_layer = TRUE)


ggplot()+geom_sf(data = uq_home_hosp, aes(geometry = geometry, color = hosp_area))




# saving hospital worker home locations file:
write_sf(uq_home_hosp, "uq_home_hosp/uq_home_hosp.shp")

### Favorite bus/train routes:

bus_shp <- read_sf("shapefiles/mbtabus/MBTABUSROUTES_ARC.shp")
bus_shp <- bus_shp %>% as.data.frame() %>%
  mutate(CTPS_ROUTE = as.character(CTPS_ROUTE)) %>% 
  group_by(CTPS_ROUTE) %>% slice(1) %>% st_as_sf(sf_column_name = "geometry")

uq_devices <- uq_home_hosp %>% as.data.frame() %>% select(device_id) %>% unique()

gid_lines <- read.csv("gid_rt_id.csv", stringsAsFactors = FALSE)

favlines <- read.csv("fav_lines.csv", stringsAsFactors = FALSE)
favlines <- inner_join(favlines, gid_lines, by = "global_route_id")
hosp_favlines <- inner_join(uq_devices, favlines, by = "device_id") %>% unique()

hosp_favlines <- left_join(hosp_favlines %>% select(device_id, global_route_id), 
               gid_lines %>% select(global_route_id, service_type, route_id),
               by = "global_route_id") %>% drop_na()

# count of routes

route_counts <- hosp_favlines %>% group_by(route_id, global_route_id, service_type) %>% 
  select(route_id, global_route_id, service_type) %>% 
  count() %>%
  arrange(desc(n))

hosp_favlines_shp <- left_join(route_counts,
                               bus_sample %>% as.data.frame(),
                               by = c( "route_id" = "CTPS_ROUTE")) %>% 
  drop_na() %>% 
  select("route_id", "n", "SHAPE_ID", "SHAPE_LEN", "geometry") %>% 
  st_as_sf(sf_column_name = "geometry")

write_sf(hosp_favlines_shp, "hosp_favlines_shp/hosp_favlines_shp.shp")