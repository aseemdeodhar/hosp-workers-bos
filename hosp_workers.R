# This analysis is to try and identify hospital shift workers by analysing usage statistics from
# the Transit App.

# loading required packages:
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
# Loading required data:

bos <- read_sf("shapefiles/i95_tracts/i95_tracts.shp")
hosp <- read_sf("shapefiles/hospitals/HOSPITALS_PT.shp")

hosp <- st_transform(hosp, crs=4269)
bos <- st_transform(bos, crs=4269)


# Plotting these two shapes together:

boshosp <- st_filter(hosp, bos)
bos_1 <- st_join(bos, boshosp)

ggplot()+geom_sf(data = bos)+geom_sf(data = boshosp)+
  theme_minimal()+
  ggtitle(label = "Hospital Locations in Inner Boston", 
          subtitle = "Optimizing Bus Routes for Boston's Healthcare workers")

# Join the hospitals dataset to their respective census block groups
ggplot()+geom_sf(data = bos_1, aes(fill = BEDCOUNT))+
  theme_minimal()+
  ggtitle("CBGs with Hospitals & Bedcount")


# Get all work locations from Transit App usage data.
# Create a subset of device_ids for work locations falling within hospital tracts

# Loading all favorite work locations data:

work_locs <- read.csv("work_locs.csv", stringsAsFactors = FALSE)

work_locs <- work_locs %>% select(device_id, address, favorite_type, latitude, longitude, locality) %>% 
  distinct() %>% 
  filter(latitude < 43.3192 & latitude > 41.0484) %>% 
  filter(longitude < -69.799 & longitude > -72.0842)

work_locs <- st_as_sf(work_locs, coords = c("longitude", "latitude"), crs = 4269)

# We get a subset of block groups which contain hospitals

hosp_blocks <- filter(bos_1, BEDCOUNT >= 1)

# Subsetting the work locations file to get only those locations which fall within block groups
# which contain hospitals

workhosp <- st_filter(work_locs, hosp_blocks)

ggplot()+
  geom_sf(data = bos_1)+
  geom_sf(data = workhosp)

length(unique(workhosp$device_id))
bos_hospworks_locs <- st_join(bos, workhosp)

# Get list of unique device IDs. Get the home saved locations of these device IDs

uq_hosp_workers <- workhosp %>% select(device_id, address, favorite_type, geometry) %>% unique()

ggplot()+
  geom_sf(data = bos_1)+
  geom_sf(data = uq_hosp_workers)


uq_hosp_workers$work_GEOID <- bos_hospworks_locs$GEOID[match(uq_hosp_workers$device_id, 
                                                          bos_hospworks_locs$device_id)]

ggplot(data = uq_hosp_workers, aes(x = work_GEOID))+geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust= -.5, size = 3)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ggtitle("CBGs with Hospitals, & Worker Count",
          subtitle = "through App data")+
  theme_minimal()

# Work CBG Counts:

work_cbg <- st_join(bos, uq_hosp_workers)

work_cbg_counts <- work_cbg %>% 
  group_by(GEOID) %>% 
  count()

ggplot()+geom_sf(data = work_cbg_counts, aes( fill = log(n)))+
  ggtitle("Work Locations of Hospital Workers")+
  theme_minimal()

######################################################################################

# loading Boston area data and filtering out duplicates:

home_locs <- read.csv("home_locs.csv", stringsAsFactors = FALSE)

home_locs <- home_locs %>% select(device_id, address, favorite_type, latitude, longitude, locality) %>% 
  distinct() %>% 
  filter(latitude < 43.3192 & latitude > 41.0484) %>% 
  filter(longitude < -69.799 & longitude > -72.0842)

home_locs <- st_as_sf(home_locs, coords = c("longitude", "latitude"), crs = 4269)

# inner join home_locs to work_locs to get a subset of home_locations with corresponding deviceIDs

home_hosp <- inner_join(home_locs %>% as.data.frame() %>% select(device_id, 
                                                                 address, 
                                                                 favorite_type,
                                                                 locality,
                                                                 geometry), 
                        uq_hosp_workers %>% as.data.frame() %>% select(device_id) , 
                        by = "device_id")

home_hosp <- st_as_sf(home_hosp, sf_column_name = "geometry")

uq_home_hosp <- home_hosp %>%  unique()

uq_home_hosp$work_GEOID <- bos_hospworks_locs$GEOID[match(uq_home_hosp$device_id, 
                                                          bos_hospworks_locs$device_id)]

ggplot()+geom_sf(data = bos)+geom_sf(data = uq_home_hosp)

# We now have the home locations of all the device_ids who's work location is in the 
# CBGs of hospitals.
# Our next goal is to group these home locations by their respective work location's CBG.
# Why? So we can guess where each hospital's workers live.

home_cbg <- st_join(bos, uq_home_hosp)

home_cbg$work_GEOID <- bos_hospworks_locs$GEOID[match(home_cbg$device_id, 
                                                      bos_hospworks_locs$device_id)]

# get a count of 'homes' in each represented CBG
home_cbg_counts <- home_cbg %>% 
                      group_by(GEOID) %>% 
                      count()

ggplot()+geom_sf(data = home_cbg_counts, aes( fill = n))+
  ggtitle("Home Locations of Hospital Workers")+
  theme_minimal()

summary(as.factor(uq_home_hosp$work_GEOID))

ggplot()+
  geom_sf(data = bos_1)+
  geom_sf(data = uq_home_hosp, aes(color = work_GEOID), size = 1)+
  ggtitle("Home locations grouped by CBG of hospital location")+
  theme_minimal()

ggplot(data = home_cbg_counts, aes(x = GEOID))+geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust= -.5, size = 3)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ggtitle("CBGs with Hospitals, & Worker Count",
          subtitle = "through App data")+
  theme_minimal()

ggplot()+geom_sf(data = uq_home_hosp, aes(color = work_GEOID))

uq_home_hosp %>% group_by(work_GEOID)

# Reading in favorite lines data to attach to selected device_ids

uq_devices <- as.data.frame(unique(uq_home_hosp$device_id))

gid_lines <- read.csv("gid_rt_id.csv", stringsAsFactors = FALSE)

favlines <- read.csv("fav_lines.csv", stringsAsFactors = FALSE)

x <- left_join(favlines %>% select(device_id, global_route_id), gid_lines %>% select(global_route_id, service_type, route_id),
               by = "global_route_id") %>% drop_na()

y <- left_join(x,
               uq_home_hosp %>% as.data.frame() %>% select(device_id, work_GEOID),
                       by = "device_id") %>% drop_na() %>%  distinct()


#y <- left_join(x, gid_lines %>% select(global_route_id, service_type, route_id),
#               by = "global_route_id") %>% drop_na()

# Seeing which lines have max usage.

ggplot()+geom_bar(data = y, aes(x = route_id, fill = work_GEOID))+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(legend.position = "none")


# compare this data with general users database:

ggplot()+geom_bar(data = y, aes(x = route_id, fill = work_GEOID))+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(legend.position = "none")

ggplot()+geom_bar(data = x, aes(x = route_id))+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  theme(legend.position = "none")

summary(as.factor(y$route_id))

pop_routes <- y %>% group_by(work_GEOID, route_id) %>% 
  select(route_id, work_GEOID) %>% 
  count() %>%
  arrange(desc(n))

top5pop_routes_groups <- pop_routes %>% group_by(work_GEOID) %>% top_n(5)

top5pop_routes_overall <- pop_routes %>% top_n(5)

top5pop_routes_overall <- y %>% group_by(route_id) %>% 
  select(route_id) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  head(20)




# reading in bus+subway spatial data:

bus_shp <- read_sf("shapefiles/mbtabus/MBTABUSROUTES_ARC.shp")

bus_sample <- bus_shp %>% 
  filter(!grepl("inbound",ROUTE_DESC))

bus_sample <- bus_shp %>% as.data.frame() %>%
  mutate(CTPS_ROUTE = as.character(CTPS_ROUTE)) %>% 
  group_by(CTPS_ROUTE) %>% slice(1) %>% st_as_sf(sf_column_name = "geometry")

asdf <- left_join(top5pop_routes,
                   bus_sample %>% as.data.frame(),
                   by = c( "route_id" = "MBTA_ROUTE")) %>% drop_na() %>% 
  select("route_id", "n", "SHAPE_ID", "SHAPE_LEN", "geometry")
  
asdf <- st_as_sf(asdf, sf_column_name = "geometry")


ggplot()+
  geom_sf(data = bos_1, aes(fill = BEDCOUNT))+
  theme_minimal()+
  geom_sf(data = asdf, aes(color = n,))+
  ggtitle("Bus Routes")+
  theme_minimal()

################################################################################

top5pop_routes_overall_shp <- left_join(top5pop_routes_overall,
                                        bus_sample %>% as.data.frame(),
                                        by = c( "route_id" = "CTPS_ROUTE")) %>% drop_na() %>% 
  select("route_id", "n", "SHAPE_ID", "SHAPE_LEN", "geometry") %>% 
  st_as_sf(sf_column_name = "geometry")

ggplot()+
  geom_sf(data = bos_1, aes(fill = BEDCOUNT))+
  theme_minimal()+
  geom_sf(data = top5pop_routes_overall_shp, aes(color = n,), size = 2)+
  geom_sf_label(data = top5pop_routes_overall_shp, aes(label = route_id))+
  ggtitle("Top 17 Bus Routes Overall")+
  scale_color_gradient(low = "green", high = "red")+
  theme_minimal()
