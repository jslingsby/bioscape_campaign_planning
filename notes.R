
#set up

# docker run -d -e DISABLE_AUTH=true -p 1999:8787 -v C:/Users/"Brian Maitner"/Desktop/current_projects/terrestrial_sampling:/home/rstudio/terrestrial_sampling adamwilsonlab/emma:latest

# docker run -u 0 -d -e DISABLE_AUTH=true -p 1999:8787 -v C:/Users/"Brian Maitner"/Desktop/current_projects/terrestrial_sampling:/home/rstudio/terrestrial_sampling adamwilsonlab/emma:latest

#docker exec -u 0 -it mycontainer bash

docker exec -itu 0 hungry_engelbart test

docker exec -itu rstudio hungry_engelbart test

##########################################

# To add to flight planning:

  # could you add some more intro information to the top of the new flight_planning website that would help someone looking at this for the first time to understand what we did?  
  #   
  #   1) add header with MODIS Cloud flags and a short description of where the cloud data come from (MOD09GA, etc.) and a map of ~3 days binary cloud cover to make it clear what the 'raw' data are.  These could be layers in the interactive map (see next point)
  # 2) move up the interactive map with google basemap showing boxes and polygons and a raster of mean total cloud cover (over 20 years during the oct-december  window).  This will show the overall cloud frequency patterns.  Add the box numbers as labels.
  # 3) Drop the Flight boxes graph because it's now redundant with above.
  # 4) For the cloud cover over time graph, add y axis label "Flight box ID" and update x axis to actual dates (using 2023 calendar is fine)
  # 5) Campaign simulations: add more details about the sampling algorithm - maybe as bullet points, e.g. a) rank boxes by mean cloudiness, b) on day 1, select the typically cloudiest box that's clear that day, c) repeat.  This was done as a moving window for each day within the 3-month candidate campaign period october-december.  This plot shows the range of days required to capture all boxes across 2000-2021.  
  # 
  # for 1) above, by 'header' I just mean a section with short paragraph.

  # 6) adjust box number: if they are essentially random, let's number them northwest to southeast or similar e.g. 11,10,15,14,20,18,2


library(sf)
library(terra)
domain <- st_read("temp/domain.gpkg")
focal_sites <- st_read("data/output/sampling_options.gpkg")
acc_sites <- st_read("data/output/acceptable_sites.shp")

plot(acc_sites)

acc_sites2 <- st_read("data/output/acceptable_sites.gpkg")
plot(acc_sites2,col="red")

acc_sites2

veg <- st_read("data/manual_downloads/VEGMAP2018_AEA_16082019Final/NVM2018_AEA_V22_7_16082019_final.shp")

colnames(veg)

unique(veg$BIOME_18)
unique(veg$Name_18)

veg %>% st_drop_geometry() -> vegdata

veg %>%
st_intersection(y = acc_sites)->acceptable_veg

veg %>%
  st_intersection(x = focal_sites) -> focal_veg

veg %>%
  st_intersection(y = domain)-> domain_veg


domain_veg %>%
  st_drop_geometry() -> domain_vegdata

focal_veg %>%
  st_drop_geometry() -> focal_vegdata

acceptable_veg %>%
  st_drop_geometry() -> acc_vegdata


domain_vegdata %>%
  dplyr::filter(grepl(pattern = "forest",x = Name_18 ,ignore.case = TRUE))%>%
  dplyr::pull(Name_18)%>%
  unique()

focal_vegdata %>%
  dplyr::filter(grepl(pattern = "forest",x = Name_18 ,ignore.case = TRUE))%>%
  dplyr::pull(Name_18)%>%
  unique()

acc_vegdata %>%
  dplyr::filter(grepl(pattern = "forest",x = Name_18 ,ignore.case = TRUE))%>%
  dplyr::pull(Name_18)%>%
  unique()

