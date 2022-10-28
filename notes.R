
#set up

# docker run -d -e DISABLE_AUTH=true -p 1999:8787 -v C:/Users/"Brian Maitner"/Desktop/current_projects/terrestrial_sampling:/home/rstudio/terrestrial_sampling adamwilsonlab/emma:latest

# docker run -u 0 -d -e DISABLE_AUTH=true -p 1999:8787 -v C:/Users/"Brian Maitner"/Desktop/current_projects/terrestrial_sampling:/home/rstudio/terrestrial_sampling adamwilsonlab/emma:latest

#docker exec -u 0 -it mycontainer bash

docker exec -itu 0 hungry_engelbart test

docker exec -itu rstudio hungry_engelbart test

##########################################




  # do we have afromontane forests in focal polygons?

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

