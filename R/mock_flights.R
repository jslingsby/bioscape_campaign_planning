# Notes

# earth engine cloud analysis for the mock planning
  # e.g. which boxes could be covered given daily cloud conditions.
  # we'll probably need to look at a daily weather satellite - can't use landsat because it's not frequent enough.



#########################################

# Part 1: ERA

library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(ggplot2)
source("R/batch_extract.R")

#domain
  domain <- st_read("data/output/domain.gpkg")

# grab just the total cloud cover
  era_terra <- terra::rast(x = "data/manual_downloads/flight_planning/adaptor.mars.internal-1666724031.1916814-7142-3-6ad9112b-c8b2-4bdd-a3d4-1a6b593bd447.nc",
                           subds="tcc")
  
               
# get flight boxes
  boxes <- st_read("data/manual_downloads/BIOSCAPE_proposed/20221026_flightboxes.gpkg") %>%
            st_transform(crs = crs(era_terra))

# extract the cloud cover for flight boxes  
  
  era_clouds <-
    batch_extract(x = era_terra,
                   y = vect(boxes),
                  batch_size = 1000,
                   exact = TRUE)


# convert to long format and add date/time

  era_clouds %>%
    pivot_longer(cols = starts_with("tcc"))%>%
    inner_join(y = bind_cols(name = names(era_terra),
                             time = time(era_terra)))%>%
    mutate(year = year(time),
           month = month(time),
           day = day(time),
           hour = hour(time))-> era_clouds

# weighted_mean
  
  era_clouds %>%
    group_by(ID,time) %>%
    summarise(wgt_mean_cc = weighted.mean(x = value,w = fraction))%>%
    mutate(year = year(time),
           month = month(time),
           day = day(time),
           hour = hour(time),
           doy = yday(time)) %>%
    mutate(cont_doy = doy + (hour/24)) ->era_cloud_weighted
  


era_cloud_weighted %>%
  group_by(ID,cont_doy)%>%
  summarize(mean_cc = mean(wgt_mean_cc))%>%
  ggplot() +
  geom_tile(mapping = aes(x = cont_doy,
                          y = ID,
                          fill = mean_cc))

era_cloud_weighted %>%
  group_by(ID,doy)%>%
  summarize(mean_cc = mean(wgt_mean_cc))%>%
  ggplot() +
  geom_tile(mapping = aes(x = doy,
                          y = ID,
                          fill = mean_cc))+
  scale_fill_gradient(low = "sky blue",high = "white")
  


era_cloud_weighted %>%
  group_by(ID,doy,year)%>%
  summarize(max_cc = max(wgt_mean_cc))%>%
  ggplot() +
  geom_tile(mapping = aes(x = doy,
                          y = ID,
                          fill = max_cc))+
  scale_fill_gradient(low = "sky blue",high = "white")+
  facet_wrap(~year)


era_cloud_weighted %>%
  group_by(ID,doy,year)%>%
  summarize(max_cc = max(wgt_mean_cc))%>%
  mutate(binary_clouds = dplyr::if_else(max_cc <= .05,true = 0,false = 1))%>%
  ggplot() +
  geom_tile(mapping = aes(x = doy,
                          y = ID,
                          fill = binary_clouds))+
  scale_fill_gradient(low = "sky blue",high = "white")+facet_wrap(~year)


era_cloud_weighted %>%
  group_by(ID, hour)%>%
  summarise(mean_cc = mean(wgt_mean_cc))%>%
  ggplot()+
  geom_line(mapping = aes(x=hour,y = mean_cc))+
  facet_wrap(~ID)


era_cloud_weighted %>%
  group_by(ID, hour)%>%
  ggplot()+
  geom_point(mapping = aes(x=hour,y = wgt_mean_cc),alpha=0.01)+
  geom_smooth(mapping = aes(x=hour,y=wgt_mean_cc),method = "loess")+
  facet_wrap(~ID)

boxes$ID <- 1:20

era_cloud_weighted %>%
  group_by(ID)%>%
  summarize(mean_cc = mean(wgt_mean_cc))%>%
  inner_join(x = boxes)%>%
  ggplot(mapping = aes(fill = mean_cc))+
  geom_sf()+
  geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
  scale_fill_gradient(low = "sky blue",high = "white")+
  geom_sf_text(aes(label = round(mean_cc,digits = 2)))
  

era_cloud_weighted %>%
  group_by(ID, month)%>%
  summarize(mean_cc = mean(wgt_mean_cc))%>%
  inner_join(x = boxes)%>%
  ggplot(mapping = aes(fill = mean_cc))+
  geom_sf()+
  geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
  scale_fill_gradient(low = "sky blue",high = "white")+
  facet_wrap(~month)+
  geom_sf_text(aes(label = round(mean_cc,digits = 2)))

# number of days

era_cloud_weighted %>%
  group_by(ID,doy,month,year)%>%
  summarize(max_cc = max(wgt_mean_cc))%>%
  mutate(binary_clouds = dplyr::if_else(max_cc <= .1,true = 0,false = 1))%>%
  group_by(ID, month)%>%
  summarize(prop_cloud_cover = sum(binary_clouds)/n(),
            cloud_days = sum(binary_clouds),
            total_days = n())%>%
  inner_join(x = boxes)%>%
  ggplot(mapping = aes(fill = prop_cloud_cover))+
  geom_sf()+
  geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
  scale_fill_gradient(low = "sky blue",high = "white",limits=c(0,1))+
  geom_sf_text(aes(label = round(prop_cloud_cover,digits = 2)))+
  facet_wrap(~month)


era_cloud_weighted %>%
  group_by(ID,doy,month,year)%>%
  summarize(max_cc = max(wgt_mean_cc))%>%
  mutate(binary_clear = dplyr::if_else(max_cc <= .1,true = 1,false = 0))%>%
  group_by(ID, month)%>%
  summarize(prop_clear = sum(binary_clear)/n(),
            clear_days = sum(binary_clear),
            total_days = n())%>%
  inner_join(x = boxes)%>%
  ggplot(mapping = aes(fill = prop_clear))+
  geom_sf()+
  geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
  scale_fill_gradient(low = "white",high = "sky blue",limits=c(0,1))+
  geom_sf_text(aes(label = round(prop_clear,digits = 2)))+
  facet_wrap(~month)








