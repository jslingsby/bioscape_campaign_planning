# Notes

# Wind analyses based on ERA5 data

#########################################

stop("do this")
# cutoff of 5m/s
# perhaps use medians instead of means
# 90th percentile daily wind

# Part 1: ERA

library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(spatstat.core)
source("R/batch_extract.R")

# calculating wind speed from u and v components of wind
  # https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398
  # (v^2 + u^2)^0.5

#domain
  domain <- st_read("data/output/domain.gpkg")

# get wind data
  
  era_u <- terra::rast(x = "data/manual_downloads/flight_planning/adaptor.mars.internal-1666724031.1916814-7142-3-6ad9112b-c8b2-4bdd-a3d4-1a6b593bd447.nc",
                           subds = "u10")
  
  era_v <- terra::rast(x = "data/manual_downloads/flight_planning/adaptor.mars.internal-1666724031.1916814-7142-3-6ad9112b-c8b2-4bdd-a3d4-1a6b593bd447.nc",
                       subds = "v10")
  
  era_speed <- ((era_u^2)+(era_v^2))^.5
  
  writeRaster(x = mean(era_speed),
              filename = "data/output/mean_era5_speed.tif")
  
# get flight boxes
  boxes <- st_read("data/manual_downloads/BIOSCAPE_proposed/20221026_flightboxes.gpkg") %>%
            st_transform(crs = crs(era_speed))

# extract the cloud cover for flight boxes  
  
  era_wind_boxes <-
    batch_extract(x = era_speed,
                   y = vect(boxes),
                  batch_size = 1000,
                   exact = TRUE)


  length(colnames(era_wind_boxes))
  colnames(era_wind_boxes)[which(FALSE==grepl(pattern = "u10",x =colnames(era_wind_boxes)))]
  
  head(colnames(era_wind_boxes))

  
# convert to long format and add date/time

  era_wind_boxes %>%
    pivot_longer(cols = starts_with("u10"))%>%
    inner_join(y = bind_cols(name = names(era_speed),
                             time = time(era_speed)))%>%
    mutate(year = year(time),
           month = month(time),
           day = day(time),
           hour = hour(time))-> era_wind_boxes

# weighted_mean
  
  era_wind_boxes %>%
    group_by(ID, time) %>%
    summarise(wgt_mean_wind_speed = weighted.mean(x = value, w = fraction),
              wgt_median_wind_speed = weighted.median(x = value,w = fraction),
              wgt_90pct_wind_speed = weighted.quantile(x = value,
                                                       w = fraction,
                                                       probs = c(0.9),
                                                       type = 1)) %>%
    mutate(year = year(time),
           month = month(time),
           day = day(time),
           hour = hour(time),
           doy = yday(time)) %>%
    mutate(cont_doy = doy + (hour/24)) -> era_wind_weighted
  
  #save the table
  saveRDS(object = era_wind_weighted,
          file = "data/output/era_wind_weighted.RDS")  

era_wind_weighted %>%
  group_by(ID,cont_doy)%>%
  summarize(mean_speed = mean(wgt_mean_wind_speed))%>%
  ggplot() +
  geom_tile(mapping = aes(x = cont_doy,
                          y = ID,
                          fill = mean_speed))

era_wind_weighted %>%
  group_by(ID,doy)%>%
  summarize(mean_speed = mean(wgt_mean_wind_speed))%>%
  ggplot() +
  geom_tile(mapping = aes(x = doy,
                          y = ID,
                          fill = mean_speed))+
  scale_fill_gradient(low = "sky blue",high = "white")
  


era_wind_weighted %>%
  group_by(ID,doy,year)%>%
  summarize(max_speed = max(wgt_mean_wind_speed))%>%
  ggplot() +
  geom_tile(mapping = aes(x = doy,
                          y = ID,
                          fill = max_speed))+
  scale_fill_gradient(low = "sky blue",high = "white")+
  facet_wrap(~year)


era_wind_weighted %>%
  group_by(ID, hour)%>%
  summarise(mean_wind_speed = mean(wgt_mean_wind_speed))%>%
  ggplot()+
  geom_line(mapping = aes(x=hour,y = mean_wind_speed))+
  facet_wrap(~ID)


era_wind_weighted %>%
  group_by(ID, hour)%>%
  summarise(max_wind_speed = max(wgt_mean_wind_speed))%>%
  ggplot()+
  geom_line(mapping = aes(x=hour,y = max_wind_speed))+
  facet_wrap(~ID)


era_wind_weighted %>%
  group_by(ID, hour)%>%
  ggplot()+
  geom_point(mapping = aes(x=hour,y = wgt_median_wind_speed),alpha=0.01)+
  geom_smooth(mapping = aes(x=hour,y=wgt_median_wind_speed),method = "loess")+
  facet_wrap(~ID)+
  ylab("Median Wind Speed")


era_wind_weighted %>%
  group_by(ID, hour)%>%
  ggplot()+
  geom_point(mapping = aes(x=hour,y = wgt_90pct_wind_speed),alpha=0.01)+
  geom_smooth(mapping = aes(x=hour,y=wgt_90pct_wind_speed),method = "loess")+
  facet_wrap(~ID)+
  ylab("90th Percentile Wind Speed")

era_wind_weighted$wgt_90pct_wind_speed


boxes$ID <- 1:20

era_wind_weighted %>%
  group_by(ID)%>%
  summarize(mean_wind_speed = mean(wgt_mean_wind_speed))%>%
  inner_join(x = boxes)%>%
  ggplot(mapping = aes(fill = mean_wind_speed))+
  geom_sf()+
  geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
  scale_fill_gradient(low = "sky blue",high = "white")+
  geom_sf_text(aes(label = round(mean_wind_speed,digits = 2)))
  

era_wind_weighted %>%
  group_by(ID, month)%>%
  summarize(mean_wind_speed = mean(wgt_mean_wind_speed))%>%
  inner_join(x = boxes)%>%
  ggplot(mapping = aes(fill = mean_wind_speed))+
  geom_sf()+
  geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
  scale_fill_gradient(low = "sky blue",high = "white")+
  facet_wrap(~month)+
  geom_sf_text(aes(label = round(mean_wind_speed,digits = 2)))

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








