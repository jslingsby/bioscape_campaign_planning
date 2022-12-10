#The goal with this script is to assess how successful our sampling would be if we were to run it in the past.


# Algorithm thoughts:

  # Initialize an empty output file
  # iterate through all the day in the time series:
    # if there is an X day period following that day, continue.  else, skip
    # rank the sites to be sampled in terms of decreasing wind speed (aquatic only), then decreasing order of mean cloud cover over time
      # sample the highest ranking (== hardest to sample) site that is below 10% cloud cover that day (and where >90% is less than 5m/s wind speed if aquatic)
        #if none, list NA
      # record the box ID, cloud cover, median wind speed, number of pixels above cloud cover, number of pixels above wind speed
      # repeat until all sites are sampled
      # process to next day in time series


########################################################################################


# Load packages and fxes

source("R/batch_extract.R")
library(tidyverse)
library(lubridate)
library(sf)
library(terra)

#Set up

# Download table from drive (to see the code underlying this or to update the data, see the file "R/mock_flights_earth_engine.R")
  googledrive::drive_download(file = "EMMA/cloud_stats.csv",
                              path = "data/test_cloud_stats.csv",
                              overwrite = TRUE) #note: if dling more than one need to add a prefix or something

  cloud_table <- read.csv("data/test_cloud_stats.csv")
  
  cloud_table %>%
    mutate(unix_date = as.numeric(as_date(date))) -> cloud_table
  
  cloud_table %>%
    group_by(id,date,target,unix_date)%>%
    summarize(mean = mean(na.omit(mean))) -> cloud_table
  
# Get wind speed data
  
  era_u <- terra::rast(x = "data/manual_downloads/flight_planning/adaptor.mars.internal-1666724031.1916814-7142-3-6ad9112b-c8b2-4bdd-a3d4-1a6b593bd447.nc",
                       subds = "u10")
  
  era_v <- terra::rast(x = "data/manual_downloads/flight_planning/adaptor.mars.internal-1666724031.1916814-7142-3-6ad9112b-c8b2-4bdd-a3d4-1a6b593bd447.nc",
                       subds = "v10")
  
  era_speed <- ((era_u^2)+(era_v^2))^.5
  
  rm(era_u,era_v)
  
  era_md <- data.frame(layer_id = 1:nlyr(era_speed),
                       time = time(era_speed)) %>%
    mutate(date = date(time),
           unix_date =  as.numeric(date))

# get flight boxes

  boxes <- st_read("data/manual_downloads/BIOSCAPE_proposed/20221026_flightboxes.gpkg") %>%
    st_transform(crs = crs(era_speed))
  
  boxes$id <- 1:nrow(boxes)
  
  wind_table <- readRDS("data/output/era_wind_weighted.RDS")
  
  wind_table %>%
    mutate(unix_date = as.numeric(as_date(paste(year,month,day)))) -> wind_table
  
# simulation settings  
    
  time_window <- 42 # the number of consecutive days you will be flying

  quality_threshold <- 0.10 #the maximum fraction of cloudy pixels to accept within a flight box
  
  max_wind_speed <- 5 #m/s
  
  wind_speed_threshold <- 0.1 #maximum number of pixels exceeding threshold


# Rank boxes by mean cloud cover (will be used to determine priority)

  cloud_table %>%
    group_by(id) %>%
    summarise(mean_cc = mean(na.omit(mean)))%>%
    mutate(cloud_rank = row_number(mean_cc)) -> priorities

  wind_table %>%
    group_by(id=ID) %>%
    summarise(med_wind = median(na.omit(wgt_median_wind_speed)))%>%
    mutate(wind_rank = row_number(med_wind))%>%
    inner_join(priorities) -> priorities

  cloud_table %>%
    ungroup()%>%
    dplyr::select(id,target) %>%
    unique() %>%
    inner_join(priorities) -> priorities
  
  priorities$target <- gsub(pattern = "Terrestrial (coincident aquatic)",
                            replacement = "Terrestrial",
                            x =  priorities$target,fixed = TRUE)
  
  
  # aquatic are ranked first by order of wind and cloud cover
    priorities %>%
      filter(target == "Aquatic")%>%
      dplyr::arrange(med_wind, mean_cc) %>%
      mutate(overall_rank =1:n() + 
               (length(unique(priorities$id)) - n())
             ) -> aqua_priorities
    
  # terrestrial are ranked only by cloud cover
    priorities %>%
      filter(target == "Terrestrial")%>%
      dplyr::arrange(mean_cc) %>%
      mutate(overall_rank = 1:n())%>% 
      dplyr::bind_rows(aqua_priorities)%>%
      dplyr::select(-mean_cc)%>%
      dplyr::select(-med_wind)-> priorities
    
    rm(aqua_priorities)


# Simulate sampling

  simulation_output <- NULL
  
  for(i in 1:length(sort(base::intersect(unique(cloud_table$unix_date),unique(wind_table$unix_date))))){
    
    #set start and end dates
    
    sampling_start <- sort(base::intersect(unique(cloud_table$unix_date),unique(wind_table$unix_date)))[i]
    
    sampling_end <- sampling_start + time_window - 1
    
    #check whether there are data for 90% of the time window
    
      cloud_table %>%
        dplyr::select(unix_date)%>%
        unique()%>%
        filter(unix_date %in% sampling_start:sampling_end)%>%
        pull(unix_date) -> sampling_days_cloud
      
      wind_table %>%
        ungroup() %>%
        dplyr::select(unix_date)%>%
        unique()%>%
        filter(unix_date %in% sampling_start:sampling_end) %>%
        pull(unix_date) -> sampling_days_wind
    
     sampling_days <- length(base::intersect(sampling_days_wind, sampling_days_cloud))
    
    #don't bother if there aren't enough days to sample
    
    if(sampling_days < (0.9 * time_window) ){next}
    
    
    # start simulation for the 6 week period
    
      out_d <- data.frame(start_date = sampling_start,
                          end_date = sampling_end,
                          date = sampling_start:sampling_end,
                          box_id = NA,
                          mean_cloud_cover = NA,
                          median_wind_speed = NA,
                          prop_cells_above_wind_threshold = NA,
                          cells_above_wind_threshold = NA,
                          comments = NA)
    
    
      for(d in sampling_start:sampling_end){
        
        #Get a list of sites to be sampled
        
        potential_sites <- priorities$id[which(!priorities$id %in% out_d$box_id)]
        
        # Pull the cloud data
        
            # sanity check
              cloud_table %>%
                filter(unix_date == d)%>%
                nrow() -> daily_options
            
              if(daily_options > 20){stop("Check code")}
              
            # Enforce maximum of 6 consecutive days flying rule
              
              if(d > sampling_start){
                
                out_d %>%
                  filter(date %in% (d-6):(d-1)) %>%
                  summarize(fract_week_worked = sum(!is.na(mean_cloud_cover))/6) -> previous_week_summary
                
                if(previous_week_summary$fract_week_worked == 1){
                  
                  
                  out_d$comments[which(out_d$date == d)] <- "7th day restriction"
                  
                  next
                  
                }
              }

        # Filter on cloud cover first
              
          cloud_table %>%
            filter(unix_date == d) %>% #filter to date
            filter(mean <= quality_threshold) %>% #filter by cloud threshold
            rename(mean_cloud_cover = mean) %>%
            filter(id %in% potential_sites) -> cloud_priority  #toss sites that have already been done
        
        #add in wind data and filter 
          wind_table %>%
            filter(unix_date == d) %>%
            group_by(ID,unix_date) %>%
            summarize(med_wind_speed = median(wgt_median_wind_speed)) %>%
            inner_join(cloud_priority,
                       by = c("ID"="id","unix_date")) %>%
              filter(!(target == "Aquatic" & med_wind_speed > max_wind_speed)) %>%
            dplyr::select(-target)%>%
          left_join(priorities, 
                    by = c("ID"="id")) %>%  #combine with prioritizaton
            ungroup()%>% #need to ungroup or the slice won't work properly
          slice_max(order_by = overall_rank, n = 1) -> priority_d

        if(nrow(priority_d) == 0){next} #if nothing matches the criteria, move along
          
        # load the wind data raster to get specific details  
          
          era_speed[[era_md$layer_id[which(era_md$unix_date == d)]]] %>%
          terra::extract(y = vect(boxes[priority_d$ID,]), cells=TRUE) -> focal_raster_data
                
          
          focal_raster_data %>%
            select(-ID) %>%
            select(-cell) %>%
            as.matrix() -> raster_matrix
          
          (raster_matrix > max_wind_speed) -> tf_matrix

            cell_matrix <- raster_matrix
            
            for(i in 1:nrow(cell_matrix)){
              
              cell_matrix[i,] <- focal_raster_data$cell[i]
            }

          focal_raster_data %>%
            select(-ID,-cell) %>%
            unlist() %>%
            as.numeric() -> raster_vals

        # record the data
          out_d$box_id[which(out_d$date == d)] <- priority_d$ID
          out_d$mean_cloud_cover[which(out_d$date == d)] <- priority_d$mean_cloud_cover
          out_d$median_wind_speed[which(out_d$date == d)] <- priority_d$med_wind_speed
          out_d$prop_cells_above_wind_threshold[which(out_d$date == d)] <- length(which(raster_vals > max_wind_speed))/length(raster_vals)
          out_d$cells_above_wind_threshold[which(out_d$date == d)] <- paste(cell_matrix[tf_matrix],collapse = ",")

        #head(cloud_table)    
        
      }# days loops
    
    simulation_output %>%
      bind_rows(out_d) -> simulation_output
    
    
  
}



#####################################################################
#saveRDS(object = simulation_output,file = "data/temp/sim_output_01_cloud_5ms_wind.RDS")
#saveRDS(object = simulation_output,file = "data/temp/sim_output_05_cloud_5ms_wind.RDS")
#saveRDS(object = simulation_output,file = "data/temp/sim_output_10_cloud_5ms_wind.RDS")
  
# Digging into simulation output

simulation_output %>%
  #filter(start_date==11229)%>%
  na.omit()%>%
  group_by(start_date)%>%
  summarise(sites_done = sum(!is.na(box_id)),
            mean_cc = mean(na.omit(mean_cloud_cover)),
            median_wind = median(na.omit(median_wind_speed)),
            days_taken = max(date)-min(start_date)+1) -> sim_summary


hist(sim_summary$days_taken)
hist(sim_summary$sites_done)
unique(sim_summary$sites_done)

simulation_output %>%
  na.omit()%>%
  group_by(start_date) %>%
  summarise(sites_done = sum(!is.na(box_id)),
            mean_cc = mean(na.omit(mean_cloud_cover)),
            median_wind = median(na.omit(median_wind_speed)),
            days_taken = max(date)-min(start_date)+1)%>%
  mutate(day_char = as_date(start_date))%>%
  mutate(year = year(day_char),
         month = month(day_char),
         day = day(day_char),
         day_of_year = yday(day_char))%>%
  ggplot(mapping = aes(x=day_of_year,y=days_taken))+
  geom_line()+
  facet_wrap(~year)

simulation_output %>%
  na.omit()%>%
  group_by(start_date)%>%
  summarise(sites_done = sum(!is.na(box_id)),
            mean_cc = mean(na.omit(mean_cloud_cover)),
            median_wind = median(na.omit(median_wind_speed)),
            days_taken = max(date)-min(start_date)+1)%>%
  mutate(day_char = as_date(start_date))%>%
  mutate(year = year(day_char),
         month = month(day_char),
         day = day(day_char),
         day_of_year = yday(day_char))%>%
  ggplot(mapping = aes(x=day_of_year,y=mean_cc))+
  geom_line()+
  facet_wrap(~year)

simulation_output %>%
  na.omit()%>%
  group_by(start_date)%>%
  summarise(sites_done = sum(!is.na(box_id)),
            mean_cc = mean(na.omit(mean_cloud_cover)),
            median_wind = median(na.omit(median_wind_speed)),
            days_taken = max(date)-min(start_date)+1)%>%
  mutate(day_char = as_date(start_date))%>%
  mutate(year = year(day_char),
         month = month(day_char),
         day = day(day_char),
         day_of_year = yday(day_char))%>%
  ggplot(mapping = aes(x=day_of_year,y=median_wind))+
  geom_line()+
  facet_wrap(~year)

head(sim_summary)

  readRDS("data/temp/sim_output_10_cloud_5ms_wind.RDS")%>%  
  na.omit()%>%
  group_by(start_date)%>%
  summarise(sites_done = sum(!is.na(box_id)),
            mean_cc = mean(na.omit(mean_cloud_cover)),
            median_wind = median(na.omit(median_wind_speed)),
            days_taken = max(date)-min(start_date)+1)%>%
  ungroup()%>%
  mutate(day_of_year = yday(as_date(start_date)),
         year = year(as_date(start_date)))%>%
  group_by(day_of_year)%>%
  summarise(q0 = quantile(days_taken,probs=0),
            q25 = quantile(days_taken,probs=0.05),
            q50 = quantile(days_taken,probs=0.5),
            q75 = quantile(days_taken,probs=0.95),
            q1 = quantile(days_taken,probs=1)
  ) %>%
  mutate(date = as.Date(day_of_year,origin="2022-12-31"))%>%
  mutate(start_of_month = floor_date(date,unit = "month"),
         month_label = month(start_of_month,label = TRUE),
         julian_label = yday(start_of_month),
         day_of_month = mday(date))->test

test %>%    
  ggplot()+
  geom_ribbon(aes(ymin=q0,ymax=q1, x = day_of_year),col="grey",alpha=0.2)+
  geom_ribbon(aes(ymin=q25,ymax=q75, x = day_of_year),col="grey",alpha=0.5)+
  geom_line(aes(x=day_of_year,y=q50))+
  geom_hline(yintercept = 37,lty=2)+
  scale_x_continuous(breaks = test$julian_label,
                     labels = test$month_label,
                     minor_breaks = test$day_of_year)+ #note: it seems like it should be possible to inherit these
  ylab("Flight Days Needed")+
  xlab("Campaign Starting Day")+
  geom_text(label = "estimated total number of flight days",
            y=37.3,
            x=280)


#Make x-axis actual date (for 2023)
#add label to horizontal line saying "estimated total number of flight days"  


#readRDS("data/temp/sim_output_10pct.RDS")%>%
readRDS("data/temp/sim_output_05pct.RDS")%>%
  #readRDS("data/temp/sim_output_01pct.RDS")%>%  
  na.omit()%>%
  group_by(start_date)%>%
  summarise(sites_done = sum(!is.na(box_id)),
            mean_cc = mean(na.omit(cloud_cover)),
            days_taken = max(date)-min(start_date)+1)%>%
  ungroup()%>%
  mutate(day_of_year = yday(as_date(start_date)),
         year = year(as_date(start_date)))%>%
  group_by(day_of_year)%>%
  summarise(q0 = quantile(days_taken,probs=0),
            q25 = quantile(days_taken,probs=0.05),
            q50 = quantile(days_taken,probs=0.5),
            q75 = quantile(days_taken,probs=0.95),
            q1 = quantile(days_taken,probs=1)
  ) %>%
  mutate(date = as.Date(day_of_year,origin="2022-12-31"))%>%
  mutate(start_of_month = floor_date(date,unit = "month"),
         month_label = month(start_of_month,label = TRUE),
         julian_label = yday(start_of_month),
         day_of_month = mday(date),
         md_label = paste(month_label,"-",day_of_month,sep = ""))->test

test %>%    
  ggplot()+
  geom_ribbon(aes(ymin=q0,ymax=q1, x = day_of_year),col="grey",alpha=0.2)+
  geom_ribbon(aes(ymin=q25,ymax=q75, x = day_of_year),col="grey",alpha=0.5)+
  geom_line(aes(x=day_of_year,y=q50))+
  geom_hline(yintercept = 37,lty=2)+
  scale_x_continuous(breaks = data$day_of_year,
                     labels = test$md_label)+ #note: it seems like it should be possible to inherit these
  ylab("Flight Days Needed")+
  xlab("Campaign Starting Day")+
  geom_text(label = "estimated total number of flight days",
            y=37.3,
            x=280)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#####################

library(ggplot2)
library(tidyverse)

#saveRDS(object = simulation_output,file = "data/temp/sim_output_10pct.RDS")    
sim_10pct <- readRDS("data/temp/sim_output_10pct.RDS")





