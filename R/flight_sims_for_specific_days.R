

#' @description flight sims using specified windows
#' @author Brian Maitner
#' @param quality_threshold the maximum fraction of cloudy pixels to accept within a flight box
#' @param max_wind_speed m/s
#' @param wind_speed_threshold maximum number of pixels exceeding threshold
#' @param use_julian If FALSE, dates will be matched by their day and month.  If TRUE, the julian day of the year will be used.

# Load packages and fxes

source("R/batch_extract.R")
library(tidyverse)
library(lubridate)
library(sf)
library(terra)



flight_sim_specific_days <- function(start_day = as_date("2023-10-23"),
                                     end_day = as_date("2023-12-04"),
                                     quality_threshold = 0.05, 
                                     max_wind_speed = 5,
                                     wind_speed_threshold = 0.1,
                                     use_julian = TRUE
                                     ){

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
    
    #Narrow down the full list of available start dates to those falling on the same day of the year
  
      all_dates  <- sort(base::intersect(unique(cloud_table$unix_date),unique(wind_table$unix_date)))
  
      all_dates <- data.frame(unix_date = all_dates) %>%
        mutate(ymd_date = as_date(unix_date),
               year = year(ymd_date),
               month = month(ymd_date),
               day = day(ymd_date),
               julian_day = yday(ymd_date)
               )
      
      if(use_julian){
        
        all_dates <-
        all_dates %>%
          filter(julian_day >= yday(start_day) &
                   julian_day <= yday(end_day))
        
      }else{
        
         
        stop("Brian, write this code")    
          
      }  

      
    start_dates <- all_dates %>%
      filter(julian_day == yday(start_day))
      
      
    
for(i in 1:nrow(start_dates)){
  
  #set start and end dates
  
  sampling_start <- start_dates$unix_date[i]
  
  sampling_end <- sampling_start + as.numeric(end_day-start_day)
  print(sampling_start)
  
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
    
    if(nrow(priority_d) == 0){
      
      message("nothing to sample this day")
      
      next

      } #if nothing matches the criteria, move along
    
    # load the wind data raster to get specific details  
    
    era_speed[[era_md$layer_id[which(era_md$unix_date == d)]]] %>%
      terra::extract(y = vect(boxes[priority_d$ID,]), cells=TRUE) -> focal_raster_data
    
    
    focal_raster_data %>%
      select(-ID) %>%
      select(-cell) %>%
      as.matrix() -> raster_matrix
    
    (raster_matrix > max_wind_speed) -> tf_matrix
    
    cell_matrix <- raster_matrix
    
    for(j in 1:nrow(cell_matrix)){
      
      cell_matrix[j,] <- focal_raster_data$cell[j]
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
  
  
  
}# end I loop
  

    return(simulation_output)
    


}#end fx
