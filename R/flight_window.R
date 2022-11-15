#The goal with this script is to assess how successful our sampling would be if we were to run it in the past.


# Algorithm thoughts:

  # Initialize an empty output file
  # iterate through all the day in the time series:
      # if there is an X day period following that day, continue.  else, skip
      # rank the sites to be sampled in decreasing order of mean cloud cover over time
        # sample the highest ranking (== hardest to sample) site that is below 10% cloud cover that day
          #if none, list NA
        # record the box ID, cloud cover
        # repeat until all sites are sampled
        # process to next day in time series


########################################################################################

library(tidyverse)
library(lubridate)

#Set up

  # Download table from drive (to see the code underlying this or to update the data, see the file "R/mock_flights_earth_engine.R")
    googledrive::drive_download(file = "EMMA/cloud_stats.csv",
                                path = "data/test_cloud_stats.csv",
                                overwrite = TRUE) #note: if dling more than one need to add a prefix or something

    cloud_table <- read.csv("data/test_cloud_stats.csv")
    
    time_window <- 42 # the number of consecutive days you will be flying
    
    quality_threshold <- 0.01 #the maximum fraction of cloudy pixels to accept within a flight box
    
    cloud_table %>%
      mutate(unix_date = as.numeric(as_date(date))) -> cloud_table
    
    cloud_table %>%
      group_by(id,date,target,unix_date)%>%
      summarize(mean = mean(na.omit(mean))) -> cloud_table
    
    # Rank boxes by mean cloud cover (will be used to determine priority)
    
      cloud_table %>%
        group_by(id) %>%
        summarise(mean_cc = mean(na.omit(mean)))%>%
        mutate(rank = row_number(mean_cc)) -> priorities
      
    
    
# Simulate sampling
    
    simulation_output <- NULL
    
    for(i in 1:length(unique(cloud_table$unix_date))){
      
      #set start and end dates
      
        sampling_start <- unique(cloud_table$unix_date)[i]
        
        sampling_end <- sampling_start + time_window - 1
      
      #check whether there is date for 90% of the  week window
        
        cloud_table %>%
          dplyr::select(unix_date)%>%
          unique()%>%
          filter(unix_date %in% sampling_start:sampling_end)%>%
          nrow() -> sampling_days
      
      #don't bother if there aren't enough days to sample
      
        if(sampling_days < (0.9 * time_window) ){next}
      
      
      # start simulation for the 6 week period
        
        out_d <- data.frame(start_date = sampling_start,
                            end_date = sampling_end,
                            date = sampling_start:sampling_end,
                            box_id = NA,
                            cloud_cover = NA)
        
        
        for(d in sampling_start:sampling_end){
          
          #Make sure there are no more than 20 options (sanity check)
          
            cloud_table %>%
              filter(unix_date == d)%>%
              nrow() -> daily_options
            
            if(daily_options > 20){stop("Check code")}
          
          
          #Get a list of sites to be sampled

            potential_sites <- priorities$id[which(!priorities$id %in% out_d$box_id)]
          
          # Pull the cloud data

            cloud_table %>%
              filter(unix_date == d) %>% #filter to date
              filter(mean <= quality_threshold) %>% #filter by threshold
              filter(id %in% potential_sites) %>% #toss sites that have already been done
              left_join(priorities) %>% #combine with prioritizaton
              slice_max(order_by = rank, n = 1) -> priority_d
            
            if(nrow(priority_d)==0){next} #if nothing matches the criteria, move along
            
          # record the data
            out_d$box_id[which(out_d$date==d)] <-priority_d$id
            out_d$cloud_cover[which(out_d$date==d)] <-priority_d$mean
            
            
            #head(cloud_table)    
          
        }# days loops
      
      simulation_output %>%
        bind_rows(out_d) -> simulation_output
      
      
      
    }
    
    
    #check on warning message
    # In out_d$box_id[which(out_d$date == d)] <- priority_d$id :
    #   number of items to replace is not a multiple of replacement length
    # 48: In out_d$cloud_cover[which(out_d$date == d)] <- priority_d$mean :
    
    
#####################################################################
    #saveRDS(object = simulation_output,file = "data/temp/sim_output_01pct.RDS")
  #saveRDS(object = simulation_output,file = "data/temp/sim_output_05pct.RDS")
  #saveRDS(object = simulation_output,file = "data/temp/sim_output_10pct.RDS")    
    
  # Digging into simulation output
    
  head(simulation_output)  
    
  simulation_output %>%
      group_by(start_date)%>%
    summarize()
    
    simulation_output %>%
      #filter(start_date==11229)%>%
      na.omit()%>%
      group_by(start_date)%>%
      summarise(sites_done = sum(!is.na(box_id)),
                mean_cc = mean(na.omit(cloud_cover)),
                days_taken = max(date)-min(start_date)+1)->sim_summary

    
    
    hist(sim_summary$days_taken)
    hist(sim_summary$sites_done)
    unique(sim_summary$sites_done)
    
    simulation_output %>%
      na.omit()%>%
      group_by(start_date)%>%
      summarise(sites_done = sum(!is.na(box_id)),
                mean_cc = mean(na.omit(cloud_cover)),
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
                mean_cc = mean(na.omit(cloud_cover)),
                days_taken = max(date)-min(start_date)+1)%>%
      mutate(day_char = as_date(start_date))%>%
      mutate(year = year(day_char),
             month = month(day_char),
             day = day(day_char),
             day_of_year = yday(day_char))%>%
      ggplot(mapping = aes(x=day_of_year,y=mean_cc))+
      geom_line()+
      facet_wrap(~year)

    
    head(sim_summary)
    
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
    
    
    
    
    
    