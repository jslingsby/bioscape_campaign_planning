# Load packages
  library(rgee)
  library(targets)
  library(sf)
  library(tidyverse)
  library(lubridate)

# install rgee python dependencies
  # rgee::ee_install()
  # rgee::ee_install_upgrade()
  # rgee::ee_check()

# Initialize rgee
  rgee::ee_Initialize(drive = TRUE)
  
#####################################################

# Load in data
  
  # get domain
    domain <- st_read("data/output/domain.gpkg")
    domain_sf <- domain
  
  # get flight boxes
    boxes <- st_read("data/flight_planning/20221026_flightboxes.gpkg")
    boxes$id <- 1:20 # need a unique ID to make things easier
    boxes_sf <- boxes
    ee_boxes <- sf_as_ee(x = boxes_sf)
  
  # Format the domain and boxes
    domain <- sf_as_ee(x = domain)
    domain <- domain$geometry()

    boxes <- sf_as_ee(x = boxes)
    boxes <- boxes$geometry()

    
# Combine layers 
    
  sen1 <- ee$ImageCollection('MODIS/061/MOD09GA')
  sen2 <- ee$ImageCollection('MODIS/006/MYD09GA')
    
  sens <- sen1$merge(sen2)
  rm(sen1, sen2)

# subset data to relevent days/dates
    
    filter_dates <- function(collection,datestart,datestop,daystart,daystop){
      getdates <- ee$Filter$date(datestart,datestop);
      getday <- ee$Filter$calendarRange(daystart,daystop,"day_of_year");
      return(ee$ImageCollection(collection)$filter(getdates)$filter(getday))
    }

    sens_filtered <-
      filter_dates(collection = sens,
                   datestart = "2000-01-01",
                   datestop = paste(format(Sys.time(), "%Y-%m-%d"),sep = ""),
                   daystart = 273,
                   daystop = 366)
    
# Convert the data into binary cloud data
  
    #Helper function to extract the values from specific bits
    # The input parameter can be a ee.Number() or ee.Image()
    # Code adapted from https://gis.stackexchange.com/a/349401/5160
      bitwiseExtract <- function(input, fromBit, toBit) {
        maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
        mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
        return(input$rightShift(fromBit)$bitwiseAnd(mask))
      }
  
    # function to convert QA to binary cloud cover
    
      get_binary_cloud <- function(img) {
        
        # Extract the NDVI band
        qa <- img$select("state_1km")
        
        # extract the relevant bits
        
        clouds <- bitwiseExtract(input = qa,fromBit = 0,toBit = 1)$eq(1)
        
        # set the date
        
        date <- img$date()$format('yyyy-MM-dd')
        
        clouds$set("date_char",date)
        
      }
  
      
    clouds_sens <- sens_filtered$map(get_binary_cloud)
    
# extract values for polygons

  # reducer function to allow mapping across layers
  
      get_polygon_stats <- function(img){
        
        date <- img$get("date_char")
        
        img$reduceRegions(
          collection = ee_boxes,
          reducer = ee$Reducer$mean(),
          scale = 1000 #native resolution is ~1 km, so no need to go finer than this
        )$map(function(x){ x$set("date",date) }) #sets the date for later use

      }

  cloud_stats <- clouds_sens$map(get_polygon_stats) # the extraction

  flat_stats <- cloud_stats$flatten() #flatter into a table
  
# Download table to drive

  flat_dl<-
    ee_table_to_drive(collection = flat_stats,
                      description = "cloud_stats",
                      folder = "EMMA",
                      timePrefix = FALSE,
                      fileFormat = "CSV",
                      selectors = c("id","date","mean","target"))
  
  
  flat_dl$start()#starts the processing
  ee_monitoring(flat_dl,max_attempts = 100000) #keeps track of progress
###################################################################################################################
  
# Download table from drive  
  googledrive::drive_download(file = "EMMA/cloud_stats.csv",
                              path = "data/test_cloud_stats.csv",
                              overwrite = TRUE) #note: if dling more than one need to add a prefix or something
  
  cloud_table <- read.csv("data/test_cloud_stats.csv")

  
# Parse date

  
  cloud_table %>%
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           day_of_year = yday(date)) -> cloud_table
  

# Plots

  
  #ID by day of year
  
    cloud_table %>%
      group_by(id,day_of_year)%>%
      ggplot() +
      geom_tile(mapping = aes(x = day_of_year,
                              y = id,
                              fill = mean))+
      scale_fill_gradient(low = "sky blue",
                          high = "white")+
      facet_wrap(~year)
    
    
    
    
  # mean cloud cover
    cloud_table %>%
      group_by(id)%>%
      summarize(mean_cc = mean(na.omit(mean)))%>%
      inner_join(x = boxes_sf)%>%
      ggplot(mapping = aes(fill = mean_cc))+
      geom_sf()+
      geom_sf(data = domain_sf,inherit.aes = FALSE,fill=NA)+
      scale_fill_gradient(low = "sky blue",high = "white")+
      geom_sf_text(aes(label = round(mean_cc,digits = 2)))
    
  # mean monthly cloud cover
    cloud_table %>%
      group_by(id, month)%>%
      filter(month != 9)%>%
      summarize(mean_cc = mean(na.omit(mean)))%>%
      inner_join(x = boxes_sf)%>%
      ggplot(mapping = aes(fill = mean_cc))+
      geom_sf()+
      geom_sf(data = domain_sf,
              inherit.aes = FALSE,fill=NA)+
      scale_fill_gradient(low = "sky blue",high = "white")+
      facet_wrap(~month)+
      geom_sf_text(aes(label = round(mean_cc,digits = 2)))
    
    
  # proportion of cloudy days (based on a threshold)  
    cloud_table %>%
      na.omit()%>%
      filter(month != 9)%>%
      group_by(id,day_of_year,month,year)%>%
      summarize(mean = mean(na.omit(mean)))%>%
      mutate(binary_clouds = dplyr::if_else(mean <= .1,true = 0,false = 1))%>%
      group_by(id, month)%>%
      summarize(prop_cloud_cover = sum(binary_clouds)/n(),
                cloud_days = sum(binary_clouds),
                total_days = n()) %>%
      inner_join(x = boxes_sf)%>%
      ggplot(mapping = aes(fill = prop_cloud_cover))+
      geom_sf()+
      geom_sf(data = domain_sf,inherit.aes = FALSE,fill=NA)+
      scale_fill_gradient(low = "sky blue",high = "white",limits=c(0,1))+
      geom_sf_text(aes(label = round(prop_cloud_cover,digits = 2)))+
      facet_wrap(~month)
    
  
    #prop clear days
    cloud_table %>%
      na.omit()%>%
      filter(month != 9)%>%
      mutate(binary_clear = dplyr::if_else(mean <= .1,true = 1,false = 0)) %>%
      group_by(id, month)%>%
      summarize(prop_clear = sum(binary_clear)/n(),
                clear_days = sum(binary_clear),
                total_days = n())%>%
      inner_join(x = boxes_sf)%>%
      ggplot(mapping = aes(fill = prop_clear))+
      geom_sf()+
      geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
      scale_fill_gradient(low = "white",high = "sky blue",limits=c(0,1))+
      geom_sf_text(aes(label = round(prop_clear,digits = 2)))+
      facet_wrap(~month)
  
    #prop clear days
    cloud_table %>%
      na.omit()%>%
      filter(month != 9)%>%
      mutate(binary_clear = dplyr::if_else(mean <= .1,true = 1,false = 0)) %>%
      group_by(id)%>%
      summarize(prop_clear = sum(binary_clear)/n(),
                clear_days = sum(binary_clear),
                total_days = n())%>%
      inner_join(x = boxes_sf)%>%
      ggplot(mapping = aes(fill = prop_clear))+
      geom_sf()+
      geom_sf(data = domain,inherit.aes = FALSE,fill=NA)+
      scale_fill_gradient(low = "white",high = "sky blue",limits=c(0,1))+
      geom_sf_text(aes(label = round(prop_clear,digits = 2)))
    
  
################################################################################
    
  # Pull example data for plotting. Just need any 3 layers from cloud_sens
    
    domain_plus_boxes_ee <-
    st_union(domain_sf,boxes_sf) %>%
      st_bbox() %>%
      st_as_sfc() %>%
      sf_as_ee()
    
    ee_as_raster(image = clouds_sens$filter(ee$Filter$eq("date_char","2021-10-01"))$first(),
                 region = domain_plus_boxes_ee,
                 dsn = "data/flight_planning/example_cloud_cover_2021-10-01.tif")
    
    ee_as_raster(image = clouds_sens$filter(ee$Filter$eq("date_char","2021-11-01"))$first(),
                 region = domain_plus_boxes_ee,
                 dsn = "data/flight_planning/example_cloud_cover_2021-11-01.tif")
    
    ee_as_raster(image = clouds_sens$filter(ee$Filter$eq("date_char","2021-12-01"))$first(),
                 region = domain_plus_boxes_ee,
                 dsn = "data/flight_planning/example_cloud_cover_2021-12-01.tif")
    
    #Note that the correct projection for these needs to be manaully specified as
    
    nasa_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
    
###############################################################################
    
  # Calculate mean cloud cover over time
      # raster of mean total cloud cover (over 20 years during the oct-december  window)  
    
    Map$addLayer(clouds_sens$mean())
    
    # should be an average of cloud_sens
    ee_as_raster(image = clouds_sens$mean(),
                 region = domain_plus_boxes_ee,
                 dsn =  "data/output/mean_cloud_cover.tif",
                 scale = 1000)
    
    library(terra)
    test <- terra::rast("data/output/mean_cloud_cover.tif")
    plot(test)    
    
#################################################################################
#################################################################################    
    
# Wind data  
  
  # Calculate stats for flight boxes
            
    wind <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")
      #band: Wind_f_tavg
      
      
      
      wind_filtered <-
        filter_dates(collection = wind,
                     datestart = "2000-01-01",
                     datestop = paste(format(Sys.time(), "%Y-%m-%d"),sep = ""),
                     daystart = 273,
                     daystop = 366)
      
    
      get_polygon_stats_cloud <- function(img){
        
        date <- img$date()$format('yyyy-MM-dd')
        
        img$reduceRegions(
          collection = ee_boxes,
          reducer = ee$Reducer$mean(),
          scale = 11132 #native resolution is ~11 km, so no need to go finer than this
        )$map(function(x){ x$set("date",date) }) #sets the date for later use
        
      }
      
      wind_stats <- wind_filtered$select("Wind_f_tavg")$map(get_polygon_stats_cloud)
      
      flat_wind_stats <- wind_stats$flatten() #flatter into a table
      
      # Download table to drive
      
      
      flat_wind_stats$getInfo()
      
      flat_dl_wind<-
        ee_table_to_drive(collection = flat_wind_stats,
                          description = "wind_stats",
                          folder = "EMMA",
                          timePrefix = FALSE,
                          fileFormat = "CSV",
                          selectors = c("id","date","mean","target"))
      
      
      flat_dl_wind$start()#starts the processing
      ee_monitoring(flat_dl_wind,max_attempts = 100000) #keeps track of progress

#Download wind stats
      
    googledrive::drive_download(file = "EMMA/wind_stats.csv",
                                path = "data/wind_stats.csv",
                                overwrite = TRUE) #note: if dling more than one need to add a prefix or something
    
    wind_table <- read.csv("data/wind_stats.csv")
    
#Mean wind speed
    
    Map$addLayer(wind_filtered$select("Wind_f_tavg")$mean())
    
    # should be an average of cloud_sens
    ee_as_raster(image = wind_filtered$select("Wind_f_tavg")$mean(),
                 region = domain_plus_boxes_ee,
                 dsn =  "data/output/mean_wind.tif",
                 scale = 11132)
    
    library(terra)
    test <- terra::rast("data/output/mean_wind.tif")
    plot(test)    
    
    
    