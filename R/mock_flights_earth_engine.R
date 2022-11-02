# Load packages
  library(rgee)
  library(targets)
  library(sf)

# install rgee python dependencies
  rgee::ee_install()
  rgee::ee_install_upgrade()
  rgee::ee_check()

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
                   datestart = "2020-09-30",
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
  ee_monitoring(flat_dl,max_attempts = 1000) #keeps track of progress

# Download table from drive  
  googledrive::drive_download(file = "EMMA/cloud_stats.csv",
                              path = "data/test_cloud_stats.csv",
                              overwrite = TRUE) #note: if dling more than one need to add a prefix or something
  
  cloud_table <- read.csv("data/test_cloud_stats.csv")
  