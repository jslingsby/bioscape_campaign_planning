#sampling design v2

# Load packages
library(googledrive)
library(terra)
library(ggplot2)
library(stars)
library(starsExtra)    
library(sf)
library(movecost) #https://www.sciencedirect.com/science/article/pii/S2352711019302341#fig2
library(piggyback)
library(tidyverse)
library(insol)
source("R/get_park_polygons.R")
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")

# Get necessary data
  sampling_locations <- st_read("data/output/sampling_options.gpkg") %>% #these are parks in the domain
                        dplyr::select(-domain)
  
  sampling_near_roads <- st_read("data/output/sampling_options_near_roads.gpkg") #these are parks in the domain
  
  domain <- read_sf("temp/domain.gpkg")


# Get an inventory of available data
  
  env_files <- pb_list("AdamWilsonLab/emma_envdata")  
  
# pull most recent NDVI
  
  env_files %>%
    filter(tag == "raw_ndvi_viirs") %>%
    arrange(timestamp) %>%
    filter(file_name != "log.csv") %>%
    slice(n()) %>%
    pull(file_name) %>%
    robust_pb_download(dest = "temp/recent_ndvi",
                       repo = "AdamWilsonLab/emma_envdata",
                       tag = "raw_ndvi_viirs",
                       overwrite = TRUE,
                       max_attempts = 10,
                       sleep_time = 30)
  
# load ndvi and do necessary transforms  
  recent_ndvi <- ((terra::rast(list.files("temp/recent_ndvi/",full.names = TRUE))/100)-1)
    

###################################################
  
  #Filtering for NDVI
  
  #Lets take a look at the distribution of ndvi vals in the focal sites
  
  road_site_ndvis <-
  terra::extract(x = recent_ndvi,
          y =   sampling_near_roads %>%
            st_transform(crs = st_crs(recent_ndvi)) %>%
            vect())
  
  hist(road_site_ndvis$NDVI,
       xlim = c(-1,1),
       breaks = seq(-1, 1, 0.1))
  abline(v = .2, col = "red") #not losing too many spots
  
  #cut down to good/borderline ndvis
  focal_sites_good_ndvi <-
    recent_ndvi %>%
    mask(sampling_near_roads %>%
           st_transform(crs = st_crs(recent_ndvi)) %>%
           vect()) %>%
    mask(recent_ndvi >= 0.2, maskvalues=0, updatevalue = NA)%>%
    terra::as.polygons() %>%
    st_as_sf()%>%
    st_transform(crs = crs(domain))
  
    
##############################################################
  
# Filtering for slope
  
    # 
    #   #get DEM
    #     robust_pb_download(file = "nasadem.tif",
    #                        dest = "temp/nasadem",
    #                        repo = "AdamWilsonLab/emma_envdata",
    #                        tag = "raw_static",
    #                        overwrite = TRUE,
    #                        max_attempts = 10,
    #                        sleep_time = 30)
    #   
    #   #load dem
    #     dem <- terra::rast("temp/nasadem/nasadem.tif")
    #     plot(dem)
    #     
    #   # cut down to only the parks(and maybe a small buffer for edge effects) to save memory
    #     dem <-
    #     dem %>%
    #       terra::mask(mask = sampling_locations %>%
    #              st_buffer(dist = 1000) %>%
    #              st_transform(crs = st_crs(dem)) %>%
    #              vect())
    #              
    #     # terra::tmpFiles(current = FALSE,
    #     #                 orphan = TRUE,
    #     #                 old = TRUE,
    #     #                 remove = TRUE)
    #     
    #     
    #   # Need DEM in units of meters, so have to transform   
    #     dem <- terra::project(dem, y = crs(domain))
    # 
    #     #need to break up dem to avoid "cannot allocate vector of X Gb" warning
    #       
    #     poly_for_dem <-
    #       sampling_locations %>%
    #         st_buffer(dist = 1000) %>%
    #         st_transform(crs = st_crs(dem)) %>%
    #         st_union() %>%
    #       st_make_valid() %>%
    #       st_cast("POLYGON") %>%
    #       as.data.frame()
    #     
    #   for(i in 1:nrow(poly_for_dem)){
    #     
    #     poly_i <- poly_for_dem[i,]
    # 
    #     dem_i <- dem %>%
    #       mask(mask = vect(poly_i))
    #     
    #     dem_i <- 
    #       dem_i %>%
    #       crop(poly_i)
    #     
    #     #plot(dem_i)
    #     
    #     #calc slope using insol package
    #       
    #       slope_insol <- insol::slope(cgrad(raster::raster(dem_i)), degrees = TRUE)
    #       slope_insol <- raster::raster(slope_insol,
    #                                    crs=raster::projection(dem_i))
    #       raster::extent(slope_insol) <- raster::extent(raster::raster(dem_i))
    #       plot(slope_insol)
    #   
    # 
    #     #calc slop using stars package
    #       
    #       slope_stars <- starsExtra::slope(x = stars::st_as_stars(dem_i,proxy=FALSE))    
    #       slope_stars <- rast(slope_stars)
    #       #plot(slope_stars)
    # 
    #     if(i == 1){
    #       
    #       stars_slope <- slope_stars
    #       insol_slope <- slope_insol
    #       
    #     }else{
    #       
    #       stars_slope <- terra::merge(stars_slope, slope_stars)
    #       insol_slope <- terra::merge(insol_slope, slope_insol)
    #       
    #     }
    #     
    #     rm(slope_stars, slope_insol, dem_i, poly_i)
    #     
    #     #clear out temp files to keep hd from filling up
    #     terra::tmpFiles(current = FALSE,
    #                     orphan = TRUE,
    #                     old = TRUE,
    #                     remove = TRUE)
    # 
    #   }
    # 
    # gc()
    # 
    # # writeRaster(x = insol_slope, filename = "data/output/insol_slope.tif")
    # # writeRaster(x = stars_slope, filename = "data/output/stars_slope.tif")
    #     
    # insol_slope <- rast("data/output/insol_slope.tif")
    # stars_slope <- rast("data/output/stars_slope.tif")
    # 
    # #make a polygon of sites that have a reasonable slope in each raster (< 30 degrees)
    # 
    #   insol_slope <- (insol_slope < 30)
    #   insol_slope[which(is.na(values(insol_slope)))] <- 0
    #   
    #   stars_slope <- (stars_slope < 30)
    #   stars_slope[which(is.na(values(stars_slope)))] <- 0
    #   
    #   #insol doesn't keep the crs, so add it in here
    #     crs(insol_slope) <- crs(stars_slope)
    # 
    #   combined_slope <- insol_slope + stars_slope
    #   
    #   #plot(combined_slope) #pretty good correspondence. we'll keep any that meet the criteria using either approach
    #   combined_slope <- (combined_slope > 0)
    #   combined_slope[which(values(combined_slope)==0)] <- NA
    #   
    #   
    #   plot(combined_slope)
    #   
    #   slope_poly <-
    #     combined_slope %>%
    #       terra::as.polygons(trunc = TRUE,
    #                   dissolve = TRUE,
    #                   values = TRUE) %>%
    #     st_as_sf()
    # # 
    # #         st_write(obj = slope_poly,
    # #                 dsn = "data/output/flat_polygons.gpkg")
    
    slope_poly <- st_read("data/output/flat_polygons.gpkg")
    
    #add a slight buffer at a bit larger than pixel size so that we cut out areas where its just a pixel or two of steep terrain.
    slope__poly_buffered <-
      slope_poly %>%
      st_buffer(dist = 40)
    
#################

    #Let's make omit sites > 3 hours from the road
    
    time_to_sites <- rast("data/output/distance_h_to_sites.tif")
    time_to_sites <- (time_to_sites < 3)
    time_to_sites[time_to_sites == 0] <- NA
    
    time_to_sites %>%
    terra::as.polygons() %>%
      st_as_sf()-> time_to_sites
    
crs(time_to_sites)@projargs==crs(domain)@projargs


#################    

  rm(recent_ndvi, road_site_ndvis, slope_poly)  
        terra::tmpFiles(current = FALSE,
                        orphan = TRUE,
                        old = TRUE,
                        remove = TRUE)
  gc()
    
  plot(sampling_near_roads)  
  
      
    #put it all together
    sampling_near_roads %>%
      
      #only include sites with an ndvi > 0.2
    st_intersection(y = st_make_valid(focal_sites_good_ndvi)) %>%
      
      #only include sites that have a slope < 30 degrees
    st_intersection(y = slope__poly_buffered) %>%  
      
      # only include sites within 3 hours of the road
    st_intersection(y= time_to_sites %>% st_make_valid()) %>%  
      
      #union and create a new object
    st_union() -> acceptable_sites

    plot(acceptable_sites)
    

    
    
    
  
  
  
  
  
  
  