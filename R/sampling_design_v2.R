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
    filter(tag == "raw_ndvi_modis") %>%
    arrange(file_name) %>%
    filter(file_name != "log.csv") %>%
    slice_tail(n = 12)%>%
    pull(file_name) %>%
    robust_pb_download(dest = "temp/recent_ndvi",
                       repo = "AdamWilsonLab/emma_envdata",
                       tag = "raw_ndvi_modis",
                       overwrite = TRUE,
                       max_attempts = 10,
                       sleep_time = 30)
  
# load ndvi and do necessary transforms  

  recent_ndvi <- ((terra::rast(list.files("temp/recent_ndvi/",full.names = TRUE))/100)-1)
  
# take mean ndvi over the last months or so
  
  recent_ndvi <- terra::app(x = recent_ndvi,
                            fun = function(x){mean(na.omit(x))})
  
  names(recent_ndvi) <- "NDVI"

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

    #Let's omit sites > 3 hours from the road
    
    time_to_sites <- rast("data/output/distance_h_to_sites.tif")

    road_site_ndvis <-
      terra::extract(x = time_to_sites,
                     y =   sampling_near_roads %>%
                       st_transform(crs = st_crs(time_to_sites)) %>%
                       vect())
    
    hist(road_site_ndvis$distance_h_to_sites,breaks=14)      

    time_to_sites <- (time_to_sites < 2)
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
      
      # only include sites within 2 hours of the road
    st_intersection(y= time_to_sites %>% st_make_valid()) %>%  
      
      #union and create a new object
    st_union() -> acceptable_sites

    plot(acceptable_sites)

    # acceptable_sites%>%
    # st_write(dsn = "data/output/acceptable_sites.shp",
    #          append=FALSE)
    # 
    # acceptable_sites%>%
    #   st_write(dsn = "data/output/acceptable_sites.gpkg",
    #            append=FALSE)
    
  rm(focal_sites_good_ndvi,sampling_locations,sampling_near_roads,slope__poly_buffered,time_to_sites)  
    

#########################################################
    
  #Kitchen sink of clustering 
    
  # Get static files
    
    env_files %>%
      filter(tag == "processed_static")%>%
      pull(file_name)%>%
      robust_pb_download(dest = "data/static/",
                         repo = "AdamWilsonLab/emma_envdata",
                         tag ="processed_static", 
                         max_attempts = 10,
                         sleep_time = 10)
  
  # Get most recend NDVI, time since fire

      # pull most recent NDVI
      
      env_files %>%
        filter(tag == "raw_ndvi_viirs") %>%
        arrange(timestamp) %>%
        filter(file_name != "log.csv") %>%
        slice((n()-11):n()) %>%
        pull(file_name) %>%
        robust_pb_download(dest = "temp/recent_ndvi",
                           repo = "AdamWilsonLab/emma_envdata",
                           tag = "raw_ndvi_viirs",
                           overwrite = TRUE,
                           max_attempts = 10,
                           sleep_time = 30)
      
      # load ndvi and do necessary transforms
      recent_ndvi <- ((terra::rast(list.files("temp/recent_ndvi/",full.names = TRUE))/100)-1)
      
      plot(recent_ndvi)
      
      #take mean of non-na values (since odds are ndvi will only be missing for one time step)
      recent_ndvi <- terra::app(x = recent_ndvi,
                         fun = function(x){mean(na.omit(x))})

  
      # pull most recent fire
      
      env_files %>%
        filter(tag == "processed_most_recent_burn_dates") %>%
        arrange(timestamp) %>%
        filter(file_name != "log.csv") %>%
        slice(n()) %>%
        pull(file_name) %>%
        robust_pb_download(dest = "temp/recent_burn_dates",
                           repo = "AdamWilsonLab/emma_envdata",
                           tag = "processed_most_recent_burn_dates",
                           overwrite = TRUE,
                           max_attempts = 10,
                           sleep_time = 30)
  
        
      #load most recent fire dates
      recent_fires <- terra::rast(list.files("temp/recent_burn_dates/",full.names = TRUE))
      
      # assign missing dates to the oldest date in the raster (since we know the vegetation is at least this old)
      recent_fires[is.na(recent_fires)] <-
        recent_fires %>%
        values()%>%
        na.omit()%>%
        min()
      
    list.files(path = "data/static/")  

  unique(values(all_rast[["SA_NLC_2020_GEO"]]))
    
all_layers <-          
c( "alos_chili.tif",
   "alos_mtpi.tif",
   "alos_topodiversity.tif",
   "CHELSA_bio10_01_V1.2.tif",
   "CHELSA_bio10_02_V1.2.tif",
   "CHELSA_bio10_03_V1.2.tif",
   "CHELSA_bio10_04_V1.2.tif",
   "CHELSA_bio10_05_V1.2.tif",
   "CHELSA_bio10_06_V1.2.tif",
   "CHELSA_bio10_07_V1.2.tif",
   "CHELSA_bio10_08_V1.2.tif",
   "CHELSA_bio10_09_V1.2.tif",
   "CHELSA_bio10_10_V1.2.tif",
   "CHELSA_bio10_11_V1.2.tif",
   "CHELSA_bio10_12_V1.2.tif",
   "CHELSA_bio10_13_V1.2.tif",
   "CHELSA_bio10_14_V1.2.tif",
   "CHELSA_bio10_15_V1.2.tif",
   "CHELSA_bio10_16_V1.2.tif",
   "CHELSA_bio10_17_V1.2.tif",
   "CHELSA_bio10_18_V1.2.tif",
   "CHELSA_bio10_19_V1.2.tif",
   "CHELSA_prec_01_V1.2_land.tif",
   "CHELSA_prec_07_V1.2_land.tif",
   "MODCF_interannualSD.tif",
   "MODCF_intraannualSD.tif",
   "MODCF_meanannual.tif",
   "MODCF_seasonality_concentration.tif",
   "nasadem.tif",
   "soil_EC_mS_m.tif",
   "soil_Ext_K_cmol_kg.tif",
   "soil_Ext_Na_cmol_kg.tif",
   "soil_Ext_P_mg_kg.tif",
   "soil_pH.tif",
   "soil_Total_C_.tif",
   "soil_Total_N_.tif") 


      
all_rast <- rast(paste("data/static/",all_layers,sep = ""))
all_rast <- c(all_rast,recent_fires,recent_ndvi)
all_rast

for(i in 1:dim(all_rast)[3]){
  
  plot(all_rast[[i]],main = names(all_rast)[i])
  plot(st_transform(domain,crs = crs(all_rast)),add=TRUE,color=NA)

}

library(corrplot)

corrplot(corr = cor(na.omit(values(all_rast))))




      
  #things to add
  
    # static stuff
    # time since fire
    # ndvi
    
  env_files %>%
    filter(tag == "processed_static")

  clim_vals_domain <-
    terra::extract(all_rast,
                   y = vect(st_transform(x = domain,crs = crs(all_rast))),
                   cells=TRUE,
                   xy=TRUE)
  
  clim_vals_domain %>%
    select(-ID)->clim_vals_domain
  
  clim_vals_domain %>%
    na.omit()->clim_vals_domain

  k_clusters_domain <- kmeans(x = clim_vals_domain%>%
                                select(-x,-y,-cell)%>%
                                scale(),
                              centers = 20,
                              nstart = 100,
                              iter.max = 1000000)
  
  
  k_raster_domain <- all_rast[[1]]
  k_raster_domain[1:ncell(k_raster_domain)] <- NA
  k_raster_domain[clim_vals_domain$cell] <- k_clusters_domain$cluster
  
plot(k_raster_domain)
varnames(k_raster_domain)  <- "cluster"
  
k_raster_domain %>%
  mask(vect(acceptable_sites)) %>%
  as.factor() %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()%>%
  rename(cluster = alos_chili)%>%
  mutate(cluster = as.factor(cluster))-> k_df_domain_masked  
  
k_raster_domain %>%
  as.factor() %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()%>%
  rename(cluster = alos_chili)%>%
  mutate(cluster = as.factor(cluster))-> k_df_domain

k_raster_domain %>%
  mask(vect(acceptable_sites)) %>%
  as.factor() %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()%>%
  rename(cluster = alos_chili)%>%
  mutate(cluster = as.factor(cluster))-> k_df_domain_masked


ggplot(data = k_df_domain) +
  geom_tile(aes(x = x, y = y, fill = cluster))+
  scale_fill_discrete()+
  geom_sf(data = st_transform(domain,crs = crs(k_raster_domain)),fill=NA)


ggplot(data = k_df_domain) +
  geom_tile(aes(x = x, y = y, fill = cluster))+
  scale_fill_discrete()+
  geom_sf(data = st_transform(acceptable_sites,crs = crs(k_raster_domain)),fill=NA)+
  geom_sf(data = st_transform(domain,crs = crs(k_raster_domain)),fill=NA)

ggplot(data = k_df_domain_masked) +
  geom_tile(aes(x = x, y = y, fill = cluster))+
  scale_fill_discrete()+
  geom_sf(data = st_transform(domain,crs = crs(k_raster_domain)),fill=NA)


k_df_domain %>%
  ggplot()+
  geom_histogram(aes(cluster),stat = "count",fill="grey",group = "domain")+
  geom_histogram(data = k_df_domain_masked,mapping = aes(cluster),fill = "blue",stat = "count",group = "focal")+
  scale_fill_manual(name="group",values=c("blue","grey"),labels=c("domain","focal"))

?geom_histogram
# try to get model running locally in docker


    
      
    
    
    
    
    
    
    
    
    
    
    
        
    
  
  
  
  
  
  
  