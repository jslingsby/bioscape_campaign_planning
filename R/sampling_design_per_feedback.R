# Layers to add

  # Drought- Done
  # Distance to water (rivers, coasts, wetlands) - Done
  # Soil depth


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

#get domain

  domain <- read_sf("temp/domain.gpkg")
  
# Get parks in domain
  
  sampling_locations <- st_read("data/output/sampling_options.gpkg") %>% #these are parks in the domain
    dplyr::select(-domain)
  
#get sampling near roads (<= 1 km ) in parks
  
  sampling_near_roads <- st_read("data/output/sampling_options_near_roads.gpkg")

# get sites with ndvi > 0.2, slope < 30, within 2 hours of road, and near road

  acceptable_sites <- st_read("data/output/acceptable_sites.gpkg")

# Get an inventory of available data

env_files <- pb_list("AdamWilsonLab/emma_envdata")  

# Download wish list items


#################################
  # water distance
    
    #load data
      rivers <- st_read("data/manual_downloads/NFEPA/NFEPA_Rivers.shp")
      wetlands <- st_read("data/manual_downloads/NFEPA/NFEPA_Wetlands.shp")

    
    #aggregate to coarser resolution (using the same resolution as the elevation layers)

        recent_fires <- terra::rast(list.files("temp/recent_burn_dates/",full.names = TRUE))         
        
        # nlc %>%
        # terra::resample(y = insol_slope,method ="near") -> near_resamp
        
        rivers %>%
          #filter(grepl(pattern = "P",x = RIVTYPE))%>% #uncomment this to only include permanent rivers
          st_transform(crs = crs(recent_fires)) %>%
          vect() %>%
          terra::rasterize(y = recent_fires,
                    touches = TRUE) -> rivers
        
        wetlands %>%
          st_transform(crs = crs(recent_fires)) %>%
          vect() %>%
          terra::rasterize(y = recent_fires,
                           touches = TRUE) -> wetlands
        
        c(wetlands,rivers) %>%
        app(function(x){
          if(any(!is.na(x))){1}else{NA}
          }) -> water

        #first, try distance over the  domain
          # water %>%
          # terra::distance() -> dist_from_water

          # terra::writeRaster(x = dist_from_water,
          #                    filename = "data/output/distance_from_freshwater.tif",overwrite=TRUE)

        
        dist_from_water <- rast("data/output/distance_from_freshwater.tif")
        
######################################
    # drought layers go back to 1981
     
    drought <- raster::raster("data/manual_downloads/drought/Global_SPEI12_2019.nc") #note: using raster because terra misses the projection     

    drought_terra <- terra::rast("data/manual_downloads/drought/Global_SPEI12_2019.nc")
    terra::crs(drought_terra) <- terra::crs(drought)

    #crs(drought_terra) <- crs(drought)
        
    recent_fires <- terra::rast(list.files("temp/recent_burn_dates/",full.names = TRUE))         
    
    drought_terra %>%
      terra::resample(y = recent_fires) -> drought_resamp

    plot(drought_resamp)
    
    plot(drought_resamp[[1]])
    plot(domain %>%
           st_transform(crs(recent_fires)),add=TRUE,color=NA)
    
    # terra::writeRaster(x = drought_resamp,
    #                    filename = "data/output/2019_drought.tif",overwrite=TRUE)
    
###########################################
    
    #soil depth
    # 
    # library(rgee)
    # soil_depth <- ee$Image("ISDASOIL/Africa/v1/bedrock_depth")
    # 
    # tar_load(domain)
    # #Format the domain
    # domain <- sf_as_ee(x = domain)
    # domain <- domain$geometry()
    # 
    # soil_depth_raster <-ee_as_raster(image = soil_depth,
    #                                  region = domain,
    #                                  dsn = "data/temp/soil",
    #                                  maxPixels = 2000000000)
    # 
    # writeRaster(x = soil_depth_raster,
    #             filename = "data/temp/soil_depth_ISDASOIL.tif")
# 
#     soil_depth_raster <- rast("data/output/soil_depth_ISDASOIL.tif")
#     
#     soil_depth_raster %>%
#       terra::resample(y = recent_fires) -> soil_depth_raster_resamp
#     
#     plot(soil_depth_raster_resamp)
#     plot(soil_depth_raster_resamp[[1]],main="Soil Depth (cm)")
#     
#     writeRaster(x = soil_depth_raster_resamp,
#                 filename = "data/output/soil_depth_ISDASOIL.tif")
#     

###########################################     

#assemble layers
    
    recent_fires <- terra::rast(list.files("temp/recent_burn_dates/",full.names = TRUE))

    #Drought  

      source("R/count_spei_anomalies.R")
      
      list.files(path = "data/manual_downloads/drought/",full.names = TRUE) %>%
        count_spei_anomalies(thresh = -1,
                             domain = domain) %>%
        resample(y = recent_fires) -> drought
    
    
    # climate (1,12,15)
    
        climate <- terra::rast(list.files("data/climate/bio/bio_V1.2/",
                                          full.names = TRUE)) %>%
          terra::resample(y = recent_fires)
        
    # soil depth
        
        soil_depth <- rast("data/output/soil_depth_ISDASOIL.tif")[[1]]
      
    # distance from water  
      
        dist_from_water <- rast("data/output/distance_from_freshwater.tif")
    
    #stick all the layers together
        
      all_layers <-  c(climate, soil_depth, dist_from_water, drought)

# Do clustering
      
      clim_vals_domain <-
        terra::extract(all_layers,
                       y = vect(st_transform(x = domain,crs = crs(all_layers))),
                       cells=TRUE,
                       xy=TRUE)
      
      clim_vals_domain %>%
        select(-ID) -> clim_vals_domain
      
      clim_vals_domain %>%
        na.omit() -> clim_vals_domain
      
      k_clusters_domain <- kmeans(x = clim_vals_domain%>%
                                    select(-x,-y,-cell)%>%
                                    scale(),
                                  centers = 20,
                                  nstart = 1000,
                                  iter.max = 10000000)
      
      
      k_raster_domain <- all_layers[[1]]
      k_raster_domain[1:ncell(k_raster_domain)] <- NA
      k_raster_domain[clim_vals_domain$cell] <- k_clusters_domain$cluster
      
      plot(k_raster_domain)
      varnames(k_raster_domain)  <- "cluster"
      names(k_raster_domain)<-"cluster"
      
      
      sampling_near_roads %>%
        st_transform(crs = crs(k_raster_domain))%>%
        plot(add=TRUE)
        
      
      k_raster_domain %>%
        mask(vect(acceptable_sites)) %>%
        as.factor() %>%
        as.data.frame(xy = TRUE) %>%
        na.omit()%>%
        mutate(cluster = as.factor(cluster))-> k_df_domain_masked  
      
      k_raster_domain %>%
        as.factor() %>%
        as.data.frame(xy = TRUE) %>%
        na.omit()%>%
        mutate(cluster = as.factor(cluster))-> k_df_domain
      
      k_raster_domain %>%
        mask(vect(acceptable_sites)) %>%
        as.factor() %>%
        as.data.frame(xy = TRUE) %>%
        na.omit()%>%
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
      
      
###############################################################
      
      #Examining our spread in climate space
      
      k_df_domain %>%
        left_join(clim_vals_domain)-> k_df_w_climate
      
      k_df_domain_masked %>%
        left_join(clim_vals_domain)-> k_df_masked_w_climate
      
      colnames(k_df_w_climate)
      
      
      #temp and precip
      ggplot(data = k_df_w_climate,
             mapping = aes(x=CHELSA_bio10_01_V1.2,
                           y = CHELSA_bio10_12_V1.2
                           ))+
        geom_point(col = "grey")+
        geom_point(data = k_df_masked_w_climate,
                   mapping = aes(x=CHELSA_bio10_01_V1.2,
                                 y = CHELSA_bio10_12_V1.2,
                                 col = cluster))+
        xlab("temperature")+
        ylab("precipitation")
      
      #season and soil
      ggplot(data = k_df_w_climate,
             mapping = aes(x=CHELSA_bio10_15_V1.2,
                           y = mean_0_200
             ))+
        geom_point(col = "grey")+
        geom_point(data = k_df_masked_w_climate,
                   mapping = aes(x=CHELSA_bio10_15_V1.2,
                                 y = mean_0_200,
                                 col = cluster))+
        xlab("seasonality")+
        ylab("soil depth")
      
      
      #distance to water and droughts
      
      ggplot(data = k_df_w_climate,
             mapping = aes(x=lyr.1,
                           y =sum
             ))+
        geom_point(col = "grey")+
        geom_point(data = k_df_masked_w_climate,
                   mapping = aes(x=lyr.1,
                                 y = sum,
                                 col = cluster))+
        xlab("distance to water")+
        ylab("drought")
      
      
