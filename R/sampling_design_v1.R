# Load packages
library(googledrive)
library(terra)
library(ggplot2)
library(stars)
library(sf)
library(movecost) #https://www.sciencedirect.com/science/article/pii/S2352711019302341#fig2
library(piggyback)
source("R/get_park_polygons.R")
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")

# Get necessary data

  roads <- read_sf("data/manual_downloads/Roads/cfr_roads.gpkg")
  parks <- get_park_polygons()

# combine park polygons into one set by chucking extraneous data
  
    parks <-
      parks$cape_nature %>%
        dplyr::select(RESERVENAM)%>%
        rename(park_name = RESERVENAM)%>%
        bind_rows(parks$national_parks %>% 
                    dplyr::select(CUR_NME)%>%
                    rename(park_name = CUR_NME))
    
# Get domain

    pb_download(file = "domain.gpkg",
                dest = 'temp/',
                repo = "AdamWilsonLab/emma_envdata",
                tag = "raw_static")

    domain <- read_sf("temp/domain.gpkg")

# Pull elevation data from envdata releases  
    
  # For original resolution (267 Mb)
  
    pb_download(file = "nasadem.tif",
                dest = "temp/",
                repo = "AdamWilsonLab/emma_envdata",
                tag = "raw_static",
                overwrite = TRUE)
  
  # For modis resolution/projection (4.7 Mb, 500m resolution) 
    # pb_download(file = "nasadem.tif",
    #             dest = "temp/",
    #             repo = "AdamWilsonLab/emma_envdata",
    #             tag = "processed_static",
    #             overwrite = TRUE )
      
    nasadem <- rast("temp/nasadem.tif")
    nasadem <- mask(x = nasadem,
                    mask = vect(domain)) # mask domain to get rid of 0 values that should be NA
    
############################
    
    # Transform everything to a common equal area projection.  Here I'll use AEA_RSA_WGS84
    
    
    # reproject dem into equal area

      nasadem <-
        nasadem %>%
        terra::project(
          y = st_crs(domain)$proj4string)

    #we only want parks in the domain
      parks <-
        parks %>%
          st_transform(st_crs(domain))%>%
        st_intersection(domain)
    
  # Buffer parks by ~10km (this is to avoid edge artifacts of roads near but outside parks)
  # Crop roads to buffered parks
  
  #temp
      
    roads <- 
    roads %>%
      st_transform(crs = st_crs(domain)) %>%
      dplyr::select(FEAT_TYPE) %>%
      st_intersection(y = parks %>%
                st_buffer(10000))

  # Buffer cropped roads to ~1km (this is the maximum walking distance to consider - we can change it later if needed).
  
    roads %>%
      dplyr::filter(FEAT_TYPE  %in%
                      c("Main Road",
                        "Arterial Road",
                        "Other Road",
                        #"Footpath",
                        "Secondary Road",
                        "Street",
                        "National Road",
                        #"On/OffRamp",
                        "National Freeway"
                        #,
                        #"Track",
                        #"Slipway"
                        ) ) %>%
      st_buffer(dist = 1000) -> road_buffer
    
    road_buffer %>%
      st_union() -> road_buffer
    
    plot(road_buffer)
    
  
  # Crop the buffered roads to parks/public land. These will be the 'caterpillar' shaped sampling domain (all public lands within 3km of a road). 

  road_buffer %>%
    st_intersection(parks) %>%
    st_union() -> road_buffer
  

  # Optional - use this much-reduced domain to do the movecost algorithm to get time to visit each 30m pixel within the sampling domain and further subtract areas that are very difficult to access - e.g. the cost thing should remove sites that are on top of a cliff even though it's adjacent to a road.


      # mask the dem
        
        nasadem %>%
          mask(vect(road_buffer)) %>%
          crop(as_Spatial(road_buffer)) -> masked_dem
        
          
      # cut roads down to those in the road buffer
        # simplify(?)
        # convert to points
        
        roads %>%
          dplyr::filter(FEAT_TYPE  %in%
                          c("Main Road",
                            "Arterial Road",
                            "Other Road",
                            #"Footpath",
                            "Secondary Road",
                            "Street",
                            "National Road",
                            #"On/OffRamp",
                            "National Freeway"
                            #,
                            #"Track",
                            #"Slipway"
                          ) ) %>%
          st_intersection(y = road_buffer) -> cropped_roads
      
        ggplot() +
          geom_sf(data = domain) +
          geom_sf(data = parks) +
          geom_sf(data = cropped_roads, col="red")
        
        
          
        #rm(roads, nasadem)
  
  # save cropped parks for later use              
    st_write(obj = parks,
             dsn = "data/output/sampling_options.gpkg",
             append = FALSE)
    
  # save buffered roads for later use  
    
    st_write(obj = road_buffer,
             dsn = "data/output/sampling_options_near_roads.gpkg",
             append = FALSE)
    
    
  # Calculate movement cost
  #https://www.sciencedirect.com/science/article/pii/S2352711019302341#fig2
  
  library(movecost)
  
  
  #Start the environmental stratification of these sampling sites compared to the full range of environments across the GCFR.  What's the smallest set of locations that approximately captures the variability across the region?
    # Mean annual temperature
    # Mean annual precipitation
    # Precipitation seasonality
    # Elevation?
    # Soil?
    # The rest of the static env vars from emma?


  #v.1
  

    #check that everything aligns alright  
      
      plot(masked_dem)
      plot(road_buffer,add=TRUE)
      plot(cropped_roads)

    #iterate through individual parks to keep size down
      
      #park_name_i <- unique(parks$park_name)[1]
      
    for(i in 1:length(unique(parks$park_name))){
      
      print(i/length(unique(parks$park_name)))

      gc()

      park_name_i <- unique(parks$park_name)[i]
      
      print(park_name_i)
      
      park_i <-
        parks %>%
        filter(park_name == park_name_i)
      
        
      plot(park_i)   
      
      #check whether the park is within the domain (masking will throw errors otherwise)
        if(!terra::relate(x = ext(masked_dem),
                      y = vect(park_i),
                      relation = "intersects")){next}
      
      # only take the part of the park within our domain (only relevant for a few, e.g. Karoo)
      
      park_i <-
        park_i %>%
          st_intersection(domain)

      #mask the dem to just the park      
      dem_i <- 
        masked_dem %>%
            mask(vect(park_i)) %>%
            crop(park_i)
      
      plot(masked_dem)
      plot(park_i, add = TRUE)
      
      plot(dem_i)
      
      #crop roads to relevant ones
      
      roads_i <- 
        cropped_roads %>%
        st_intersection(y = park_i) %>%
        st_cast("MULTIPOINT") %>%
        st_cast("POINT")

      gc()
      
      if(nrow(roads_i) == 0){next}
      
      plot(dem_i)
      plot(roads_i[1],
           add = TRUE)
      
      #trying to figure out why karoo breaks
      #   test <- terra::extract(x = dem_i,
      #                  y = vect(roads_i))
      #   
      #   roads_i <- 
      #     roads_i %>%
      #     st_intersection(y = park_i%>%
      #                       st_buffer(dist = -7000))
      #   
      #   library(stars)
      #   
      #   ggplot()+
      #     geom_stars(data = st_as_stars(dem_i))+
      #     geom_sf(data = park_i,col="black",fill=NA)+
      #     geom_sf(data = roads_i)
      # 
      # plot(masked_dem)  
        
      # end figuring out    
          
      movement_cost_i <- 
        movecost(dtm = raster::raster(dem_i),
                 origin = as_Spatial(roads_i),
                 studyplot = as_Spatial(park_i),
                 outp = "raster",
                 funct = "tofp",
                 move = 8,
                 time = "h",
                 return.base = FALSE,
                 graph.out = FALSE,
                 cont.lab = FALSE)
      
      plot(movement_cost_i$accumulated.cost.raster)
      
      # Aggregate output rasters
      
      if(i == 1){
        
        movecost_rast_out <- movement_cost_i$accumulated.cost.raster
        
      }else{
      
        movecost_rast_out <- (merge(movecost_rast_out,movement_cost_i$accumulated.cost.raster))
        plot(movecost_rast_out)
          
      }
      
      rm(movement_cost_i, roads_i, park_i, dem_i)
      gc()
      tmpFiles(current = FALSE,
               orphan = TRUE,
               old = TRUE,
               remove = TRUE)
      
    }  
      
    
# Examine output
       
    # writeRaster(x = movecost_rast_out,
    #             filename = "data/output/distance_h_to_sites.tif",overwrite=TRUE)
      
# publish sampling distances as a raster

    pb_upload(file = "data/output/distance_h_to_sites.tif",
              repo = "BioSCape-io/terrestrial_sampling",
              tag = "current")  
          
      
    gc()
    plot(movecost_rast_out)
    library(terra)
    # time <- terra::rast("data/output/distance_h_to_sites.tif")
    # 
  plot(time)    
    
################################################################################

  # Looking at environmental gradient
        
  movecost_rast_out <- rast("data/output/distance_h_to_sites.tif")
  sampling_locations <- st_read("data/output/sampling_options.gpkg")

  movecost_rast_out %>%
    as.data.frame(xy = TRUE) %>%
    na.omit()%>%
  ggplot() +
    geom_tile(aes(x = x, y = y, fill = distance_h_to_sites))+
    scale_fill_gradient(low = "green",high = "red")+
    geom_sf(data = domain, fill = NA)+
    geom_sf(data = sampling_locations, fill = NA)
  
  
  hist(na.omit(values(movecost_rast_out)), main = "Histogram of time (h) to focal sites")
  
  # get climate data
    
    library(ClimDatDownloadR)
    
    ClimDatDownloadR::Chelsa.Clim.download(save.location = "data/climate/",
                                           parameter = "bio",
                                           bio.var = c(1,12,15))
    
  # read in data
    climate <- terra::rast(list.files("data/climate/bio/bio_V1.2/",
                                      full.names = TRUE))
    
  #crop climate data (crop before projecting to make it easier)

    climate %>%
      crop(y = st_transform(domain, crs = crs(climate)))-> climate
    
    climate %>%
      terra::project(y = crs(movecost_rast_out)) -> climate
    
  # extract values within the domain
    
    clim_vals_domain <-
      terra::extract(climate,
                     y = vect(domain),
                     cells=TRUE,
                     xy=TRUE)
    
    clim_vals_domain %>%
      rename(bio1 = CHELSA_bio10_01_V1.2,
             bio12 = CHELSA_bio10_12_V1.2,
             bio15 = CHELSA_bio10_15_V1.2) %>%
      dplyr::select(-ID) -> clim_vals_domain
    
  # extract values within the parks
    
    clim_vals_parks <-
      terra::extract(climate,
                     y = vect(parks),
                     cells=TRUE,
                     xy=TRUE)
    
    clim_vals_parks %>%
      rename(bio1 = CHELSA_bio10_01_V1.2,
             bio12 = CHELSA_bio10_12_V1.2,
             bio15 = CHELSA_bio10_15_V1.2) %>%
      dplyr::select(-ID) -> clim_vals_parks

    
  # extract values within the potential sampling locations
    
  clim_vals_focal <-
    terra::extract(climate,
                   y = vect(road_buffer),
                   cells=TRUE,
                   xy=TRUE)
  

  clim_vals_focal %>%
    rename(bio1 = CHELSA_bio10_01_V1.2,
           bio12 = CHELSA_bio10_12_V1.2,
           bio15 = CHELSA_bio10_15_V1.2) %>%
    dplyr::select(-ID) -> clim_vals_focal
  
  
  # visualize climate vals

    cor(na.omit(clim_vals_domain[1:3]))# all pretty independent
    cor(na.omit(clim_vals_focal[1:3]))# all pretty independent
    
    bio1_v_12_domain <-
    ggplot(data = clim_vals_domain,
           mapping = aes(x = bio1, y = bio12)) +
      geom_point()
    
    bio1_v_12_focal <-
      ggplot(data = clim_vals_focal,
             mapping = aes(x = bio1, y = bio12)) +
      geom_point()
    
    
    bio1_v_15_domain <-
    ggplot(data = clim_vals_domain,
           mapping = aes(x = bio1, y = bio15)) +
      geom_point()
    
    
    bio1_v_15_focal <-
      ggplot(data = clim_vals_focal,
             mapping = aes(x = bio1, y = bio15)) +
      geom_point()
    
    bio12_v_15_domain <-
    ggplot(data = clim_vals_domain,
           mapping = aes(x = bio12, y = bio15)) +
      geom_point()
    
    bio12_v_15_focal <-
      ggplot(data = clim_vals_focal,
             mapping = aes(x = bio12, y = bio15)) +
      geom_point()
    
    
    bio1_v_12v_2_domain <-
      ggplot(data = clim_vals_domain,
             mapping = aes(x = bio1, y = bio12)) +
      geom_point(aes(col=bio15))
    
    
    bio1_v_12v_2_focal <-
      ggplot(data = clim_vals_focal,
             mapping = aes(x = bio1, y = bio12)) +
      geom_point(aes(col=bio15))
    
    bio1_v_12v_2_domain
    
    
    #save plots

      ggsave(filename = file.path("figures/bio1_v_12_domain.jpg"),
             plot = bio1_v_12_domain)
      
      ggsave(filename = file.path("figures/bio1_v_15_domain.jpg"),
             plot = bio1_v_15_domain)
      
      ggsave(filename = file.path("figures/bio12_v_15_domain.jpg"),
             plot = bio12_v_15_domain)
      
      
      ggsave(filename = file.path("figures/bio1_v_12_focal.jpg"),
             plot = bio1_v_12_focal)
      
      ggsave(filename = file.path("figures/bio1_v_15_focal.jpg"),
             plot = bio1_v_15_focal)
      
      ggsave(filename = file.path("figures/bio12_v_15_focal.jpg"),
             plot = bio12_v_15_focal)
      
    #3d plots 
      #https://stackoverflow.com/questions/50027798/in-r-rgl-how-to-print-shadows-of-points-in-plot3d
      library(rgl)
      plot3d(x = clim_vals_domain$bio1,
             y = clim_vals_domain$bio12,
             z = clim_vals_domain$bio15,
             xlab = "Temp",
             ylab = "Precip",
             zlab = "Prec. Seasonality",
             main = "Domain Level")
      
      plot3d(x = clim_vals_focal$bio1,
             y = clim_vals_focal$bio12,
             z = clim_vals_focal$bio15,
             xlab = "Temp",
             ylab = "Precip",
             zlab = "Prec. Seasonality",
             main = "Focal sites")
      
      # show2d({
      #   par(mar=c(0,0,0,0))
      #   plot(x = clim_vals$bio1,
      #        y = clim_vals$bio12, 
      #        col = "grey")
      # })

#############################################
  
  #Dividing up sites
      
      domain <- read_sf("temp/domain.gpkg")
      
  #v1 clustering
      
  library(cluster)    

  clim_vals_domain <- na.omit(clim_vals_domain)
  clim_vals_focal <- na.omit(clim_vals_focal)
  clim_vals_parks <- na.omit(clim_vals_parks)
  
      
  k_clusters_domain <- kmeans(x = scale(clim_vals_domain[,1:3]),
                       centers = 20,nstart = 100,
                       iter.max = 1000000)
  
  k_clusters_parks <- kmeans(x = scale(clim_vals_parks[,1:3]),
                             centers = 20,nstart = 100,
                             iter.max = 1000000)
  
  
  k_clusters_focal <- kmeans(x = scale(clim_vals_focal[,1:3]),
                              centers = 20,nstart = 100,
                              iter.max = 1000000)
  
  
  
  plot3d(x = clim_vals_domain$bio1,
         y = clim_vals_domain$bio12,
         z = clim_vals_domain$bio15,
         xlab = "Temp",
         ylab = "Precip",
         zlab = "Prec. Seasonality",
         col = rainbow(n = length(unique(as.numeric(k_clusters_domain$cluster))))[as.numeric(k_clusters_domain$cluster)])
  
  
  plot3d(x = clim_vals_focal$bio1,
         y = clim_vals_focal$bio12,
         z = clim_vals_focal$bio15,
         xlab = "Temp",
         ylab = "Precip",
         zlab = "Prec. Seasonality",
         col = rainbow(n = length(unique(as.numeric(k_clusters_focal$cluster))))[as.numeric(k_clusters_focal$cluster)])
  
  
  
  
  k_raster_domain <- climate[[1]]
  k_raster_domain[1:ncell(k_raster_domain)] <- NA
  k_raster_domain[clim_vals_domain$cell] <- k_clusters_domain$cluster
  
  k_raster_domain %>%
    mask(vect(road_buffer)) %>%
           as.factor() %>%
           as.data.frame(xy = TRUE) %>%
           na.omit()%>%
           rename(cluster = CHELSA_bio10_01_V1.2)%>%
           mutate(cluster = as.factor(cluster))-> k_df_domain_masked
         

  k_raster_domain %>%
    as.factor() %>%
  as.data.frame(xy = TRUE) %>%
    na.omit()%>%
    rename(cluster = CHELSA_bio10_01_V1.2)%>%
    mutate(cluster = as.factor(cluster))-> k_df_domain

  
  
  ggplot(data = k_df_domain) +
    geom_tile(aes(x = x, y = y, fill = cluster))+
    scale_fill_discrete()+
    geom_sf(data = domain,fill=NA)+
    geom_sf(data = parks, fill = NA)

  ggplot(data = k_df_domain_masked) +
    geom_tile(aes(x = x, y = y, fill = cluster))+
    scale_fill_discrete()+
    geom_sf(data = domain,fill=NA)+
    geom_sf(data = parks, fill = NA)
  
    
  k_raster_parks <- setValues(climate[[1]],NA)
  
  k_raster_parks[clim_vals_parks$cell] <- k_clusters_parks$cluster
  
  k_raster_parks %>%
    as.factor() %>%
    as.data.frame(xy = TRUE) %>%
    na.omit()%>%
    rename(cluster = CHELSA_bio10_01_V1.2)%>%
    mutate(cluster = as.factor(cluster))-> k_df_parks
  
  ggplot(data = k_df_parks) +
    geom_tile(aes(x = x, y = y, fill = cluster))+
    scale_fill_discrete()+
    geom_sf(data = domain,fill=NA)
  
  
  k_raster_focal <- setValues(climate[[1]],NA)
  
  k_raster_focal[clim_vals_focal$cell] <- k_clusters_focal$cluster
  
  k_raster_focal %>%
    as.factor() %>%
    as.data.frame(xy = TRUE) %>%
    na.omit()%>%
    rename(cluster = CHELSA_bio10_01_V1.2)%>%
    mutate(cluster = as.factor(cluster))-> k_df_focal
  
  ggplot(data = k_df_focal) +
    geom_tile(aes(x = x, y = y, fill = cluster))+
    scale_fill_discrete()+
    geom_sf(data = domain,fill=NA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # do all clusters across the domain occur in parks? focal parks?

  terra::extract(x = k_raster_domain,
                 y = vect(st_union(domain))) %>%
    pull(CHELSA_bio10_01_V1.2)%>%
    hist(main="Histogram of clusters in domain")
  
  
  terra::extract(x = k_raster_domain,
               y = vect(st_union(parks))) %>%
  pull(CHELSA_bio10_01_V1.2)%>%
hist(main="Histogram of clusters in parks")

terra::extract(x = k_raster_domain,
               y = vect(st_union(road_buffer))) %>%
  pull(CHELSA_bio10_01_V1.2)%>%
  hist(main="Histogram of clusters in parks near roads")

res(k_raster_domain)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# How many veg types show up in the clusters?


env_files <- pb_list(repo = "AdamWilsonLab/emma_envdata")
vegmap <- st_read("C:/Users/Brian Maitner/Desktop/current_projects/emma_envdata/data/manual_download/VEGMAP2018_AEA_16082019Final/VEGMAP2018_Final.gdb")

colnames(vegmap)

test <-
vegmap[c("Name_18","BIOME_18","BIOREGION_18",
  "SUBTYPNM_18","MCDSUBTYPE")]%>%
  st_drop_geometry()%>%unique()
unique(test$Name_18)#467
unique(test$BIOREGION_18)#44
unique(test$SUBTYPNM_18)#26
unique(test$MCDSUBTYPE)#28
unique(test$BIOME_18)#~9

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Visualizing the study area

library(raster)
library(mapview)
library(leaflet)
library(leafem)

domain %>%
  st_transform(crs = st_crs(4326)) -> domain_wgs

parks %>%
  st_transform(crs = st_crs(4326)) -> parks_wgs

road_buffer %>%
  st_transform(crs = st_crs(4326)) -> road_buffer_wgs




leaflet(data = domain_wgs) %>%
  addProviderTiles("Esri.NatGeoWorldMap", group = "NatGeo") %>%
  #addProviderTiles("NASAGIBS.ModisTerraTrueColorCR", group = "True Colors") %>%
  #addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addPolygons(color = "black",
              stroke = TRUE,
              fill = FALSE,
              group = "Domain") %>%
  addPolygons(data = parks_wgs,
              color = "red",
              stroke = FALSE,
              fill = TRUE,
              fillOpacity = 1,
              group = "Parks") %>%
  addPolygons(data = road_buffer_wgs,
              color = "Red",
              stroke = FALSE,
              fill = TRUE,
              group = "Options",fillOpacity = 1)%>%
  addLayersControl(
    baseGroups = c("NatGeo"),
    overlayGroups = c("Domain", "Parks","Options"),
    options = layersControlOptions(collapsed = FALSE),position = "topright")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  k_clusters2 <- kmeans(x = scale(clim_vals[,c(1:3,5:6)]),
                       centers = 16,nstart = 100,
                       iter.max = 1000000)
  
  plot3d(x = clim_vals$bio1,
         y = clim_vals$bio12,
         z = clim_vals$bio15,
         xlab = "Temp",
         ylab = "Precip",
         zlab = "Prec. Seasonality",
         col = rainbow(n = length(unique(as.numeric(k_clusters2$cluster))))[as.numeric(k_clusters2$cluster)])
  
  k_raster2 <- setValues(climate[[1]],NA)
  
  k_raster2[clim_vals$cell] <- k_clusters2$cluster
  
  k_raster2 %>%
    as.factor() %>%
    as.data.frame(xy = TRUE) %>%
    na.omit()%>%
    rename(cluster = CHELSA_bio10_01_V1.2)%>%
    mutate(cluster = as.factor(cluster))-> k_df2
  
  ggplot(data = k_df2) +
    geom_tile(aes(x = x, y = y, fill = cluster))+
    scale_fill_discrete()+
    geom_sf(data = domain,fill=NA)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  

  pam_clusters <- pam(x = scale(clim_vals[,1:3]),
                      k = 9,
                      keep.diss = FALSE,
                      diss = FALSE,
                      metric = "euclidean",
                      keep.data = FALSE)
  
  ?pam
  
  plot3d(x = clim_vals$bio1,
         y = clim_vals$bio12,
         z = clim_vals$bio15,
         xlab = "Temp",
         ylab = "Precip",
         zlab = "Prec. Seasonality",
         col = rainbow(n = length(unique(as.numeric(pam_clusters$clustering))))[as.numeric(pam_clusters$clustering)])
  
  
  
  # going to manually split up data, since I couldn't find a nice package
      
      
      

