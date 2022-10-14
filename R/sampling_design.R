#' @description Sampling design for BIOSCAPE
#' @author Brian Maitner

# Load packages
library(googledrive)
library(terra)
library(ggplot2)
library(stars)
library(sf)
library(movecost) #https://www.sciencedirect.com/science/article/pii/S2352711019302341#fig2
library(piggyback)
library(ClimDatDownloadR)
library(rgee)
source("R/get_park_polygons.R")
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
source("R/count_spei_anomalies.R")


#########################################################################

# Get basic spatial data (domain, park areas, roads) (if needed)
  
  if(!file.exists("data/output/sampling_options_near_roads.gpkg")|
     !file.exists("data/output/sampling_options.gpkg")|
     !file.exists("data/output/domain.gpkg")){
    
    # Get the full potential domain
    
      pb_download(file = "domain.gpkg",
                  dest = 'temp/',
                  repo = "AdamWilsonLab/emma_envdata",
                  tag = "raw_static")
      
      domain <- read_sf("temp/domain.gpkg")
    
    
    # Get necessary road and park data
    
      roads <- read_sf("data/manual_downloads/Roads/cfr_roads.gpkg")
      parks <- get_park_polygons()
    
    # combine park polygons into one set by chucking extraneous data
    
      parks <-
        parks$cape_nature %>%
        dplyr::select(RESERVENAM) %>%
        rename(park_name = RESERVENAM) %>%
        bind_rows(parks$national_parks %>% 
                    dplyr::select(CUR_NME) %>%
                    rename(park_name = CUR_NME)) %>%
        st_transform(st_crs(domain)) %>%
        st_intersection(domain) %>%
        select(-domain)
      
      
    # Buffer parks by ~10km (this is to avoid edge artifacts of roads near but outside parks)
      
    # Crop roads to buffered parks
      
      roads <- 
        roads %>%
        st_transform(crs = st_crs(domain)) %>%
        dplyr::select(FEAT_TYPE) %>%
        st_intersection(y = parks %>%
                          st_buffer(10000))
      
    # Buffer cropped roads to ~1km (this is the maximum walking distance to consider - we can change it later if needed).
      # then keep only the roads within a park
      
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
        st_buffer(dist = 1000) %>%
        st_union() %>%
        st_intersection(parks) %>%
        st_union() -> road_buffer
    
      plot(road_buffer)
      
    # save buffered roads for later use  
      
      st_write(obj = road_buffer,
               dsn = "data/output/sampling_options_near_roads.gpkg",
               append = FALSE)
      
      # save cropped parks for later use              
      st_write(obj = parks,
               dsn = "data/output/sampling_options.gpkg",
               append = FALSE)
      
      # save domain
      # save cropped parks for later use              
      st_write(obj = domain,
               dsn = "data/output/domain.gpkg",
               append = FALSE)
  }else{
    
    domain <- st_read("data/output/domain.gpkg")
    parks <- st_read("data/output/sampling_options.gpkg")
    road_buffer <- st_read("data/output/sampling_options_near_roads.gpkg")
    roads <- st_read("data/manual_downloads/Roads/cfr_roads.gpkg") %>%
      st_transform(crs = st_crs(domain)) %>%
      dplyr::select(FEAT_TYPE)
    
  }
    
    
  
#########################################################################
  
# Calculate movement cost (if needed)
  

  if(!file.exists("data/output/distance_h_to_sites.tif")){
  
    # Pull elevation data from envdata releases  
    
      pb_download(file = "nasadem.tif",
                  dest = "temp/",
                  repo = "AdamWilsonLab/emma_envdata",
                  tag = "raw_static",
                  overwrite = TRUE)
      
      nasadem <- rast("temp/nasadem.tif") %>%
        mask(x = nasadem,
             mask = vect(domain)) %>% # mask domain to get rid of 0 values that should be NA
        terra::project(
          y = st_crs(domain)$proj4string) %>% #reproject to match domain
        mask(vect(road_buffer)) %>% #mask to road buffer
        crop(as_Spatial(road_buffer)) -> masked_dem
    
    # cut roads down to those in the road buffer
    
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
      
      #remove spandrels
        rm(nasadem, roads)
  
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
    
    writeRaster(x = movecost_rast_out,
                filename = "data/output/distance_h_to_sites.tif",
                overwrite = FALSE)
    
    # publish sampling distances as a raster
    
    pb_upload(file = "data/output/distance_h_to_sites.tif",
              repo = "BioSCape-io/terrestrial_sampling",
              tag = "current")  
    
  }else{
    
    movecost_rast_out <- terra::rast("data/output/distance_h_to_sites.tif")
    
  }
  
  
#########################################################################
  

# Filtering NDVI

  # Get an inventory of available data
  
    env_files <- pb_list("AdamWilsonLab/emma_envdata")  

  # pull most recent NDVI
  
    env_files %>%
      filter(tag == "raw_ndvi_modis") %>%
      arrange(file_name) %>%
      filter(file_name != "log.csv") %>%
      #slice_tail(n = 24)%>%
      pull(file_name) %>%
      robust_pb_download(dest = "temp/recent_ndvi",
                         repo = "AdamWilsonLab/emma_envdata",
                         tag = "raw_ndvi_modis",
                         overwrite = TRUE,
                         max_attempts = 10,
                         sleep_time = 10)

  # load ndvi and do necessary transforms  

    recent_ndvi <- ((terra::rast(list.files("temp/recent_ndvi/",full.names = TRUE))/100)-1)

  # take mean ndvi over the last 12 months or so

    recent_ndvi <- terra::app(x = recent_ndvi,
                            fun = function(x){mean(na.omit(x))})

    names(recent_ndvi) <- "NDVI"

  # select only sites with mean NDVI > 0.2
    
    focal_sites_good_ndvi <-
      recent_ndvi %>%
      mask(road_buffer %>%
             st_transform(crs = st_crs(recent_ndvi)) %>%
             vect()) %>%
      mask(recent_ndvi >= 0.2, maskvalues=0, updatevalue = NA)%>%
      terra::as.polygons() %>%
      st_as_sf()%>%
      st_transform(crs = crs(domain))
    
#########################################################################
    
# Filtering for slope
    
    
  if(!file.exists("data/output/flat_polygons.gpkg")){
    
      #get DEM
        robust_pb_download(file = "nasadem.tif",
                           dest = "temp/nasadem",
                           repo = "AdamWilsonLab/emma_envdata",
                           tag = "raw_static",
                           overwrite = TRUE,
                           max_attempts = 10,
                           sleep_time = 30)

      #load dem
        dem <- terra::rast("temp/nasadem/nasadem.tif")
        plot(dem)

      # cut down to only the parks(and maybe a small buffer for edge effects) to save memory
        dem <-
        dem %>%
          terra::mask(mask = sampling_locations %>%
                 st_buffer(dist = 1000) %>%
                 st_transform(crs = st_crs(dem)) %>%
                 vect())

        # terra::tmpFiles(current = FALSE,
        #                 orphan = TRUE,
        #                 old = TRUE,
        #                 remove = TRUE)


      # Need DEM in units of meters, so have to transform
        dem <- terra::project(dem, y = crs(domain))

        #need to break up dem to avoid "cannot allocate vector of X Gb" warning

        poly_for_dem <-
          sampling_locations %>%
            st_buffer(dist = 1000) %>%
            st_transform(crs = st_crs(dem)) %>%
            st_union() %>%
          st_make_valid() %>%
          st_cast("POLYGON") %>%
          as.data.frame()

      for(i in 1:nrow(poly_for_dem)){

        poly_i <- poly_for_dem[i,]

        dem_i <- dem %>%
          mask(mask = vect(poly_i))

        dem_i <-
          dem_i %>%
          crop(poly_i)

        #plot(dem_i)

        #calc slope using insol package

          slope_insol <- insol::slope(cgrad(raster::raster(dem_i)), degrees = TRUE)
          slope_insol <- raster::raster(slope_insol,
                                       crs=raster::projection(dem_i))
          raster::extent(slope_insol) <- raster::extent(raster::raster(dem_i))
          plot(slope_insol)


        #calc slop using stars package

          slope_stars <- starsExtra::slope(x = stars::st_as_stars(dem_i,proxy=FALSE))
          slope_stars <- rast(slope_stars)
          #plot(slope_stars)

        if(i == 1){

          stars_slope <- slope_stars
          insol_slope <- slope_insol

        }else{

          stars_slope <- terra::merge(stars_slope, slope_stars)
          insol_slope <- terra::merge(insol_slope, slope_insol)

        }

        rm(slope_stars, slope_insol, dem_i, poly_i)

        #clear out temp files to keep hd from filling up
        terra::tmpFiles(current = FALSE,
                        orphan = TRUE,
                        old = TRUE,
                        remove = TRUE)

      }

    gc()

    # writeRaster(x = insol_slope, filename = "data/output/insol_slope.tif")
    # writeRaster(x = stars_slope, filename = "data/output/stars_slope.tif")

    insol_slope <- rast("data/output/insol_slope.tif")
    stars_slope <- rast("data/output/stars_slope.tif")

    #make a polygon of sites that have a reasonable slope in each raster (< 30 degrees)

      insol_slope <- (insol_slope < 30)
      insol_slope[which(is.na(values(insol_slope)))] <- 0

      stars_slope <- (stars_slope < 30)
      stars_slope[which(is.na(values(stars_slope)))] <- 0

      #insol doesn't keep the crs, so add it in here
        crs(insol_slope) <- crs(stars_slope)

      combined_slope <- insol_slope + stars_slope

      #plot(combined_slope) #pretty good correspondence. we'll keep any that meet the criteria using either approach
      combined_slope <- (combined_slope > 0)
      combined_slope[which(values(combined_slope)==0)] <- NA


      plot(combined_slope)

      slope_poly <-
        combined_slope %>%
          terra::as.polygons(trunc = TRUE,
                      dissolve = TRUE,
                      values = TRUE) %>%
        st_as_sf()

            st_write(obj = slope_poly,
                    dsn = "data/output/flat_polygons.gpkg")
            
  }else{
    
    slope_poly <- st_read("data/output/flat_polygons.gpkg")
    
    
  }
    
    #add a slight buffer at a bit larger than pixel size so that we cut out areas where its just a pixel or two of steep terrain.
    slope__poly_buffered <-
      slope_poly %>%
      st_buffer(dist = 40)

#########################################################################
    
# Omit sites > 2 hours from the road
    
  time_to_sites <- rast("data/output/distance_h_to_sites.tif")
    
  time_to_sites <- (time_to_sites < 2)
  time_to_sites[time_to_sites == 0] <- NA
  
  time_to_sites %>%
    terra::as.polygons() %>%
    st_as_sf()-> time_to_sites
  
#########################################################################
  
# Put it all together
  
  # put it all together:
  
    #select from sites in parks and within 1 km of a road
    road_buffer %>%
    
    #only include sites with an ndvi > 0.2
    st_intersection(y = st_make_valid(focal_sites_good_ndvi)) %>%
    
    #only include sites that have a slope < 30 degrees
    st_intersection(y = slope__poly_buffered) %>%  
    
    # only include sites within 2 hours of the road
    st_intersection(y= time_to_sites %>% st_make_valid()) %>%  
    
    #union and create a new object
    st_union() -> acceptable_sites
  
  #plot(acceptable_sites)
  
  # acceptable_sites%>%
  # st_write(dsn = "data/output/acceptable_sites.shp",
  #          append=FALSE)
  # 
  # acceptable_sites%>%
  #   st_write(dsn = "data/output/acceptable_sites.gpkg",
  #            append=FALSE)
    
#########################################################################

  # PUll environmental data for clustering
  
  
  # Climate  
      
    # get climate data
  
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
        terra::project(y = crs(domain)) -> climate
  

  # water distance
    
    if( !file.exists("data/output/distance_from_freshwater.tif"))  {
      
    #load data
      rivers <- st_read("data/manual_downloads/NFEPA/NFEPA_Rivers.shp")
      wetlands <- st_read("data/manual_downloads/NFEPA/NFEPA_Wetlands.shp")
      
    
    #aggregate to coarser resolution (using the same resolution as the elevation layers)
    
    rivers %>%
      #filter(grepl(pattern = "P",x = RIVTYPE))%>% #uncomment this to only include permanent rivers
      st_transform(crs = crs(movecost_rast_out)) %>%
      vect() %>%
      terra::rasterize(y = movecost_rast_out,
                       touches = TRUE) -> rivers
    
    wetlands %>%
      st_transform(crs = crs(movecost_rast_out)) %>%
      vect() %>%
      terra::rasterize(y = movecost_rast_out,
                       touches = TRUE) -> wetlands
    
    c(wetlands,rivers) %>%
      app(function(x){
        if(any(!is.na(x))){1}else{NA}
      }) -> water
    
      water %>%
      terra::distance() -> dist_from_water
  
      terra::writeRaster(x = dist_from_water,
                         filename = "data/output/distance_from_freshwater.tif",overwrite=TRUE)
    
    }else{
    
      dist_from_water <- rast("data/output/distance_from_freshwater.tif")  
      
    }
    
  # Drought  

    if(!file.exists("data/output/drought.tif")){
      
      list.files(path = "data/manual_downloads/drought/",full.names = TRUE) %>%
        count_spei_anomalies(thresh = -1,
                             domain = domain) %>%
        resample(y = dist_from_water) -> drought
      
      terra::writeRaster(x = drought,
                         filename = "data/output/drought.tif",overwrite=TRUE)
      
      
    }else{
      
      drought <- terra::rast("data/output/drought.tif")
      
    }  
      
  # Soil Depth

      if(!file.exists("data/output/soil_depth_ISDASOIL.tif")){
      
        
        soil_depth <- ee$Image("ISDASOIL/Africa/v1/bedrock_depth")
  
        #Format the domain
        
        domain_ee <- sf_as_ee(x = domain)
        domain_ee <- domain_ee$geometry()
  
        soil_depth_raster <- ee_as_raster(image = soil_depth,
                                         region = domain_ee,
                                         dsn = "data/temp/soil",
                                         maxPixels = 2000000000)
  
        writeRaster(x = soil_depth_raster,
                    filename = "data/temp/soil_depth_ISDASOIL.tif")
  
            soil_depth_raster <- rast("data/output/soil_depth_ISDASOIL.tif")
  
            soil_depth_raster %>%
              terra::resample(y = recent_fires) -> soil_depth_raster_resamp
  
            plot(soil_depth_raster_resamp)
            plot(soil_depth_raster_resamp[[1]],main="Soil Depth (cm)")
  
            writeRaster(x = soil_depth_raster_resamp,
                        filename = "data/output/soil_depth_ISDASOIL.tif")
            
      }else{
        
        soil_depth_raster <- rast("data/output/soil_depth_ISDASOIL.tif")

      }

      
      
    
  
#########################################################################    
#cleanup
  
  terra::tmpFiles(current = FALSE,
                orphan = TRUE,
                old = TRUE,
                remove = TRUE)
  gc()
  