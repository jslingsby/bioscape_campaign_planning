#code to count spei anomalies

count_spei_anomalies <- function(file_list = list.files("data/manual_downloads/drought/",full.names = TRUE),
                            thresh = -1,
                            domain){
  
  
  

  for(i in 1:length(file_list)){
    
    print(i)
    rast_i <- rast(file_list[i])
    
    crs(rast_i) <-    crs(raster::raster(file_list[i]))
    
    rast_i %>%
    terra::crop(y = domain %>%
                  st_transform(crs = crs(rast_i))%>%
                  ext())->rast_i 
    
    
    #plot(rast_i)

      rast_i <-   rast_i <= thresh
  
      rast_i <- sum(rast_i)
      plot(rast_i)
      
    if(i == 1){
      
      out <- rast_i
    }else{
      
      out <- out + rast_i
    } 

  }# end i loop
  
  
  return(out)  
  
  
} #end fx
