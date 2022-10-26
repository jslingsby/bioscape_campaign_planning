#' @param x as in terra::extract.  (raster) 
#' @param y as in terra::extract.  (vector)
#' @param batch_size number of layers to process at once
#' @param ... additional arguments passed to extract

batch_extract <- function(x,y,batch_size = 1000, ...){
  
  total_layers <- terra::nlyr(x)
  batches <- split(1:total_layers, ceiling(seq_along(1:total_layers)/batch_size))
  
  for( i in 1:length(batches)){
    
    out_i <- terra::extract(x = x[[unlist(batches[i])]],
                            y = y, ...)
    
    # out_i <- terra::extract(x = x[[unlist(batches[i])]],
    #                         y = y, exact=TRUE)
    gc()

    if(i == 1){
      
      out <- out_i
      
    }else{
      
      #inner join makes sense, but is MUCH more memory intense than bind
      # out %>%
      #   inner_join(out_i) -> out
      
      out %>%
      bind_cols(out_i[2:ncol(out_i)]%>%
                  select(-fraction))->out

      
      
    }

  }#end i loop
  
  return(out)
  
}
