#' Create images for slides
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param height height (dimension) of image in cm
#' @param width width (dimension) of image in cm
#' @return Multiple SVGs in string-format in list to be used in slider
#' @import patchwork ggplot2 svglite
#' @export
viz_slider <- function(data, height = 18.3, width = 36.8) {

  if( is.character(data) ) { 
    return( list(error = data) )
  } else if( !is.data.frame(data) ) {
    return( list(error = "Input not a dataframe"))
  }
  
  no_fig <- unique(data[["pertwee"]])
  no_fig <- no_fig[!is.na(no_fig)]
  data$interval_esm <-  data[["pertwee"]]

  suppressWarnings(
    ts1 <- viz_ts(data, left_right = "left")
  )
  suppressWarnings(
    ts2 <- viz_ts(data, left_right = "right")
  )
  
  slider_list <- list()
  
  for (i in no_fig) {
    
    # Creates relevant zoomed in version of data (specific time frame)
    data_zoom <- filter(data, pertwee ==  i)

    # Combine graphs for slider
    suppressWarnings(
      combined <- (ts1 + viz_zoom(data_zoom) | ts2 + viz_zoom(data_zoom) ) / viz_nw(data_zoom, output = "slider") / viz_grid(data_zoom) + plot_layout(ncol = 1, heights = c(1, 2, 2))
    )
    #ggsave(paste0("slider_", i,".svg"), combined, width = width, height = height, unit = "cm")
    #slider_list[[paste0("slider_", i)]] <- combined

    # Create string of svg
    viz_string <- svglite::svgstring(fix_text_size = FALSE, standalone = FALSE,
                                     width = width, height = height)
    suppressWarnings( plot(combined) )
    invisible(dev.off())
    
    # Add svg string to list    
    # as.scalar function does not work
    # slider_list[[paste0("svg_slider_", i)]] <- as.scalar2(viz_string())
    slider_list[[paste0("svg_slider_", i)]] <- as.scalar2(as.character(viz_string()))
  }
  #return(slider_list)
  return( list(svgs = list(slider = slider_list)) )
}
