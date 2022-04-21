#' Creates first visualisation for feedback-report (response-rate)
#'
#' @param data A dataframe with raw data
#' @param output_format String ("svg" or "ggplot") defining whether output should be ggplot or svg
#' @return An svg string of visualisation
#' @import ggplot2 svglite
#' @export
viz_report_response <- function(data, output_format = "svg") {

  if( is.character(data) ) { 
    return( list(error = data) )
  } else if( !is.data.frame(data) ) {
    return( list(error = "Input not a dataframe"))
  }
  
  responded <- sum(is.na(data$csp_dna_non_response))
  total <- length(data$csp_dna_non_response)
  resp <- 100 * responded / total
  
  title <- paste0(responded, " uit ", total, " metingen ingevuld")
  
  g <- ggplot(data = data.frame(x = 0, y = resp), aes(x = x, y = y)) +
    geom_bar(data = data.frame(x = 0, y = 100), 
             stat = "identity", fill = "#4F71BE", colour = "darkgrey") +
    geom_bar(stat = "identity", fill = "#DF8344") +
    geom_text(aes(label = paste0(round(resp), "%")), 
                  hjust = 1, colour = "white", size = 8) +
    theme_void() +
    coord_flip() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18)
    )

  if(output_format == "ggplot") {
    g
  } else {
    # svg(file = "viz_report_response.svg", height = 1.5, width = 5)
    # plot(g)
    # dev.off() 
    # # list(svg = as.scalar2())
    viz_string <- svglite::svgstring(fix_text_size = FALSE, standalone = FALSE, 
                                     height = 1.5, width = 5)
    plot(g)
    invisible(dev.off())
    # as.scalar function does not work
    # list(svg = as.scalar2(viz_string())) 
    #list(svg = (viz_string())) 
    list(svgs = list(response = as.scalar2(as.character(viz_string()))))
    
  }
  
}
