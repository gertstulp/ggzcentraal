#' Creates first visualisation for feedback-report (alone by phase)
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param output_format String ("svg" or "ggplot") defining whether output should be ggplot or svg
#' @return An svg of visualisation
#' @import dplyr ggplot2 svglite
#' @export
viz_report_alone <- function(data, output_format = "svg") {

  if( is.character(data) ) { 
    return( list(error = data) )
  } else if( !is.data.frame(data) ) {
    return( list(error = "Input not a dataframe"))
  }
  
  # This creates the dataframe for the plot
  # It contains percentages being alone per phase
  d <- data %>% 
    # Create new variable
    mutate(fase = factor(csp_dna_fase, levels = c(4, 3, 2, 1))) %>% 
    # Below lines of code groups data by csp_dna_55_a0 and fase
    # And calculates how often these combinations are occuring
    group_by(csp_dna_55_a0, fase, .drop = FALSE) %>% 
    summarise(n = n()) %>% 
    # Remove missing cases
    filter( !is.na(csp_dna_55_a0) & !is.na(fase) ) %>% 
    # For each group in csp_dna_55_a0 calculate new variable with percentages
    group_by(csp_dna_55_a0) %>% 
    mutate(perc = 100 * n / sum(n)) 
  
  # This creates the labels with sample sizes for the x-axes for the plot
  lbls_alone <- d %>% 
    group_by(csp_dna_55_a0) %>% 
    summarise(n_alone = sum(n)) %>% 
    mutate(n_tot = sum(n_alone),
           perc =  round(100 * n_alone / n_tot),
           label = case_when(csp_dna_55_a0 == 0 ~ "In gezelschap",
                             csp_dna_55_a0 == 1 ~ "Alleen"),
           lbl = paste0(label, " [", perc, "%]")) 
  
  # This creates the labels for the phases with sample sizes for panels of the plot
  lbls_fase <- d %>% 
    group_by(fase) %>% 
    summarise(n_fase = sum(n)) %>% 
    mutate(n_tot = sum(n_fase),
           perc =  round(100 * n_fase / n_tot),
           lbl = paste0(fase, " [", perc, "%]")) %>% 
    pull(lbl)

  g <- ggplot(d, aes(x = csp_dna_55_a0, y = perc, fill = fase)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_discrete(
      breaks = lbls_alone$csp_dna_55_a0, 
      labels = lbls_alone$lbl
    ) +
    scale_fill_manual(breaks = factor(c(1, 2, 3, 4)),
                      labels = rev(lbls_fase),
                      values = c("1" = "green3", "2" = "yellow2",
                                 "3" = "darkorange", "4" = "firebrick2")) +
    theme_minimal() +
    labs(x = NULL, y = "%") +
    theme(
      legend.position = "top",
      panel.grid = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      plot.margin = margin(r = 50)
    ) +
    coord_flip()
  
  
  if(output_format == "ggplot") {
    g
  } else {
    # svg(file = "viz_report_alone.svg", height = 2.5, width = 5)
    # print(g)
    # dev.off() 
    viz_string <- svglite::svgstring(fix_text_size = FALSE, standalone = FALSE, 
                                     height = 2.5, width = 5)
    plot(g)
    invisible(dev.off())
    # as.scalar function does not work
    # list(svg = as.scalar2(viz_string())) 
    # list(svg = (viz_string())) 
    list(svgs = list(alone = as.scalar2(as.character(viz_string()))))
  }
  
}
