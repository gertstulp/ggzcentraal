#' Creates average behaviour per phase for feedback-report
#'
#' @param data A dataframe with raw data
#' @param output_format String ("svg" or "ggplot") defining whether output should be ggplot or svg
#' @return An svg of visualisation
#' @import dplyr forcats ggplot2 tidyr grid
#' @export
viz_report_behaviour <- function(data, output_format = "svg") {

  if( is.character(data) ) { 
    return( list(error = data) )
  } else if( !is.data.frame(data) ) {
    return( list(error = "Input not a dataframe"))
  }
  
  # Vector with variable names of negative emotions
  negatief <- c("Bedroefd", "Boos", "Bang", "Energie", "Spanning", "Zelfbeeld",
                "Terugtrekken", "Destructief", "Suicidaliteit", "Onrustig", "Bijzondere_ervaringen",
                "Negatief_contact", "Lichamelijke_klachten", "Onplezierig")
  
  # Vector with variable names of positive emotions
  positief <- c("Blij", "Ondernemen", "Contact_behoefte", "Ervaren_controle",
                "Zorg_zelf", "Activiteiten", "Verplichtingen", "Plezierig")

  # Create long dataframe
  long <- data %>%
    select(all_of(c("Datum", "csp_dna_55a", "csp_dna_56a",
                    "csp_dna_57a", "csp_dna_77a", "csp_dna_78a","csp_dna_fase", "dayno", "pertwee",
                    positief, negatief))) %>%
    gather(all_of(c(positief, negatief)), key = "Var", value = "Score") %>%
    mutate(pos_neg = case_when(Var %in% positief ~ "positief",
                               Var %in% negatief ~ "negatief"),
           csp_dna_fase = factor(csp_dna_fase, levels = c(1, 2, 3, 4)))
  
  # Create dataframe with sample sizes for each fase
  n_fase_grid <- data %>%
    mutate(csp_dna_fase = factor(csp_dna_fase, levels = c(1, 2, 3, 4))) %>% 
    filter(!is.na(csp_dna_fase)) %>%
    group_by(csp_dna_fase, .drop = FALSE) %>% 
    summarise(n_fase = n()) %>% 
    mutate(labels = paste0(csp_dna_fase, " ( ", n_fase, " )"))
  
  # Vector with labels for facets
  to_string <- as_labeller(c(`1` = n_fase_grid$labels[1],
                             `2` = n_fase_grid$labels[2],
                             `3` = n_fase_grid$labels[3],
                             `4` = n_fase_grid$labels[4]))
  
  ggplot(filter(long, !is.na(csp_dna_fase)),
         aes(x = forcats::fct_reorder(Var, -Score, na.rm = T), y = Score,
             colour = pos_neg)) +
    geom_point(stat = "summary", fun = "mean", size = 4) +
    scale_colour_manual(breaks = c("positief", "negatief"), values = c("#CC79A7", "#56B4E9")) +
    guides(colour = "none") +
    scale_y_continuous(limits = c(0, 100), guide = guide_axis(n.dodge = 3)) +
    facet_grid(.~csp_dna_fase, labeller = to_string, drop = FALSE) +
    theme_linedraw() +
    labs(y = "gemiddelde score", title = "Gemiddelde score per gedraging per 'fase'", x = NULL) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 8)) +
    coord_flip() -> grid_gg # Store as ggplot object
  
  # This "hack" is needed to give facets different colours
  g <- ggplot_gtable(ggplot_build(grid_gg))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- c("green3", "yellow2", "darkorange", "firebrick2")
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  if(output_format == "ggplot") {
    grid::grid.draw(g)
  } else {
    # svg(file = "viz_report_behaviour.svg", height = 7.5, width = 5)
    # grid.draw(g)
    # dev.off()
    viz_string <- svglite::svgstring(fix_text_size = FALSE, standalone = FALSE, 
                                     height = 7.5, width = 5)
    plot(g)
    invisible(dev.off())
    # as.scalar function does not work
    # list(svg = as.scalar2(viz_string())) 
    #list(svg = (viz_string())) 
    list(svgs = list(behaviour = as.scalar2(as.character(viz_string()))))
  }

}
