#' Creates grid visualisation for feedback-report 
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @param output_format String ("svg" or "ggplot") defining whether output should be ggplot or svg
#' @return A ggplot-object ('grid'-visualisation)
#' @import ggplot2 dplyr forcats svglite tidyr
#' @export
viz_report_grid <- function(data, output_format = "svg") {

  if( is.character(data) ) { 
    return( list(error = data) )
  } else if( !is.data.frame(data) ) {
    return( list(error = "Input not a dataframe"))
  }
  
  # New names of all variables in grid
  grid_nms_mw <- c("Whatsapp", "Bellen", "Deurbel", "Smsen", "Afgezegd", "Werk_school_sport",
                   "Douchen", "Dag_nacht_ritme", "Eten", "Bewogen", "Recept_Medicatie",
                   "Medicatie_ingenomen", "Snijden", "Bonken_hoofd", "Krabben",
                   "Krassen", "Slaan_vuist", "Anderen_schade", "Spullen_kapot",
                   "Uitrekken_haren", "Alcohol_drugs", "Uitgeven_geld", "Gokken",
                   "Seksueel_risicovol", "Eetbui", "Passieve_gedachtes", "Actieve_gedachtes",
                   "Afscheidsbrief", "Concreet_plan", "TV", "Muziek", "Yoga", "Wandelen",
                   "Creatief", "Lezen", "Sporten", "Huishouden", "Spelletje", "Slapen",
                   "Contact_zoeken", "Afgesproken", "Buiten", "Anders", "Nagelbijten",
                   "Ijsberen", "Mouw_trekken", "Schoonmaken", "Roken", "Praten_anders",
                   "Uitpraten_niet", "Piekeren", "Dwanghandelingen", "Contact_veel",
                   "Alleen_niet", "Beslissingen_zelf", "Stemmen", "Schimmen", "Dissociaties",
                   "Achterdochtig", "Opdracht_krijgen", "Herbeleving", "Werken",
                   "Opleiding", "Kind_zorg", "Wassen", "Koken", "Schoonmaken_verplichting",
                   "Boodschappen", "Afspraken_nakomen", "Telefoon_onaardig", "Face_to_face_onaardig",
                   "Whatsapp_onaardig", "Social_media_onaardig", "Ademhalingsproblemen",
                   "Pijn_borst", "Hartkloppingen", "Misselijkheid", "Ontlasting",
                   "Buikpijn", "Hoofdpijn", "Duizeligheid", "Vermoeidheid", "Rugpijn",
                   "Spierpijn", "Tintelingen", "Zweten", "Veel_slapen", "Weinig_slapen",
                   "Nachtmerries")
  
  # Vector with variable names that get assigned colour "#CC79A7"
  neg <- c( "Whatsapp", "Bellen", "Deurbel", "Smsen", "Afgezegd", "Werk_school_sport", 
            "Snijden", "Bonken_hoofd", "Krabben", "Krassen", "Slaan_vuist", 
            "Anderen_schade", "Spullen_kapot", "Uitrekken_haren", "Alcohol_drugs", 
            "Uitgeven_geld", "Gokken", "Seksueel_risicovol", "Eetbui", "Passieve_gedachtes", 
            "Actieve_gedachtes", "Afscheidsbrief", "Concreet_plan", "Nagelbijten", 
            "Ijsberen", "Mouw_trekken", "Schoonmaken", "Roken", "Praten_anders", 
            "Uitpraten_niet", "Piekeren", "Dwanghandelingen", "Contact_veel", 
            "Alleen_niet", "Beslissingen_zelf", "Stemmen", "Schimmen", "Dissociaties", 
            "Achterdochtig", "Opdracht_krijgen", "Herbeleving", "Telefoon_onaardig", 
            "Face_to_face_onaardig", "Whatsapp_onaardig", "Social_media_onaardig", 
            "Ademhalingsproblemen", "Pijn_borst", "Hartkloppingen", "Misselijkheid", 
            "Ontlasting", "Buikpijn", "Hoofdpijn", "Duizeligheid", "Vermoeidheid", 
            "Rugpijn", "Spierpijn", "Tintelingen", "Zweten", "Veel_slapen", 
            "Weinig_slapen", "Nachtmerries" )
  
  # Vector with variable names that get assigned colour "#56B4E9"
  pos <- c("Douchen", "Dag_nacht_ritme", "Eten", "Bewogen", "Recept_Medicatie", 
           "Medicatie_ingenomen", "TV", "Muziek", "Yoga", "Wandelen", "Creatief", 
           "Lezen", "Sporten", "Huishouden", "Spelletje", "Slapen", "Contact_zoeken", 
           "Afgesproken", "Buiten", "Anders", "Werken", "Opleiding", "Kind_zorg", 
           "Wassen", "Koken", "Schoonmaken_verplichting", "Boodschappen", 
           "Afspraken_nakomen")

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

  # Create dataframe with counts of each behaviour in each fase
  data_for_plot <- data %>%
    select(Datum, all_of(grid_nms_mw), csp_dna_fase) %>%
    mutate(csp_dna_fase = factor(csp_dna_fase, levels = c(1, 2, 3, 4))) %>%
    tidyr::gather(all_of(grid_nms_mw), key = "Variabele", value = "Score") %>%
    filter(!is.na(csp_dna_fase)) %>%
    group_by(csp_dna_fase, Variabele, .drop = FALSE) %>%
    # Count how often behaviour occurs in each phase
    summarise(behaviour_sum = sum(Score, na.rm = T)) %>%
    # Add overall phase counts
    left_join(n_fase_grid, by = "csp_dna_fase") %>%
    # Create % behaviour in phase
    mutate(prop = 100 * behaviour_sum / n_fase) %>%
    # Add colours to behaviours
    mutate(clr = case_when(Variabele %in% neg ~ "#CC79A7",
                           Variabele %in% pos ~ "#56B4E9")) 
    
  # Create ggplot
  ggplot(data_for_plot, aes(x = forcats::fct_reorder(Variabele, prop), y = prop)) +
    geom_bar(stat = "identity", aes(fill = clr)) +
    scale_y_continuous(limits = c(0, 100), guide = guide_axis(n.dodge = 3)) +
    scale_fill_identity() +
    facet_grid(.~csp_dna_fase, scales = "free_x", labeller = to_string) +
    theme_linedraw() +
    labs(y = "%", title = "Frequentieverdeling voor gedragingen per 'fase'", x = NULL) +
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
    grid.draw(g)
  } else {
    # svg(file = "viz_report_grid.svg", height = 7.5, width = 5)
    # grid.draw(g)
    # dev.off()
    viz_string <- svglite::svgstring(fix_text_size = FALSE, standalone = FALSE, 
                                     height = 7.5, width = 5)
    plot(g)
    invisible(dev.off())
    # as.scalar function does not work
    # list(svg = as.scalar2(viz_string())) 
    #list(svg = (viz_string())) 
    list(svgs = list(grid = as.scalar2(as.character(viz_string()))))
  }
}
