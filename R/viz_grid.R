#' Create ggplot-visualisation of 'grid' with experienced behaviours
#'
#' @param data A dataframe in pre-specified format through "prepare_data"
#' @return A ggplot-object ('grid'-visualisation)
#' @import ggplot2 dplyr tidyr forcats
#' @export
viz_grid <- function(data) {

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

  # This creates a "long" rather than "wide" dataset, necessary for visualisation
  grid_df <- data %>%
    # Select variables
    select(Datum, all_of(grid_nms_mw), csp_dna_fase, dayno, pertwee) %>%
    # From wide to long dataformat
    gather(all_of(grid_nms_mw), key = "Variabele", value = "Score") %>%
    # Create new variables
    mutate(var_cat = case_when(Variabele %in% c("Whatsapp","Bellen","Deurbel","Smsen","Afgezegd","Werk_school_sport") ~ "Terugtrekken",
                               Variabele %in% c("Douchen","Dag_nacht_ritme" ,"Eten","Bewogen", "Recept_Medicatie","Medicatie_ingenomen") ~ "Zorg_zelf",
                               Variabele %in% c("Snijden","Bonken_hoofd","Krabben","Krassen","Slaan_vuist","Anderen_schade","Spullen_kapot","Uitrekken_haren",
                                                "Alcohol_drugs","Uitgeven_geld","Gokken","Seksueel_risicovol","Eetbui"  ) ~ "Destructief",
                               Variabele %in% c("Passieve_gedachtes","Actieve_gedachtes","Afscheidsbrief","Concreet_plan") ~ "Suicidaliteit",
                               Variabele %in% c("TV","Muziek","Yoga","Wandelen","Creatief","Lezen","Sporten","Huishouden","Spelletje",
                                                "Slapen","Contact_zoeken" ,"Afgesproken","Buiten", "Anders") ~ "Activiteiten",
                               Variabele %in% c("Nagelbijten","Ijsberen","Mouw_trekken","Schoonmaken","Roken","Praten_anders","Uitpraten_niet",
                                                "Piekeren","Dwanghandelingen","Contact_veel","Alleen_niet","Beslissingen_zelf") ~  "Onrustig" ,
                               Variabele %in% c("Stemmen", "Schimmen", "Dissociaties", "Achterdochtig", "Opdracht_krijgen",
                                                "Herbeleving" ) ~  "Bijzondere_ervaringen",
                               Variabele %in% c("Werken","Opleiding","Kind_zorg", "Wassen","Koken",
                                                "Schoonmaken_verplichting", "Boodschappen","Afspraken_nakomen" ) ~   "Verplichtingen",
                               Variabele %in% c("Telefoon_onaardig", "Face_to_face_onaardig", "Whatsapp_onaardig",
                                                "Social_media_onaardig") ~ "Negatief_contact",
                               Variabele %in% c("Ademhalingsproblemen","Pijn_borst","Hartkloppingen","Misselijkheid","Ontlasting","Buikpijn",
                                                "Hoofdpijn", "Duizeligheid","Vermoeidheid","Rugpijn","Spierpijn", "Tintelingen",
                                                "Zweten","Veel_slapen","Weinig_slapen", "Nachtmerries") ~ "Lichamelijke_klachten"),
           clr = case_when(var_cat %in% c("Terugtrekken", "Lichamelijke_klachten", "Bijzondere_ervaringen", "Onrustig","Suicidaliteit","Destructief","Negatief_contact") ~ "#CC79A7",
                           var_cat %in% c("Zorg_zelf", "Verplichtingen", "Activiteiten") ~ "#56B4E9"), # This adds colouring variable
           var_cat = fct_inorder(var_cat),
           Var2 = fct_inorder(Variabele),
           Score = as.numeric(Score)) %>%
    # Remove missing values for variable clr
    filter(!is.na(clr)) -> grid_df

  # Creating order in factor
  grid_df %>%
    group_by(var_cat) %>%
    mutate(y = dense_rank(as.numeric(Var2))) -> grid_df

  ggplot(grid_df, aes(x = var_cat, y = y)) +
    geom_blank() +
    geom_point(data = filter(grid_df, Score == 1),
               aes(x = var_cat, y = y, colour = clr), size = 8, inherit.aes = FALSE) +
    geom_text(data = filter(grid_df, Score == 1),
              aes(x = var_cat, y = y, label = tolower(substr(Variabele, 1, 3))), size = 3,
              colour = "white", angle = 45, inherit.aes = FALSE) +
    theme_classic() +
    scale_colour_identity() +
    coord_flip() +
    scale_x_discrete(limits = rev(c("Terugtrekken", "Lichamelijke_klachten", "Bijzondere_ervaringen", "Onrustig","Suicidaliteit","Destructief","Negatief_contact","Zorg_zelf", "Verplichtingen", "Activiteiten"))) +
    scale_y_continuous(breaks = c(1:50)) +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.5))  +
    facet_wrap(as.formula(paste("~", "factor(Datum, ordered = TRUE)")), nrow = 1)
}
