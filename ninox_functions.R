library(lubridate)
library(dplyr)
library(ggplot2)

#### Constantes
# Altitude du soleil au moment de la mesure *10 en degré pour filtrer les données
SUN_ALT_MIN <- -250
# Valeur mininale de sqm_mag utilisée pour filtrer les données
MIN_SQM_MAG_VAL <- 21
# Valeur maximale de sqm_mag utilisée pour filtrer les données
MAX_SQM_MAG_VAL <- 22.2
# Différence maximale entre sqm_mag min et max par nuit utilisée pour filtrer les données
DIFF_SQM_MAG <- 1

process_all <- function(file_name, nom_site, sun_alt_min = SUN_ALT_MIN, diff_sqm_mag = DIFF_SQM_MAG, min_sqm_mag_val = MIN_SQM_MAG_VAL, max_sqm_mag_val = MAX_SQM_MAG_VAL) {
  # Chargement et prétraitrement du fichier
  all_data <- load_and_process_file(file_name)
  # Selection des meilleurs nuits
  best_night <- get_best_night(all_data, nb_flat_day = NULL, nb_best_day = NULL,  sun_alt_min, diff_sqm_mag, min_sqm_mag_val, max_sqm_mag_val)

  flat_night <- get_best_night(all_data, nb_flat_day = 10, nb_best_day = NULL, sun_alt_min, diff_sqm_mag, min_sqm_mag_val, max_sqm_mag_val)
  the_best_night<- get_best_night(all_data, nb_flat_day = 10, nb_best_day = 1, sun_alt_min, diff_sqm_mag, min_sqm_mag_val, max_sqm_mag_val)


  # # #########################################
  # # Rapport du lot de données
  report_path <- sprintf("%s.txt", nom_site)
  # moyenne :
  mean <- mean(all_data$sqm_mag)
  # mediane :
  median <- median(all_data$sqm_mag)
  # Modal
  sqm_mag_mod <- get_modal_sqm_mag_value(all_data)
  # CALCUL DU nombre jour avec des données :
  n_day <- n_distinct(all_data$ymd)

  report_txt <- c(
    sprintf("SITE : %s", nom_site),
    sprintf("FICHIER : %s", file_name),
    sprintf("MEAN : %s", round(mean,2)),
    sprintf("MEDIAN : %s",round( median,2)),
    sprintf("MODAL : %s", round(sqm_mag_mod, 2)),
    sprintf("Number of days with measurements : %s", n_day)
  )
  writeLines(report_txt, report_path)

  # Génération des graphiques
  generate_graph(best_night, nom_site, "", sqm_mag_mod)
  generate_graph(all_data, nom_site, "All data", sqm_mag_mod)

  generate_graph_density(the_best_night, nom_site, sprintf("%s", min(as.Date(the_best_night$date))))

  generate_graph_density_all_data(all_data, best_night, flat_night, the_best_night, nom_site )

}

load_and_process_file <- function(file_path) {
  # #########################################
  # CHARGEMENT DES DONNEES
  # chagerment du fichier données brutes
  data <- read.csv2(file_path, header = TRUE, sep = ",", dec = ".")

  ## enlever les colonnes sans intéret ou les ninox ne calcule
  # pas les valeurs Temp, humidité, pression etc...
  data <- subset(data, select = -c(az, alt, temp_sensor, temp_ambiant, humidity, pressure, cloud_cover, wind_speed))

  # convertir les jour julien en date normal en ajoutant une colonne
  data$date <- as.POSIXct(as.Date(data$jd_utc, origin = structure(-2440588 + 0.5, class = "Date")))

  # ensuite on va tout separer pour avoir des donnes simple a utiliser et pouvoir manipuler plus facilement
  # créer une colonne pour chaque objet (y,m,d...)
  date1 <- ymd_hms(data$date)
  data$y <- year(date1)
  data$m <- month(date1)
  data$d <- day(date1)
  data$h <- hour(date1)
  data$min <- minute(date1)
  data$sec <- second(date1)
  # calcul du "numéro de la nuit" basé sur des jours julien : jour de l'année(jour - 12h)
  data <- data %>% mutate(day_night = date - dhours(12))
  data <- data %>% mutate(julian_night = yday(date - dhours(12)))

  # créer des colonnes qui rassemble les infos année, mois, date
  # pour povoir sélectionner les données ensuite
  data$ymd <- paste(data$y, data$m, data$d, sep = "_")
  data$heurevrai <- paste(data$h, data$min, data$sec, sep = ":")
  data$heure_r <- as.POSIXlt(data$heurevrai, format = "%H:%M:%S")

  # cette commande permet de soustraire 1 jour au valeur comprise entre 23:59:59 et midi
  # comme ca à partir de minuit on passe à j+1 et le graph est continu
  data <- data %>%
    mutate(heure_graph = case_when(
      heure_r <= as.POSIXlt("23:59:59", format = "%H:%M:%S") & heure_r >= as.POSIXlt("12:00:00", format = "%H:%M:%S") ~ heure_r - ddays(1),
      TRUE ~ heure_r
    ))

  return(data)
}

get_modal_sqm_mag_value <- function(data_in) {
  # CALCUL DE LA VALEUR MODAL
  # mode :
  # permet d'arrondir les valeurs de sqm à 1 chiffres après la virgule
  sqm_mag_density <- density(data_in$sqm_mag)
  sqm_mag_mod <- sqm_mag_density$x[which.max(sqm_mag_density$y)]
  return(sqm_mag_mod)
}

generate_aggrate_per_night <- function(data_in) {
  # ################################
  # Génération d'un tableau d'aggrégation par nuit

  # group_by() night - c-ad julian_night
  grp_tbl <- data_in %>% group_by(julian_night)
  # min, max des sql_mag et (sum de la dérivé /nb données) par nuit
  agg_tbl <- grp_tbl %>%
    summarise(
      min = min(sqm_mag),
      max = max(sqm_mag),
      sum_deriv = sum(abs_derive, na.rm=TRUE) / n()
    )

  # Ordonnancement par la sum de la dérivée
  agg_tbl <- agg_tbl[order(agg_tbl$sum_deriv),]
  return(agg_tbl)
}

get_best_night <- function(data_in, nb_flat_day = NULL, nb_best_day = NULL, sun_alt_min = SUN_ALT_MIN, diff_sqm_mag = DIFF_SQM_MAG, min_sqm_mag_val = MIN_SQM_MAG_VAL, max_sqm_mag_val = MAX_SQM_MAG_VAL) {
  # #########################################
  # FILTRE : NUITS AVEC LES MEILLEURS MESURES
  # Filtrer les données pour avoir que les périodes de nuits complètes
  # Où sun_alt < - 250
  # Où la moyenne de la dérivée est la plus faible
  # Où les valeurs sqm_mag sont maximales

  data_without_sun <- subset(x = data_in, subset = sun_alt < sun_alt_min)

  # Ajout de la valeur absolue de la dérivée de sqm_mag (cia fonction diff) en fonction de la nuit
  data_without_sun <- transform(
      data_without_sun,
      abs_derive = ave(sqm_mag, julian_night, FUN = function(x) c(NA, abs(diff(x))))
  )

  agg_tbl <- generate_aggrate_per_night(data_without_sun)
  # Selection des nuits avec les "meilleurs" valeur :
  #   diff entre min et max < 1
  #   et valeurs comprises entre 21 et 22.8
  agg_tbl <- subset(x = agg_tbl, subset = (max - min < diff_sqm_mag) & (min > min_sqm_mag_val & max < max_sqm_mag_val))

  # Selection des données des nuit plus "plates"
  # Par défaut on selection toutes les nuits ayant les valeurs dans les bornes
  selection <- agg_tbl$julian_night
  # Si nb_flat_day
  # Selection des x nuits les plus "plates"
  if (! is.null(nb_flat_day)) {
    selection <-  (agg_tbl$julian_night)[1:nb_flat_day]
  }
  # Si nb_best_day
  # Sélection des x nuits avec les valeurs les plus fortes dans les nuits "plates"
  if (!is.null(nb_best_day)) {
    max_agg_night <- head(agg_tbl, nb_flat_day)
    max_agg_night <- max_agg_night[order(-max_agg_night$max),]
    selection <- (max_agg_night$julian_night)[1:nb_best_day]
  }
  best_night <- subset(
      x = data_in,
      subset = data_in$julian_night %in% selection
    )
  return(best_night)
}

generate_graph <- function(data_in, nom_site, sous_titre, valeur_modal) {
  generate_graph_density(data_in, nom_site, sous_titre)
  generate_graph_magnitude(data_in, nom_site, sous_titre, valeur_modal)
  generate_graph_density_2(data_in, nom_site, sous_titre)
}

generate_graph_density <- function(data_in, nom_site, sous_titre) {
  # ##############################################
  # GRAPHIQUES
  # Graphique  de densité : "meilleurs nuits"
  title_nb_nights <- ""
  if (n_distinct(data_in$julian_night) > 1) {
    title_nb_nights <- sprintf(
      " (%s nights)",
      n_distinct(data_in$julian_night)
    )
  }
  plot <- ggplot(data_in) +
    geom_point(aes(x = heure_graph, y = sqm_mag), color = "blue", size = 0.1) +
    scale_y_reverse(breaks = seq(23,16,-1),limits=c(23,16)) +
    ylab("NSB (magsqm/arsec²)") +
    xlab("Time (UTC)") +
    ggtitle(sprintf("NINOX %s %s %s", nom_site, sous_titre, title_nb_nights))

  ggsave(plot,
    filename = gsub(" ", "_", sprintf("%s_%s_densite.jpg", nom_site, sous_titre)),
    device = "jpg",
    height = 6, width = 5, units = "in"
  )
}

generate_graph_magnitude <- function(data_in, nom_site, sous_titre, valeur_modal) {
  # histogramme de magnitude :
  plot <- qplot(
    sqm_mag,
    data = data_in,
    geom = "histogram",
    fill = I("blue"),
    colour = I("black"),
    binwidth = 0.09
  ) + geom_vline(
    xintercept = valeur_modal,
    linetype = "dashed",
    size = 0.6
  ) + scale_x_continuous(
    breaks = seq(16, 23, 0.5),
    limits = c(16, 23)
  ) + xlab(
    "NSB (magsqm/arsec²)"
  ) + ggtitle(
    sprintf("NINOX %s %s (%s nights)", nom_site, sous_titre, n_distinct(data_in$julian_night)),
    subtitle=sprintf("Modal value = %s", round(valeur_modal,2))
  )
  ggsave(plot,
    filename = gsub(" ", "_", sprintf("%s_%s_magnitude.jpg", nom_site, sous_titre)),
    device = "jpg",
    height = 6, width = 5, units = "in"
  )
}

generate_graph_density_2 <- function(data_in, nom_site, sous_titre) {
  # diagramme de densité
  plot <- ggplot(data_in, aes(x = heure_graph, y = sqm_mag)) +
    geom_count(aes(color = after_stat(n), size = after_stat(n))) +
    guides(color = "legend") +
    scale_y_reverse(breaks = seq(23, 16, -1), limits = c(23, 16)) +
    ylab("NSB (magsqm/arsec²)") +
    xlab("Time (UTC)") +
    ggtitle(
      sprintf("NINOX %s %s (%s nights)", nom_site, sous_titre, n_distinct(data_in$julian_night))
    )
  scale_color_gradient(low = "red", high = "blue")

  ggsave(plot,
    filename = gsub(" ", "_", sprintf("%s_%s_densite2.jpg", nom_site, sous_titre)),
    device = "jpg",
    height = 6, width = 5, units = "in"
  )
}

generate_graph_density_all_data <- function(all_night, best_night, flat_night, the_best_night, nom_site, sous_titre="") {
  # diagramme de densité
  plot <- ggplot(best_night) +
  scale_y_reverse(limits=c(23,16)) +
    ylab("NSB (magsqm/arsec²)") +
    xlab("Time (UTC)") +
    ggtitle(
      sprintf("NINOX %s %s", nom_site, sous_titre),
    ) +
    geom_point(data=all_night, aes(x = heure_graph, y = sqm_mag, color='#d3d2d2'), size = 00.1)+
    geom_point(aes(x = heure_graph, y = sqm_mag, color = "red"), size = 00.1)+
    geom_point(data=flat_night, aes(x = heure_graph, y = sqm_mag, color = "green"),  size = 00.1) +
    geom_point(data=the_best_night, aes(x = heure_graph, y = sqm_mag, color='blue'), size = 00.1)   +
    scale_color_identity(name = "Nights",
                          breaks = c("#d3d2d2", "red", "green", "blue"),
                          labels = c("All", "Best", "Flat", "Most scored"),
                          guide = "legend")

  ggsave(plot,
    filename = gsub(" ", "_", sprintf("%s_%s_densite_all_data.jpg", nom_site, sous_titre)),
    device = "jpg",
    height = 6, width = 5, units = "in"
  )
}
