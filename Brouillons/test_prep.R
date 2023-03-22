transpose_by_station_annee_mois = METEO_QUANTI %>%
  group_by(id_station, annee_mesure, mois_mesure) %>%
  select(-date_mesure) %>% 
  pivot_wider(
    id_cols = c(id_station, annee_mesure, mois_mesure),
    names_from = heure_mesure,
    values_from = c(pression_niveau_mer, var_pression_3h, vitesse_vent_moyen_10m, temperature, 
                    point_rosee, humidite, visibilite_horizontale, pression_station,rafales_periode ,
                    periode_mesure_rafale, precipiation_derh, precipitation_3dh, precipitation_6dh,
                    precipitation_12dh),
    values_fn = mean) %>% 
  ungroup() %>% 
  mutate(id_station = as.numeric(levels(id_station))[id_station])


transpose_by_station_annee_mois_gen <- transpose_by_station_annee_mois %>% 
  group_by(id_station) %>%
  arrange(annee_mesure,mois_mesure) %>% 
  mutate (
    mean_ant_temp_23H = lag(round(cummean(transpose_by_station_annee_mois_gen[[4]]),3)),
    
  )  %>%
  
  distinct(id_station,annee_mesure,mois_mesure)
  ungroup()
