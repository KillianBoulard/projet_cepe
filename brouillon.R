test_incendies = incendies %>%
  mutate(
    date_alerte = as.Date(date_alerte),
    dir_vent = as.character(dir_vent, na.rm= T),
    v_moyenn_vent = as.double(v_moyenn_vent, na.rm= T),
    date_m_1 = as.Date(date_alerte, format = "%d/%m/%Y") %m+% months(-1),
    date_m_12 = as.Date(date_alerte, format = "%d/%m/%Y") %m+% months(-12),
    
    #Formattage des dates pour future jointure m-1 et m-12
    annee = format(date_alerte," %Y"),
    mois = format(date_alerte," %m"),
  
  group_by(
    annee, mois, code_insee) %>%
  
  mutate(
    occurence_commune_mois = n(),
    surface_parcourue = sum(surface_parcourue),
    
    #Poids des différentes origines
    nb_origine_indetermine = sum(origine_alerte == "Indéterminé") / n(),
    nb_origine_autre = sum(origine_alerte=="Autre", na.rm=TRUE) / n(),
    nb_origine_moy_aer = sum(origine_alerte=="Moyen aérien", na.rm=TRUE) / n(),
    nb_origine_patrouille = sum(origine_alerte=="Patrouille", na.rm=TRUE) / n(),
    nb_origine_population = sum(origine_alerte=="Population", na.rm=TRUE) / n(),
    nb_origine_pomp_pre_pos = sum(origine_alerte=="Pompiers pré-positionnés", na.rm=TRUE) / n(),
    nb_origine_cam = sum(origine_alerte=="Vigie-caméra", na.rm=TRUE) / n(),
    
    #Poids des différentes natures
    nb_nat_accidentelle = sum(nature == "Accidentelle" , na.rm = T) / n(),
    nb_nat_inv_part = sum(nature == "Involontaire (particulier)", na.rm = T) / n(),
    nb_nat_inv_trav = sum(nature == "Involontaire (travaux)", na.rm = T) / n(),
    nb_nat_malv = sum(nature == "Malveillance", na.rm = T) / n(),
    nb_nat_naturelle = sum(nature == "Naturelle", na.rm = T) /n()
    
  ) %>%
  distinct(
    annee, mois, annee_m_1, mois_m_1, annee_m_12, mois_m_12, departement, nom_commune, code_insee,
    surface_parcourue, occurence_commune_mois, 
    #Origine
    nb_origine_indetermine, nb_origine_autre,
    nb_origine_moy_aer, nb_origine_patrouille, nb_origine_pomp_pre_pos, 
    nb_origine_cam, nb_origine_population,
    #Nature
    nb_nat_accidentelle, nb_nat_inv_part, nb_nat_inv_trav, nb_nat_malv, nb_nat_naturelle) %>%
  
  select(
    annee, mois, annee_m_1, mois_m_1, annee_m_12, mois_m_12, departement, nom_commune, code_insee,
    surface_parcourue, occurence_commune_mois,
    #Origine
    nb_origine_indetermine, nb_origine_autre,
    nb_origine_moy_aer, nb_origine_patrouille, nb_origine_pomp_pre_pos,
    nb_origine_cam, nb_origine_population,
    #Nature
    nb_nat_accidentelle, nb_nat_inv_part, nb_nat_inv_trav, nb_nat_malv, nb_nat_naturelle)

