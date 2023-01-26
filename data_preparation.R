#Les fichiers sont disponibles dans README.md
#Emplacement local des fichiers (à définir pour chaque utilisateur)
setwd("c:/Users/User/Desktop/Cours et documents/formation R ensae/DATA")
setwd("C:/Users/VOYK743/Desktop/Fichiers Perso/Formation ENSAI/Datasets")

# Le dataset incendies comprend l'ensemble des incendies jour / jour 
# Jusqu'à la maille commune du 01/01/2010 au 31/12/2021
incendies <- read.csv(file="Incendies.csv", header = T, sep=";", skip = 6,  encoding="UTF-8")

#Le dataset meteo comprend l'ensemble des données métérologiques de 
# 2010 à 2023
meteo <- read.csv(file="donnees-synop-essentielles-omm.csv", header = T, sep=";")

#Date processing en tibble
as_tibble(incendies)
as_tibble(meteo)

#Nettoyage et préparation du fichier des incenbdies 
#Renommage
incendies = rename(incendies,
    "annee" = Année,
    "numero" = Numéro,
    "departement" = Département,
    "code_insee" = Code.INSEE,
    "nom_commune" = Nom.de.la.commune,
    "date_alerte" = Date.de.première.alerte,
    "origine_alerte" = Origine.de.l.alerte,
    "moyens_premiere_intervention" = Moyens.de.première.intervention,
    "surface_parcourue" = Surface.parcourue..m2.,
    "surface_foret" = Surface.forêt..m2.,
    "surface_maquis" = Surface.maquis.garrigues..m2.,
    "surface_nat_autre_foret" = Autres.surfaces.naturelles.hors.forêt..m2.,
    "surface_agricole" = Surfaces.agricoles..m2.,
    "surface_autre_terre_boisee" =  Surface.autres.terres.boisées..m2.,
    "surface_non_boisee_nat" = Surfaces.non.boisées.naturelles..m2.,
    "surface_non_boisee_art" = Surfaces.non.boisées.artificialisées..m2.,
    "surface_non_boisee" = Surfaces.non.boisées..m2.,
    "precision_surf" = Précision.des.surfaces,
    "surface_feu_initiale" = Surface.de.feu.à.l.arrivée.des.secours...0.1.ha,
    "voie_caross_proche" = Voie.carrossable.la.plus.proche,
    "act_hab_proche" = Activité.ou.habitation.la.plus.proche,
    "type_peupl" = Type.de.peuplement,
    "connaissance" = Connaissance,
    "source_enquete" = Source.de.l.enquête,
    "nature" = Nature,
    "enterv_equipe" = Intervention.de.l.équipe.RCCI,
    "deces_bat_touches" = Décès.ou.bâtiments.touchés,
    "nb_deces" = Nombre.de.décès,
    "nb_bat_tot_detruit" = Nombre.de.bâtiments.totalement.détruits,
    "nb_bat_part_detruit" = Nombre.de.bâtiments.partiellement.détruits,
    "hygrometrie" = Hygrométrie,
    "v_moyenn_vent" = Vitesse.moyenne.du.vent,
    "dir_vent" = Direction.du.vent,
    "precision_donnee" = Précision.de.la.donnée,
    "presence_contour_valide" = Présence.d.un.contour.valide
  )
  
#Data prep : fichier indendie :
# Formattage des variables ;
# Mise en forme des données pouvant avoir une importance lors de la modelisation
test_incendies = incendies %>%
  mutate(
    date_alerte = as.Date(date_alerte, format = "%d/%m/%Y"),
    dir_vent = as.character(dir_vent, na.rm= T),
    v_moyenn_vent = as.double(v_moyenn_vent, na.rm= T),
    date_m_1 = as.Date(date_alerte, format = "%d/%m/%Y") %m+% months(-1),
    date_m_12 = as.Date(date_alerte, format = "%d/%m/%Y") %m+% months(-12),
    
    #Formattage des dates pour future jointure sur m-1 et m-12
    annee = format(date_alerte," %Y"),
    mois = format(date_alerte," %m"),
    annee_m_1 = format(date_m_1, "%Y"),
    mois_m_1 = format(date_m_1," %m"),
    annee_m_12 = format(date_m_12, "%Y"),
    mois_m_12 = format(date_m_12," %m")
    ) %>%
  
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


#Ajout / jointure pour ajouter les variables concernant les mois M-1 et M-12



summary(incendies)
colnames(test_incendies)
head(test_incendies)
