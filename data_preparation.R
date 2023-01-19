#Les fichiers sont disponibles dans README.md
#Emplacement local des fichiers (à définir pour chaque utilisateur)
setwd("c:/Users/User/Desktop/Cours et documents/formation R ensae/DATA")

# Le dataset incendies comprend l'ensemble des incendies jour / jour 
# Jusqu'à la maille commune du 01/01/2010 au 31/12/2021
incendies <- read.csv(file="Incendies.csv", header = T, sep=";", skip = 6)

#Le dataset meteo comprend l'ensemble des données métérologiques de 
# 2010 à 2023
meteo <- read.csv(file="donnees-synop-essentielles-omm.csv", header = T, sep=";")

#Date processing en tibble
as_tibble(incendies)
as_tibble(meteo)

#Nettoyage et préparation du fichier des incendies 
#Renommage
incendies = rename(incendies,
    "annee" = Année,
    "numero" = Numéro,
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
  
#création d'un nouvel id par ordre chronologique et ajout d'une variable année / mois
new = incendies %>%
        mutate(
         date_incendie = as.Date(date_incendie, format = "%d/%m/%Y"),
         test = format(date_incendie,"%m"))

colnames(incendies)

