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
    date_alerte = as.Date(date_alerte),
    dir_vent = as.character(dir_vent, na.rm= T),
    v_moyenn_vent = as.double(v_moyenn_vent, na.rm= T),
    date_m_1 = as.Date(date_alerte, format = "%d/%m/%Y") %m+% months(-1),
    date_m_12 = as.Date(date_alerte, format = "%d/%m/%Y") %m+% months(-12),
    annee = format(date_alerte," %Y"),
    mois = format(date_alerte," %m"),
    annee_m_1 = format(date_m_1, "%Y"),
    mois_m_1 = format(date_m_1," %m"),
    annee_m_12 = format(date_m_12, "%Y"),
    mois_m_12 = format(date_m_12," %m")) %>%
  group_by(
    annee, mois, code_insee) %>%
  mutate(
    occurence_commune_mois = n(),
    surface_parcourue = sum(surface_parcourue),
    nb_vent_N = sum(dir_vent=="N", na.rm=TRUE),
    nb_vent_S = sum(dir_vent=="S", na.rm=TRUE),
    nb_vent_O = sum(dir_vent=="O", na.rm=TRUE),
    nb_vent_E = sum(dir_vent=="E", na.rm=TRUE),
    nb_vent_SE = sum(dir_vent=="SE", na.rm=TRUE),
    nb_vent_SO = sum(dir_vent=="SO", na.rm=TRUE),
    nb_vent_NE = sum(dir_vent=="NE", na.rm=TRUE),
    nb_vent_NO = sum(dir_vent=="NO", na.rm=TRUE),
    vit_moy_vent_N = nb_vent_N / mean(v_moyenn_vent, dir_vent =="N", na.rm=TRUE),
    vit_moy_vent_S = sum(dir_vent=="S", na.rm=TRUE),
    vit_moy_vent_O = sum(dir_vent=="O", na.rm=TRUE),
    vit_moy_vent_E = sum(dir_vent=="E", na.rm=TRUE),
    vit_moy_vent_SE = sum(dir_vent=="SE", na.rm=TRUE),
    vit_moy_vent_SO = sum(dir_vent=="SO", na.rm=TRUE),
    vit_moy_vent_NE = sum(dir_vent=="NE", na.rm=TRUE),
    vit_moy_vent_NO = sum(dir_vent=="NO", na.rm=TRUE)
  ) %>%
  distinct(
    annee, mois, annee_m_1, mois_m_1, annee_m_12, mois_m_12, departement, nom_commune, code_insee,
    surface_parcourue, occurence_commune_mois, nb_vent_N) %>%
  select(
    annee, mois, annee_m_1, mois_m_1, annee_m_12, mois_m_12, departement, nom_commune, code_insee,
    surface_parcourue, occurence_commune_mois, nb_vent_N)


#Ajout / jointure pour ajouter les variables concernant les mois M-1 et M-12

incendies_vent <- mutate(incendies_vent,
                         nb_vent_N = ifelse(dir_ven=="N",1,0),
                         nb_vent_S = ifelse(dir_ven=="S",1,0),
                         nb_vent_O = ifelse(dir_ven=="O",1,0),
                         nb_vent_E = ifelse(dir_ven=="E",1,0),
                         nb_vent_SE = ifelse(dir_ven=="SE",1,0),
                         nb_vent_SO = ifelse(dir_ven=="SO",1,0),
                         nb_vent_NE = ifelse(dir_ven=="NE",1,0),
                         nb_vent_NO = ifelse(dir_ven=="NO",1,0)
)

incendies_vent <- incendies_vent %>% 
  group_by(annee,code_insee) %>% 
  mutate ( moyenne_vent =mean(v_moyenn_vent),
           somme_vent=sum(nb_vent_N)+sum(nb_vent_S)+sum(nb_vent_O)+sum(nb_vent_E)+sum(nb_vent_SE)+
             sum(nb_vent_SO)+sum(nb_vent_NE)+sum(nb_vent_NO),
           nb_vent_N=sum(nb_vent_N)/somme_vent,
           nb_vent_S=sum(nb_vent_S)/somme_vent,
           nb_vent_O=sum(nb_vent_O)/somme_vent,
           nb_vent_E =sum(nb_vent_E)/somme_vent,
           nb_vent_SE =sum(nb_vent_SE)/somme_vent,
           nb_vent_SO =sum(nb_vent_SO)/somme_vent,
           nb_vent_NE =sum(nb_vent_NE)/somme_vent,
           nb_vent_NO =sum(nb_vent_NO)/somme_vent) %>% 
  select(annee,code_insee,moyenne_vent,somme_vent,nb_vent_N,nb_vent_S,
         nb_vent_O,nb_vent_E ,nb_vent_SE ,nb_vent_SO ,nb_vent_NE ,nb_vent_NO ) %>% slice(1)



incendies2B009<-incendies_vent %>% filter(code_insee=="2B009")  %>% 
  group_by(annee,code_insee) %>%
  summarise(moyenne_vent2=mean(v_moyenn_vent)) %>% select(annee,code_insee,moyenne_vent2) 



summary(incendies)

incendies_vent %>% explore_all(n = n)




colnames(test_incendies)
head(test_incendies)
