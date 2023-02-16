#### statsistiques détaillées pour les communes
library(csvread)
library(ggplot2)
library(dplyr)
# Charger
# library(wesanderson)
# library(explore)
library(rpart)
library(ggthemes)

#travail
setwd("C:/Users/bigas/Documents/laurent/formation_cepe/projets_MachineLearning/data")


#taff
setwd("C:/Users/vhle524/OneDrive - LA POSTE GROUPE/Documents/projetcepe/data")


incendies<- read.csv(file="Incendies.csv",
                     col.names = c("annee","id","departement","code_insee",
                                   "nom_commune","date_alerte","origine_alerte","moyens_premiere_intervention",
                                   "surface_parcourue","surface_foret","surface_maquis",
                                   "surface_nat_autre_foret","surface_agricole","surface_autre_terre_boisee","surface_non_boisee_nat",
                                   "surface_non_boisee_art","suface_non_boisee","precision_surf","surface_feu_initiale",
                                   "voie_caross_proche","act_hab_proche","type_peupl","connaissance","source_enquete",
                                   "nature","interv_equipe","deces_bat_touches",
                                   "nb_deces","nb_bat_tot_detruit","nb_bat_part_detruit","hygrometrie",
                                   "v_moyenn_vent","dir_ven","temperature","precision_donnee","presence_contour_valide"),
                     header = T, 
                     sep=";",
                     encoding='UTF-8',skip = 6)

#correction des champs texte et transformation en vecteurs si nécessaire ##
incendies$origine_alerte <- gsub('IndÃ©terminÃ©', 'Indetermine', incendies$origine_alerte)
incendies$origine_alerte <- gsub('é', 'e', incendies$origine_alerte)

incendies$precision_surf <- gsub('EstimÃ©es', 'Estimees', incendies$precision_surf)
incendies$precision_surf <- gsub('é', 'e', incendies$precision_surf)

incendies$origine_alerte <- gsub('Pompiers prÃ©-positionnÃ©s', 'Pompiers pre-pos', incendies$origine_alerte)
incendies$origine_alerte <- gsub('Vigie-camÃ©ra', 'Vigie-cameras', incendies$origine_alerte)
incendies$origine_alerte <- gsub('é', 'e', incendies$origine_alerte)

incendies$precision_surf <- gsub('MesurÃ©es', 'Mesuree', incendies$precision_surf )
incendies$precision_surf <- gsub('é', 'e', incendies$precision_surf)

incendies$origine_alerte <-as.factor(incendies$origine_alerte)
incendies$precision_surf <-as.factor(incendies$precision_surf)
incendies$origine_alerte <-as.factor(incendies$origine_alerte)
incendies$precision_surf <-as.factor(incendies$precision_surf)
incendies$nature <-as.factor(incendies$nature)
#incendies$annee<-as.factor(incendies$annee)
incendies$departement<-as.factor(incendies$departement)

incendies$date_alerte <-  as.Date(incendies$date_alerte,format = "%Y-%m-%d")


table(incendies$departement)


incendies$surface_agricole<-as.integer(incendies$surface_agricole)


temp<-incendies %>% select(annee,code_insee,origine_alerte,surface_parcourue,surface_foret,surface_maquis,
                           surface_nat_autre_foret,surface_agricole,surface_autre_terre_boisee,surface_non_boisee_nat,
                           surface_non_boisee_art,suface_non_boisee,nature) %>% filter(as.integer(annee)<2020) %>%
                    select(- c(annee,surface_parcourue)) %>%
                    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
                                                                                                       



                    


tempGB <- temp %>% group_by(code_insee)





%>%
  summarise(moyenne_vent2=mean(v_moyenn_vent)) %>% select(annee,code_insee,moyenne_vent2) 





