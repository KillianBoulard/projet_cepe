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

table (incendies$origine_alerte)

incendies$surface_agricole<-as.integer(incendies$surface_agricole)


temp<-incendies %>% ##filter(code_insee=="01286") %>% 
                    select(annee,code_insee,origine_alerte,surface_parcourue,surface_foret,surface_maquis,
                           surface_nat_autre_foret,surface_agricole,surface_autre_terre_boisee,surface_non_boisee_nat,
                           surface_non_boisee_art,suface_non_boisee,nature) %>% filter(as.integer(annee)<2020) %>%
                    select(- c(annee, surface_parcourue))                           %>%
                      mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))           %>%
                      mutate(
                      nature_ind = ifelse(nature=="",1,0) ,
                      nature_acc = ifelse(nature=="Accidentelle",1,0),
                      nature_inv_part = ifelse(nature=="Involontaire (particulier)",1,0),
                      nature_inv_trav = ifelse(nature=="Involontaire (travaux)",1,0),
                      nature_nat   = ifelse(nature=="Naturelle",1,0),
                      nature_malv  = ifelse(nature=="Malveillance",1,0), 
                      
                      orig_alerte_autr  = ifelse(origine_alerte=="Autre",1,0) ,
                      orig_alerte_ind   = ifelse(origine_alerte=="Indetermine",1,0) ,
                      orig_alerte_mae   = ifelse(origine_alerte=="Moyen aerien",1,0) ,
                      orig_alerte_pat   = ifelse(origine_alerte=="Patrouille",1,0) ,
                      orig_alerte_ppr   = ifelse(origine_alerte=="Pompiers pre-positionnes",1,0) ,
                      orig_alerte_pop   = ifelse(origine_alerte=="Population",1,0) ,
                      orig_alerte_vcam  = ifelse(origine_alerte=="Vigie-camera",1,0)
                      
                      )  %>% 
                     select(- c(nature,origine_alerte))



temp <- temp2 %>% group_by(code_insee) %>% 
  mutate ( moy_surface_foret  = mean(surface_foret),
           moy_surface_maquis = mean(surface_maquis),
           moy_surface_nat_autre_foret = mean(surface_nat_autre_foret),
           moy_surface_agricole        = mean(surface_agricole),
           moy_surface_autre_terre_boisee  = mean(surface_autre_terre_boisee),
           moy_surface_non_boisee_nat     = mean(surface_non_boisee_nat),
           moy_surface_non_boisee_art     = mean(surface_non_boisee_art),
           moy_suface_non_boisee          = mean(suface_non_boisee),
           
           moy_nature_ind      = 	mean(nature_ind),
           moy_nature_acc      = 	mean(nature_acc),
           moy_nature_inv_part = 	mean(nature_inv_part),
           moy_nature_inv_trav = 	mean(nature_inv_trav),
           moy_nature_nat      = 	mean(nature_nat),
           moy_nature_malv     = 	mean(nature_malv), 
           
           
           moy_orig_alerte_autr  = mean(orig_alerte_autr ),
           moy_orig_alerte_ind   = mean(orig_alerte_ind  ),
           moy_orig_alerte_mae   = mean(orig_alerte_mae  ),
           moy_orig_alerte_pat   = mean(orig_alerte_pat  ),
           moy_orig_alerte_ppr   = mean(orig_alerte_ppr  ),
           moy_orig_alerte_pop   = mean(orig_alerte_pop  ),
           moy_orig_alerte_vcam  = mean(orig_alerte_vcam )
           
       )



tempGB <- temp2 %>% distinct(code_insee,
                             ###info surface par commune
                             moy_surface_foret,
                             moy_surface_maquis,
                             moy_surface_nat_autre_foret,                               
                             moy_surface_agricole,
                             moy_surface_autre_terre_boisee,
                             moy_surface_non_boisee_nat,moy_surface_non_boisee_art,moy_suface_non_boisee,
                             ###info nature incendie
                             moy_nature_ind,moy_nature_acc,moy_nature_inv_part,moy_nature_inv_trav,moy_nature_nat,
                             moy_nature_malv,
                             ###info origine incendie
                             moy_orig_alerte_autr, moy_orig_alerte_ind  ,moy_orig_alerte_mae  ,
                             moy_orig_alerte_pat,  moy_orig_alerte_ppr , moy_orig_alerte_pop , moy_orig_alerte_vcam )
                             
                             ) 



