#Le dataset meteo comprend l'ensemble des données métérologiques de # 2010 à 2023
library(ggplot2)
library(dplyr)
library(tidyr)
library(geosphere)
library(tidyverse)
library(purrr)
library(FactoMineR)
library(corrplot)
library(missMDA)
library(psych)


setwd("C:/Users/vhle524/OneDrive - LA POSTE GROUPE/Documents/projetcepe/data")

#####################################
## #importation des données meteo ###
#####################################
##################################################################################################
meteo <- read.csv(file="donnees-synop-essentielles-omm.csv", 
                  col.names = c("id_station","date_mesure","pression_niveau_mer","var_pression_3h","type_tendance_barométrique","direction_vent_moyen_10mn",
                                "vitesse_vent_moyen_10m",
                                "temperature","point_rosee","humidite","visibilite_horizontale","id_temps_present",
                                "id_temps_passe_1","id_temps_passe_2","nebulosite_totale","nebulosite_nuage_etage_inf","hauteur_base_nuage_etage_inf","type_nuage_etage_inf","type_nuage_etage_moy",
                                "type_nuage_etage_sup","pression_station","niveau_barometrique","geopotentiel","var_pression_24h",
                                "temp_min_12h","temp_min_24h","temp_max_12h","temp_max_24h",
                                "temp_min_sol_12h","methode_mesure_temp_thmouille","temperature_thmouille","rafales_10dermin","rafales_periode","periode_mesure_rafale",
                                "etat_sol","hauteur_couche_neigl","hauteur_neige_fraiche","periode_mesure_neige_fraiche","precipiation_derh","precipitation_3dh","precipitation_6dh",
                                "precipitation_12dh","precipitation_24dh","ph_spe_1","ph_spe_2","ph_spe_3","ph_spe_4","nebulosite_couche_nuageuse_1","type_nuage_1","hauteur_base_1",
                                "nebulosite_couche_nuageuse_2","type_nuage_2","hauteur_base_2","nebulosite_couche_nuageuse_3","type_nuage_3","hauteur_base_3",
                                "nebulosite_couche_nuageuse_4","type_nuage_4","hauteur_base_4","coordonnees","nom_station","lib_type_tendance_barometrique","lib_temps_passe1",
                                "lib_temps_present","temperature_C","temperature_min_12h_C","temperature_min_24h_C","temperature_max_12h_C",
                                "temperature_max_24h_C","temperature_min_sol_12h_C","latitude","longitude","altitude","lib_commune","code_commune",
                                "lib_epci","code_epci","lib_departement","code_departement","lib_region","code_region","mois"),
                  header = T, sep=";",encoding='UTF-8') 



# variables liées à la temperature
prop_na <-meteo %>% summarize_all(funs(sum(is.na(.)) / length(.)))


############################################################################################################
######################## autre taff ########################################################################


convert_temp_KtoC<-function(x) x-273.15


convert_temperature(277)



####### unités du fichier meteo 
# pression en Pa
# vitesse du vent en m/s
# humidité en %
# direction du vent en degré
# temperature en kelvin
# visibilite en metres
# nebulosité en %
# hauteur de la base de nuages en metres
# rafale en m/secondes
# precipitation en mm

table(meteo$code_departement)

## enlever certaines variables descriptives en caracteres qui ne servent a rien a priori 
METEO_QUANTI<-meteo %>%  
  filter(substr(code_departement,1,2) !="97" & substr(code_departement,1,2) !="98") %>% 
  select (- type_tendance_barométrique,-id_temps_present,-id_temps_passe_1,-id_temps_passe_2,
            -nom_station,- lib_type_tendance_barometrique, - lib_temps_passe1,-lib_temps_present,
            -lib_commune,-lib_epci,-lib_commune,-lib_departement,-lib_region,
            -code_region, - methode_mesure_temp_thmouille, -temperature_thmouille, -code_epci,
          - temp_min_12h,- temp_min_24h,- temp_max_12h,-temp_max_24h,
          - temperature_C,-coordonnees, -temp_min_sol_12h,
          -code_commune,-mois,-code_departement, -ph_spe_1,-ph_spe_2,-ph_spe_3,-ph_spe_4,
          -type_nuage_etage_inf,-type_nuage_etage_moy,-type_nuage_etage_sup,
          -type_nuage_1,-type_nuage_2,-type_nuage_3,-type_nuage_4,-longitude,-latitude,
          -geopotentiel, -direction_vent_moyen_10mn,
          # on enleve les vars ou plus de 35% de na
          -nebulosite_totale,-nebulosite_nuage_etage_inf,-hauteur_base_nuage_etage_inf,
          -niveau_barometrique,-var_pression_24h,-rafales_10dermin,-etat_sol,
          -hauteur_couche_neigl,-hauteur_neige_fraiche,-periode_mesure_neige_fraiche,
          -precipitation_24dh,-nebulosite_couche_nuageuse_1,-hauteur_base_1,
          -nebulosite_couche_nuageuse_2,-hauteur_base_2,-nebulosite_couche_nuageuse_3,
          -hauteur_base_3,-nebulosite_couche_nuageuse_4,-hauteur_base_4,
          -temperature_min_12h_C,-temperature_min_24h_C,-temperature_max_12h_C,
          -temperature_max_24h_C,-temperature_min_sol_12h_C,
          ) %>% 
  mutate(heure_mesure = as.factor(substr(date_mesure,12,19)),
         date_mesure  = as.Date(date_mesure,"%Y-%m-%d"),
         temperature  = convert_temp_KtoC(temperature),
         point_rosee  = convert_temp_KtoC(point_rosee),
         #supression des variables en doublon apres ACP
         #temp_min_24h = convert_temp_KtoC(temp_min_24h), 
         #temp_max_12h = convert_temp_KtoC(temp_max_12h),
         #temp_max_24h = convert_temp_KtoC(temp_max_24h)
         ) %>% 
  #on remplace par les moyennes les na restants
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))),
         id_station = as.integer(id_station))
  
 #mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))  


  table(METEO_QUANTI$heure_mesure)

##### proportion de na 
 prop_na <- METEO_QUANTI %>% summarise_all(list(na = ~sum(is.na(.))/length(.) *100)) 

 
 
 
 
########################################### tentative de PCA ###########



echant_aleatoire_meteo<- METEO_QUANTI[sample(nrow(METEO_QUANTI), 200, replace = FALSE), ]

echant_aleatoire_meteo2<- as.data.frame(echant_aleatoire_meteo,row.names = echant_aleatoire_meteo$id_station)

echant_aleatoire_meteo2<- echant_aleatoire_meteo2 %>% select(-id_station,-date_mesure,-heure_mesure)

res.pca <- PCA(echant_aleatoire_meteo2, graph = TRUE)

########################################################################### ###########

### matrice de corrélation des données 

mcor <- (cor(echant_aleatoire_meteo2))

mcor
df<-as.data.frame((mcor))

corrplot(mcor,type="upper", order="hclust", tl.col="black", tl.srt=45)

corrplot(mcor, method = 'circle', order = 'AOE', diag = FALSE)


corrplot(mcor)
test<-METEO_QUANTI[1:10000,c("id_station","date_mesure","heure_mesure")]

test = 1
