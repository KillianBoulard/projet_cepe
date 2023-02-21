#Le dataset meteo comprend l'ensemble des données métérologiques de # 2010 à 2023
library(ggplot2)
library(dplyr)
library(tidyr)
library(geosphere)
library(tidyverse)
library(purrr)
library(FactoMineR)
library(corrplot)


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


## enlever certaines variables descriptives en caractere qui ne servent a rien a priori 
METEO_QUANTI<-meteo %>%  
  select (- type_tendance_barométrique,-id_temps_present,-id_temps_passe_1,-id_temps_passe_2,
            -nom_station,- lib_type_tendance_barometrique, - lib_temps_passe1,-lib_temps_present,
            -lib_commune,-lib_epci,-lib_commune,-lib_departement,-code_departement,-lib_region,
            -code_region, - methode_mesure_temp_thmouille, -temperature_thmouille, -code_epci,
          - temp_min_12h,- temp_min_24h,- temp_max_12h,-temp_max_24h,- temperature_C) %>% 
  mutate(date_mesure =as.Date(date_mesure,"%Y-%m-%d"),
         temperature = convert_temp_KtoC(temperature),
         point_rosee = convert_temp_KtoC(point_rosee),
         temp_min_12h = convert_temp_KtoC(temp_min_12h),
         temp_min_24h = convert_temp_KtoC(temp_min_24h),
         temp_max_12h = convert_temp_KtoC(temp_max_12h),
         temp_max_24h = convert_temp_KtoC(temp_max_24h)) %>% 
 mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))  



temperature<- METEO_QUANTI  %>% select(
  temperature,point_rosee,temp_min_12h,temp_min_24h,temp_max_12h,temp_max_24h,
  temperature_C,temperature_min_12h_C,temperature_min_24h_C,temperature_max_12h_C,
  temperature_max_24h_C,temperature_min_sol_12h_C)


echant_aleatoire_meteo<-tas <- temperature[sample(nrow(METEO_QUANTI), 80000, replace = FALSE), ]
  



mcor <- (cor(echant_aleatoire_meteo))
mcor
corrplot(mcor,type="upper", order="hclust", tl.col="black", tl.srt=45)

test<-METEO_QUANTI[1:10000,]


res.pca <- PCA(echant_aleatoire_meteo, graph = TRUE)

res.pca$var





fviz_pca_var(res.pca, col.var = "black")


plot(res.pca)

print(res.pca)
    