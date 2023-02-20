#Le dataset meteo comprend l'ensemble des données métérologiques de # 2010 à 2023
library(ggplot2)
library(dplyr)
library(tidyr)
library(geosphere)
library(tidyverse)
library(purrr)

setwd("C:/Users/vhle524/OneDrive - LA POSTE GROUPE/Documents/projetcepe/data")

#####################################
## #importation des données meteo ###
#####################################
##################################################################################################
meteo <- read.csv(file="donnees-synop-essentielles-omm.csv", 
                  col.names = c("id_station","date","pression_niv_mer","var_press_3h","type_tend_barom",
                                "direction_vent_moy_10m","vitesse_vent_moy_10m","temp",
                                "point_rosee","humidite","visib_hor","temps_present","temp_passe_1",
                                "temp_passe_2","nebulosite_tot","nebulosite_nuag_etinf","hauteur_base_nuage_etinf","type_nuage_etinf",
                                "type_nuage_etmoy","type_nuage_etsup","pression_station",
                                "niveau_barom","geopotentiel","var_press_24h","temp_min_12h","temp_min_24h","temp_max_12h",
                                "temp_max_24h","temp_min_sol_12h","methode_mesure_tempthmou","temp_therm_mouille","rafales_10min",
                                "rafales_periode","periode_mes_rafales","etat_sol","hauteur_tot_che_neglau_sol","hauteur_neige_fr",
                                "periode_mes_neigefr","precipitations_dern_heure","precipitations_3_dern_heure","precipitations_6_dern_heure",
                                "precipitations_12_dern_heure","precipitations_24_dern_heure","phen_spe_1","phen_spe_2","phen_spe_3","phen_spe_4",
                                "nebulosite_couche_nuage1","type_nuage_1","hauteur_base1","nebulosite_couche_nuage2","type_nuage_2",
                                "hauteur_base2","nebulosite_couche_nuage3","type_nuage_3","hauteur_base3",
                                "nebulosite_couche_nuage4","type_nuage_4","hauteur_base4","coordoonees","nom",
                                "type_tendance_barom","temp_passe_1","temps_present","temperature","temp_min12h_C",
                                "temp_min24h_C","temp_max12h_C","temp_max24h_C","temp_min_sol_12h_C","latitude",
                                "longitude","altitude","nom_commune","code_commune","nom_epci",
                                "code_epci","nom_dep","code_dep","nom_region","code_region","mois"),
                  header = T, sep=";",encoding='UTF-8') 






station<- meteo %>% distinct (id_station,latitude,longitude) %>% 
  group_by(id_station) %>%  
  mutate(nb = 1:n()) %>% 
  mutate(latitude_station=latitude,longitude_station=longitude) %>%
  filter(nb==1) %>% 
  select(id_station,latitude_station,longitude_station) 



communes <- read.csv(file="correspondance-code-insee-code-postal.csv", header = T, sep=";", encoding="UTF-8")

####################################################################
### mise en forme de la table des communes avec coordonnées geo  ###
####################################################################
com_dataset <- communes %>%
  distinct(code_insee = X.U.FEFF.Code.INSEE,
           population = Population,
           altitude_moy = Altitude.Moyenne,
           superficie = Superficie,
           gps = geo_point_2d) %>%
  mutate(code_insee = if_else(nchar(code_insee) == 4,paste0("0",code_insee),code_insee)) %>%
  separate(col = "gps",
           into = paste0("gps", 1:2), sep = ",",
           extra = "merge") %>% 
  mutate (latitude_commune=as.numeric(gps1),
          longitude_commune=as.numeric(gps2)) %>%
  select(code_insee,latitude_commune,longitude_commune)

geo_om_dataset<-com_dataset %>% distinct(code_insee,latitude_commune,longitude_commune)

###
#########################################################################################
### récuperation de la station la plus proche de la commune en calculant sa distance  ###
#########################################################################################


DIST_MIN_COMM_STATION<-crossing(geo_com_dataset, station) %>%
  mutate(commune_long_lat = map2(longitude_commune, latitude_commune, ~ c(.x, .y)),
         station_long_lat = map2(longitude_station, latitude_station, ~ c(.x, .y)),
         distance = unlist(map2(commune_long_lat, station_long_lat, ~ distGeo(.x, .y)))) %>%
  group_by(code_insee) %>%
  mutate(min_distance = distance == min(distance)) %>%
  ungroup() %>% filter(min_distance==TRUE) %>%
  distinct(code_insee,latitude_commune,longitude_commune,id_station,latitude_station,longitude_station,distance)









############################################################################################################
######################## autre taff ########################################################################


convert_temp_KtoC<-function(x) x-273.15


convert_temperature(277
                    )

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
                                "lib_epci","code_epci","lib_departement","code_departement","lib_region","code_region","mois"
                  ),
                  header = T, sep=";",encoding='UTF-8') 



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



METEO_QUANTI<-meteo %>%  
  select (- type_tendance_barométrique,-id_temps_present,-id_temps_passe_1,-id_temps_passe_2,
            -nom_station,- lib_type_tendance_barometrique, - lib_temps_passe1,-lib_temps_present,
            -lib_commune,-lib_epci,-lib_commune,-lib_departement,-code_departement,-lib_region,
            -code_region, - methode_mesure_temp_thmouille, -temperature_thmouille, -code_epci) %>% 
  mutate(temperature = convert_temp_KtoC(temperature),
         point_rosee = convert_temp_KtoC(point_rosee),
         temp_min_12h = convert_temp_KtoC(temp_min_12h),
         temp_min_24h = convert_temp_KtoC(temp_min_24h),
         temp_max_12h = convert_temp_KtoC(temp_max_12h),
         temp_max_24h = convert_temp_KtoC(temp_max_24h)
         ) %>% 
 mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))  





test<-METEO_QUANTI[1:10000,]










    