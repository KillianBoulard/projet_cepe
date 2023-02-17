#Le dataset meteo comprend l'ensemble des données métérologiques de # 2010 à 2023
library(ggplot2)
library(dplyr)
library(tidyr)
library(geosphere)


setwd("C:/Users/vhle524/OneDrive - LA POSTE GROUPE/Documents/projetcepe/data")


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


station<- meteo %>% distinct (id_station,latitude,longitude)
  

communes <- read.csv(file="correspondance-code-insee-code-postal.csv", header = T, sep=";",  encoding="UTF-8")

com_dataset = communes %>%
  distinct(code_insee = X.U.FEFF.Code.INSEE,
           population = Population,
           altitude_moy = Altitude.Moyenne,
           superficie = Superficie,
           gps = geo_point_2d) %>% 
  mutate(code_insee = if_else(nchar(code_insee) == 4,paste0("0",code_insee),code_insee)) %>%
  separate(col = "gps",
           into = paste0("gps", 1:2), sep = ",",
           extra = "merge") %>% rename(com_lat=gps1, com_long=gps2)


nb<-dim(com_dataset)[1]
nb2<-dim(station)[1]

test <-data.frame()
test2 <-data.frame()

commune1<-com_dataset[1,]



commune1$distance=distGeo(,p2)



