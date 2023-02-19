#chargement des librairies
library(tidyverse)

defaultEncoding <- "UTF8"

#Lien autre
#data_meteo <- read.csv("~/MDS/projet_cepe_incendies/donnees-synop-essentielles-omm.csv", sep=";")
#lien pc fixe
data_meteo <- read.csv("C:/Users/m9das/iCloudDrive/ENSAE/Environnement projet LBP/projet_cepe/donnees-synop-essentielles-omm.csv", header=T, sep=";")
#On plante la graine de hasard pour faire un échantillon de notre jeu de données
set.seed(50)
sample_meteo <- data_meteo[sample(1:nrow(data_meteo),10000), ]
#on selectionne des donnes à 4 horaires differents pour creer une colonne a 4 horaires de la journee
sample_meteo <-
  sample_meteo %>% 
  filter(str_detect(Date,"10:00:00|13:00:00|17:00:00|20:00:00"))
#Creation colonne date avec conversion au bon format
sample_meteo$year <- 
  as.Date(substr(sample_meteo$Date,1,10))
sample_meteo$time <- 
  substr(sample_meteo$Date,12,16)

sorted_meteo <- sample_meteo %>% 
  arrange_all() 
  

arrange_all(sorted_meteo, pick(ID.OMM.station,Date))
