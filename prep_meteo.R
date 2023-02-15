#chargement des librairies
library(tidyverse)

defaultEncoding <- "UTF8"

data_meteo <- read.csv("~/MDS/projet_cepe_incendies/donnees-synop-essentielles-omm.csv", sep=";")
sample_meteo <- head(data_meteo,3000)

sorted_meteo <- sample_meteo %>% 
  arrange_all() 
  

arrange_all(sorted_meteo, pick(ID.OMM.station,Date))
