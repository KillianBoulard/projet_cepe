#Les fichiers sont disponibles dans README.md
#Emplacement local des fichiers (à définir pour chaque utilisateur)
setwd("c:/Users/User/Desktop/Cours et documents/formation R ensae/DATA")

# Le dataset incidendies comprend l'ensemble des incendies jour / jour 
# Jusqu'à la maille commune du 01/01/2010 au 31/12/2021
icendies <- read.csv(file="Incendies.csv", header = T, sep=";", skip = 6)

#Le dataset meteo comprend l'ensemble des données métérologiques de 
# 2010 à 2023
meteo <- read.csv(file="donnees-synop-essentielles-omm.csv", header = T, sep=";")


format(d$date, format = "%m")
