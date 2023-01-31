library(csvread)
library(ggplot2)
library(dplyr)
# Charger
library(wesanderson)
library(explore)
library(rpart)
library(ggthemes)

#travail
setwd("C:/Users/bigas/Documents/laurent/formation_cepe/projets_MachineLearning/data")


#taff
setwd("C:/Users/vhle524/OneDrive - LA POSTE GROUPE/Documents/projetcepe/data")


# incendie20<- read.csv(file="Incendies20.csv",
#                      col.names = c("annee","id","departement","code_insee",
#                                    "nom_commune","date_alerte","origine_alerte","moyens_premiere_intervention",
#                                    "surface_parcourue","surface_foret","surface_maquis",
#                                    "surface_nat_autre_foret","surface_agricole","surface_autre_terre_boisee","surface_non_boisee_nat",
#                                    "surface_non_boisee_art","suface_non_boisee","precision_surf","surface_feu_initiale",
#                                    "voie_caross_proche","act_hab_proche","type_peupl","connaissance","source_enquete",
#                                    "nature","interv_equipe","deces_bat_touches",
#                                    "nb_deces","nb_bat_tot_detruit","nb_bat_part_detruit","hygrometrie",
#                                    "v_moyenn_vent","dir_ven","temperature","precision_donnee","presence_contour_valide"),
#                      header = T, 
#                      sep=";",
#                      encoding='utf-8',skip = 3)
# 
# 
# 
# 
# incendie21<- read.csv(file="Incendies21.csv",
#                        col.names = c("annee","id","departement","code_insee",
#                                      "nom_commune","date_alerte","origine_alerte","moyens_premiere_intervention",
#                                      "surface_parcourue","surface_foret","surface_maquis",
#                                      "surface_nat_autre_foret","surface_agricole","surface_autre_terre_boisee","surface_non_boisee_nat",
#                                      "surface_non_boisee_art","suface_non_boisee","precision_surf","surface_feu_initiale",
#                                      "voie_caross_proche","act_hab_proche","type_peupl","connaissance","source_enquete",
#                                      "nature","interv_equipe","deces_bat_touches",
#                                      "nb_deces","nb_bat_tot_detruit","nb_bat_part_detruit","hygrometrie",
#                                      "v_moyenn_vent","dir_ven","temperature","precision_donnee","presence_contour_valide"),
#                        header = T, 
#                        sep=";",
#                        encoding='utf-8',skip = 3)



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



#incendies<-rbind(incendie20,incendie21)

#test

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
incendies$annee<-as.factor(incendies$annee)
incendies$departement<-as.factor(incendies$departement)

incendies$date_alerte <-  as.Date(incendies$date_alerte,format = "%Y-%m-%d")







incendiestest<-incendies %>% filter (nature != "" & dir_ven !="")

test<-as.data.frame(table(incendies$departement))

ggplot(incendies, aes(x=nb_event, color=nature)) +
  geom_histogram(fill="black", position="dodge")+
  theme(legend.position="left")


ggplot(data=incendies,aes(x=nature,fill=dir_ven),position="dodge")+geom_bar()
 
ggplot(data=incendies,aes(x=annee,fill=nature),position="dodge")+geom_bar()



ggplot(incendies, aes(x = annee, fill=nature)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat="count",
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  labs(title="répartition des incendies par nature", 
       x="Année de l'incendie", y = "nb incendies")
  theme_economist_white()




ggplot(data=incendiestest,aes(x=annee,fill=nature),position="dodge")+geom_bar()



geom_bar(stat="identity", position=position_dodge())


incendies_vent<-incendies %>%  
  filter(dir_ven !="") %>% mutate (mois= format(date_alerte, format = "%m")) %>%
  select(annee,mois,code_insee,v_moyenn_vent,dir_ven)

summary(incendies_vent)




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
                           prop_vent_N=sum(nb_vent_N)/somme_vent,
                           prop_vent_S=sum(nb_vent_S)/somme_vent,
                           prop_vent_O=sum(nb_vent_O)/somme_vent,
                           prop_vent_E =sum(nb_vent_E)/somme_vent,
                           prop_vent_SE =sum(nb_vent_SE)/somme_vent,
                           prop_vent_SO =sum(nb_vent_SO)/somme_vent,
                           prop_vent_NE =sum(nb_vent_NE)/somme_vent,
                           prop_vent_NO =sum(nb_vent_NO)/somme_vent) %>% 
                         select(annee,code_insee,moyenne_vent,prop_vent_N,prop_vent_S,
                                prop_vent_O,prop_vent_E ,prop_vent_SE ,prop_vent_SO ,
                                prop_vent_NE ,prop_vent_NO ) %>% slice(1)






incendies2B009<-incendies_vent %>% filter(code_insee=="2B009")  %>% 
                group_by(annee,code_insee) %>%
                summarise(moyenne_vent2=mean(v_moyenn_vent)) %>% select(annee,code_insee,moyenne_vent2) 



summary(incendies)

incendies_vent %>% explore_all(n = n)


