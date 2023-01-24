library(csvread)
library(ggplot2)
library(dplyr)
# Charger
library(wesanderson)

library(rpart)


incendies<- read.csv(file="C:/Users/vhle524/OneDrive - LA POSTE GROUPE/Documents/projetcepe/Incendies2021b.csv",
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
                     encoding='utf-8')




#correction des champs texte et transformation en vecteurs si nécessaire ##
incendies$origine_alerte <- gsub('IndÃ©terminÃ©', 'Indetermine', incendies$origine_alerte)
incendies$origine_alerte <-as.vector(incendies$origine_alerte)

incendies$precision_surf <- gsub('EstimÃ©es', 'Estimees', incendies$precision_surf)

incendies$precision_surf <-as.vector(incendies$precision_surf)












incendies$origine_alerte <- gsub('Pompiers prÃ©-positionnÃ©s', 'Pompiers pre-pos', incendies$origine_alerte)
incendies$origine_alerte <- gsub('Vigie-camÃ©ra', 'Vigie-cameras', incendies$origine_alerte)

incendies$origine_alerte <-as.vector(incendies$origine_alerte)

incendies$precision_surf <- gsub('MesurÃ©es', 'Mesuree', incendies$precision_surf )
incendies$precision_surf <-as.vector(incendies$precision_surf)
incendies$nature <-as.vector(incendies$nature)

#incendies$departement<-as.numeric(incendies$departement)
incendies$nb_event=1


incendiestest<-incendies %>% filter (nature != "" & dir_ven !="")

test<-as.data.frame(table(incendies$departement))

ggplot(incendies, aes(x=nb_event, color=nature)) +
  geom_histogram(fill="black", position="dodge")+
  theme(legend.position="left")

ggplot(data=incendiestest,aes(x=nature,fill=dir_ven),position="dodge")+geom_bar()

test<-as.data.frame(table(incendies$nature))

incendies$nature


incendiestest2<- as_tibble(incendiestest %>% select(departement,origine_alerte,surface_parcourue,source_enquete,
                                                    nature,v_moyenn_vent,hygrometrie,dir_ven,temperature))


as
set.seed((1234))



rename(incendiestest2,toto=surface_parcourue)

incendiestest2=rename(incendiestest2,Y=surface_parcourue)

modele_lm<-lm(Y~.,incendiestest2)
coefficients(modele_lm)
plot(modele_lm)

summary(modele_lm)

vi <- varImp(modele_lm,
             permut = 1)





# Plot variable importance
plotVarImp(vi)
test<-as.data.frame(table(incendies$origine_alerte))
barplot(c(2,5,7), col=wes.palette(n=3, name="GrandBudapest"))

