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
  select(- c(nature,origine_alerte)) %>% group_by(code_insee) %>%
  
  mutate ( 
    moy_surface_foret  = mean(surface_foret),
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
           
  )  %>%

         distinct(code_insee,
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



ggplot(temp,aes(x=moy_orig_alerte_pop))+ geom_histogram() + scale_x_continuous(trans='log2')

test<-(ST_DET_COMM %>% select(- code_insee))




testKm<-kmeans(test,nstart=20,centers=10)


names(testKm)
ggplot(test,aes(x=moy_surface_non_boisee_art,y=moy_surface_non_boisee_nat))+geom_point(col=testKm$cluster)


nbg=20
inter=1:nbg
intra=1:nbg
for(ii in 1:nbg){
  tmp=kmeans(test,centers=ii,nstart=100)
  inter[ii] <- tmp$betweenss
  intra[ii] <- tmp$tot.withinss
}
inter <- inter/tmp$totss
intra <- intra/tmp$totss
plot(inter,type="b")
lines(intra)

gp4 <- kmeans(test,centers = 3, nstart=100)

ggplot(test,
       aes(x=moy_surface_non_boisee_art,y=moy_surface_non_boisee_nat)) +geom_point(col=gp4$cluster)

ggplot(test,aes(x=moy_surface_non_boisee_art,y=moy_surface_non_boisee_nat))+geom_point(col=gp4$cluster)





ggplot(data=incendies,aes(x=nature,fill=dir_ven),position="dodge")+geom_bar()


dataINC<- incendies %>% filter(nature !="") %>% filter(departement== 83 | departement ==13 | departement == 06)
                                 
                               
                               
                               

ggplot(data=dataINC,aes(x=annee,fill=nature),position="dodge")+geom_bar() +
  coord_flip()


ggplot(data = dataINC) +
  geom_histogram(aes(x = surface_parcourue)) +
  facet_grid(
    rows = vars(annee), cols = vars(departement)
  ) + scale_x_continuous(trans='log2')







