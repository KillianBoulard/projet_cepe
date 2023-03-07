#Les fichiers sont disponibles dans README.md
#Emplacement local des fichiers (à définir pour chaque utilisateur)
setwd("c:/Users/User/Desktop/Cours et documents/formation R ensae/DATA")
setwd("C:/Users/VOYK743/Desktop/Fichiers Perso/Formation ENSAI/Datasets")

# Le dataset incendies comprend l'ensemble des incendies jour / jour 
# Jusqu'à la maille commune du 01/01/2010 au 31/12/2021
incendies <- read.csv(file="Incendies.csv", header = T, sep=";", skip = 6,  encoding="UTF-8")

#Le dataset meteo comprend l'ensemble des données métérologiques de 
# 2010 à 2023
meteo <- read.csv(file="donnees-synop-essentielles-omm.csv", header = T, sep=";")

#Date processing en tibble
as_tibble(incendies)
as_tibble(meteo)

#Nettoyage et préparation du fichier des incenbdies 
#Renommage
incendies = rename(incendies,
                   "annee" = Année,
                   "numero" = Numéro,
                   "departement" = Département,
                   "code_insee" = Code.INSEE,
                   "nom_commune" = Nom.de.la.commune,
                   "date_alerte" = Date.de.première.alerte,
                   "origine_alerte" = Origine.de.l.alerte,
                   "moyens_premiere_intervention" = Moyens.de.première.intervention,
                   "surface_parcourue" = Surface.parcourue..m2.,
                   "surface_foret" = Surface.forêt..m2.,
                   "surface_maquis" = Surface.maquis.garrigues..m2.,
                   "surface_nat_autre_foret" = Autres.surfaces.naturelles.hors.forêt..m2.,
                   "surface_agricole" = Surfaces.agricoles..m2.,
                   "surface_autre_terre_boisee" =  Surface.autres.terres.boisées..m2.,
                   "surface_non_boisee_nat" = Surfaces.non.boisées.naturelles..m2.,
                   "surface_non_boisee_art" = Surfaces.non.boisées.artificialisées..m2.,
                   "surface_non_boisee" = Surfaces.non.boisées..m2.,
                   "precision_surf" = Précision.des.surfaces,
                   "surface_feu_initiale" = Surface.de.feu.à.l.arrivée.des.secours...0.1.ha,
                   "voie_caross_proche" = Voie.carrossable.la.plus.proche,
                   "act_hab_proche" = Activité.ou.habitation.la.plus.proche,
                   "type_peupl" = Type.de.peuplement,
                   "connaissance" = Connaissance,
                   "source_enquete" = Source.de.l.enquête,
                   "nature" = Nature,
                   "enterv_equipe" = Intervention.de.l.équipe.RCCI,
                   "deces_bat_touches" = Décès.ou.bâtiments.touchés,
                   "nb_deces" = Nombre.de.décès,
                   "nb_bat_tot_detruit" = Nombre.de.bâtiments.totalement.détruits,
                   "nb_bat_part_detruit" = Nombre.de.bâtiments.partiellement.détruits,
                   "hygrometrie" = Hygrométrie,
                   "v_moyenn_vent" = Vitesse.moyenne.du.vent,
                   "dir_vent" = Direction.du.vent,
                   "precision_donnee" = Précision.de.la.donnée,
                   "presence_contour_valide" = Présence.d.un.contour.valide
)

#Data prep : fichier indendie :
# Formattage des variables ;
# Mise en forme des données pouvant avoir une importance lors de la modelisation
clean_incendies = incendies %>%
  mutate(
    date_alerte = as.Date(date_alerte, format = "%d/%m/%Y"),
    dir_vent = as.character(dir_vent, na.rm= T),
    v_moyenn_vent = as.double(v_moyenn_vent, na.rm= T),
    date_m_1 = date_alerte %m+% months(-1),
    date_m_12 = date_alerte %m+% months(-12),
    
    #Formattage des dates pour future jointure sur m-1 et m-12
    annee = format(date_alerte,"%Y"),
    mois = format(date_alerte,"%m"),
    annee_m_1 = format(date_m_1, "%Y"),
    mois_m_1 = format(date_m_1,"%m"),
    annee_m_12 = format(date_m_12, "%Y"),
    mois_m_12 = format(date_m_12,"%m")
  ) %>%
  
  group_by(
    annee, mois, code_insee) %>%
  
  mutate(
    occurence_commune_mois = n(),
    surface_parcourue = sum(surface_parcourue),
    
    #Poids des différentes origines
    poids_origine_indetermine = sum(origine_alerte == "Indéterminée") / n(),
    poids_origine_autre = sum(origine_alerte=="Autre", na.rm=TRUE) / n(),
    poids_origine_moy_aer = sum(origine_alerte=="Moyen aérien", na.rm=TRUE) / n(),
    poids_origine_patrouille = sum(origine_alerte=="Patrouille", na.rm=TRUE) / n(),
    poids_origine_population = sum(origine_alerte=="Population", na.rm=TRUE) / n(),
    poids_origine_pomp_pre_pos = sum(origine_alerte=="Pompiers pré-positionnés", na.rm=TRUE) / n(),
    poids_origine_cam = sum(origine_alerte=="Vigie-caméra", na.rm=TRUE) / n(),
    
    #Poids des différentes natures
    poids_nat_accidentelle = sum(nature == "Accidentelle" , na.rm = T) / n(),
    poids_nat_inv_part = sum(nature == "Involontaire (particulier)", na.rm = T) / n(),
    poids_nat_inv_trav = sum(nature == "Involontaire (travaux)", na.rm = T) / n(),
    poids_nat_malv = sum(nature == "Malveillance", na.rm = T) / n(),
    poids_nat_naturelle = sum(nature == "Naturelle", na.rm = T) /n(),
    poids_nat_ind = sum(nature == "", na.rm = T) /n()
  ) %>%
  
  distinct(
    annee, mois, annee_m_1, mois_m_1, annee_m_12, mois_m_12, departement, nom_commune, code_insee,
    surface_parcourue, occurence_commune_mois, 
    #Origine
    poids_origine_indetermine, poids_origine_autre,
    poids_origine_moy_aer, poids_origine_patrouille, poids_origine_pomp_pre_pos, 
    poids_origine_cam, poids_origine_population,
    #Nature
    poids_nat_accidentelle, poids_nat_inv_part, poids_nat_inv_trav, poids_nat_malv, poids_nat_naturelle,
    poids_nat_ind) %>%
  
  select(
    annee, mois, annee_m_1, mois_m_1, annee_m_12, mois_m_12, departement, nom_commune, code_insee,
    surface_parcourue, occurence_commune_mois,
    #Origine
    poids_origine_indetermine, poids_origine_autre,
    poids_origine_moy_aer, poids_origine_patrouille, poids_origine_pomp_pre_pos,
    poids_origine_cam, poids_origine_population,
    #Nature
    poids_nat_accidentelle, poids_nat_inv_part, poids_nat_inv_trav, poids_nat_malv, poids_nat_naturelle,
    poids_nat_ind)

write_xlsx(clean_incendies, getwd())

#DJ
m_12_incendies <- dplyr::left_join(clean_incendies, clean_incendies,
                                   by = c('code_insee' = 'code_insee',
                                          'annee_m_12' = 'annee',
                                          'mois_m_12' = 'mois'))

m_1_incendies <- dplyr::left_join(clean_incendies, clean_incendies,
                                  by = c('code_insee' = 'code_insee',
                                         'annee_m_1' = 'annee',
                                         'mois_m_1' = 'mois'))


#Clean des données méteo
summary(incendies)
colnames(test_incendies)
head(test_incendies)



# test de cohérence :
#Test de coherence : cela doit donner 24,465k observations
test = station %>% 
  group_by(id_station) %>% 
  summarise(nrow = n())

test = station %>% 
  group_by(id_station) %>% 
  select(id_station, latitude_station,longitude_station) %>% 
  mutate(count = n())


test = DIST_MIN_COMM_STATION %>% 
  filter(code_insee == "07128")

test <- meteo %>%
  filter(id_station == 7471) %>%
  mutate(as.character(id_station),as.character(latitude), as.character(longitude))
  distinct(id_station,as.numeric(latitude), as.numeric(longitude)) %>% 
    select(id_station,as.numeric(latitude), as.numeric(longitude))



test = meteo %>% 
  distinct(id_station,latitude, longitude) %>% 
  mutate(latitude = as.numeric(latitude), as.numeric(longitude)) %>% 
  group_by(id_station) %>% 
  ungroup()

  


test = inc_dataset %>% 
  distinct(code_insee,mois)


test = base_dataset %>% 
  filter(presence_feu == 1)



## Exploration des données
- Vérification des valeurs manquantes ;
- Tableaux / plots ; 
- Renommage des colonnes.

```{r}
is.na(inc_dataset)
colSums(is.na(meteo))
apply(is.na(inc_dataset), MARGIN = 2, sum)
dim(inc_dataset)
```

## Data preprocessing Part I

```{r}
inc_dataset = inc_dataset %>%
  filter(!is.na(id_D)) %>%
  mutate(target = surface_parcourue) %>%
  mutate_at(c('origine_alerte', 'nature', 'dir_vent'), as.factor) %>% #as.ch
  select(-c(nb_bat_tot_detruit, nb_bat_part_detruit,
            nb_deces, surface_agricole)) %>%
  mutate(nbda = if_else(is.na(nbda), 0, as.numeric(nbda))) %>%
  filter(duree>1 & modeSortie !=9)

dataset
```

## Resample library : training set, validation

```{r}
set.seed(42)
dataset_split = initial_split(dataset, prop = 0.8, strata = target)
training = training(dataset_split)
test_set = testing(dataset_split)

training

set.seed(42)
train_split = initial_split(training, prop = 0.8, strata = target)
train_set = training(train_split)
eval_set = testing(train_split)
train_set

cat(dim(train_set),'/',dim(eval_set),'/',dim(test_set))
```

## 2 Recipes library : create a collection of recipes
### 2.1 Basic recipe
```{r}
rec_basic = recipe(data = train_set, target~.) %>%
  step_impute_mode(sexe) %>%
  step_impute_mean(age) %>%
  step_normalize(age) %>% #Centre et réduit, step_scale = juste réduire
  step_other(ghm2, threshold = 0.05) %>%
  step_dummy(ghm2) #Encodage 0 ou 1

rec_basic
prep(rec_basic)

juice(prep(rec_basic)) #Design matrix
formula(prep(rec_basic))
```

Version plus généralisable

```{r}
basic_rec = recipe(data= train_set, target~.) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_other(ghm2, threshold = 0.02) %>%
  step_other(dp, threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) #suppression des catégorie identiques (toujours egale 0) notamment a cause des facteurs

juice(prep(basic_rec))
```

### 2.2 Interaction recipe

```{r}
rec_interaction = basic_rec %>% 
  step_interact(~age : starts_with('dp'))

juice(prep(rec_interaction))
```

### 2.3 Spline recipe

```{r}
rec_spline = 
  # basic_rec %>% 
  rec_interaction %>% 
  step_ns(age, duree, deg_free = 3) #On peut aussi mettre tune()
juice(prep(rec_spline))
```



## 3. Parsnip library : creating model and fitting
### 3.1 logistical model and workflow

model is 
* a model 
* a engine (package)
* a mode (classification / regression)

```{r}
log_mod = logistic_reg() %>%
  set_engine('glm')
set_mode('classification')
```

wf is
* declarer un workflow
* ajouter un recipe
* ajouter un modèle

```{r}
log_wf = workflow() %>%
  add_recipe(rec_basic) %>%
  add_model(log_mod)

log_wf

log_fit = fit(log_wf, train_set)

predict(log_fit, eval_set)
predict(log_fit, eval_set, type = 'prob')


log_pred = eval_set %>%
  select(target) %>%
  bind_cols(
    predict(log_fit, eval_set),
    predict(log_fit, eval_set, type = 'prob')
  )

log_pred
```

### 3.2 RandomForest model and workflow
```{r}
rf_mod = rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('classification')
```


```{r}
rf_wf = workflow() %>% 
  add_recipe(basic_rec) %>% 
  add_model(rf_mod)

rf_fit = fit(rf_wf, train_set)

rf_fit %>% 
  predict(eval_set, type = 'prob')

rf_fit %>% 
  predict(eval_set)

rf_pred = eval_set %>% 
  select(target) %>% 
  bind_cols(
    rf_fit %>% 
      predict(eval_set, type = 'prob'),
    rf_fit %>% 
      predict(eval_set)
  )

rf_pred

```

### 3.3 Xgboost

```{r}
xg_mod = boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
                    loss_reduction = tune()) %>% 
  set_engine('xgboost') %>% 
  set_mode('classification')
```

```{r}
rf_wf = workflow() %>% 
  add_recipe(basic_rec) %>% 
  add_model(rf_mod)

rf_fit = fit(rf_wf, train_set)

rf_fit %>% 
  predict(eval_set, type = 'prob')

rf_fit %>% 
  predict(eval_set)

rf_pred = eval_set %>% 
  select(target) %>% 
  bind_cols(
    rf_fit %>% 
      predict(eval_set, type = 'prob'),
    rf_fit %>% 
      predict(eval_set)
  )

rf_pred
```

## 4. Yardstick library : Evaluer les performances des modèles
###4.1 Evaluer log

```{r}
log_pred

accuracy(log_pred, target, .pred_class)

log_pred %>% 
  group_by(target, .pred_class) %>% 
  summarise(n=n())

roc_auc(log_pred, target, .pred_0) #mesure comment l'aglo séparer les classes 1 et 0
roc_auc(log_pred, target, .pred_1, event_level = 'second') #ou comme ça
```

###4.2 Evaluer RF

```{r}
rf_pred
accuracy(rf_pred, target, .pred_class)
roc_auc(rf_pred, target, .pred_0) #mesure comment l'aglo séparer les classes 1 et 0
```

### Workflowset

```{r}
wf_set = 
  workflow_set(
    preproc = list(
      basic = basic_rec,
      inter = rec_interaction,
      spline = rec_spline
    ),
    models = list(
      log = log_mod,
      rf = rf_mod,
      xg = xg_mod
    )
  )
```

###validation croisée

```{r}
set.seed(42)
folds = vfold_cv(training ,v=5)

keep_pred = control_grid(save_pred = T, save_workflow = T)

set.seed(42)
res_wf_set =
  wf_set %>%
  workflow_map(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc), #ex : accuracy, roc_auc, f_meas, specifity...
    control = keep_pred,
    verbose = T, #Pendant le training permet d'avoir un suivi
    grid = 20 #Par rapport au paramètres définis dans XG, il va tester X combinaisons
  )

rank_results(res_wf_set, rank_metric='roc_auc')

```

```{r}
best_result = 
  res_wf_set %>% 
  extract_workflow_set_result('inter_xg') %>% 
  select_best(metric = 'roc_auc')
```

## 8 Best model, last fit and final prediction

```{r}
best_res_fit = 
  res_wf_set %>% 
  extract_workflow('inter_xg') %>% 
  finalize_workflow(best_result) %>% 
  last_fit(split = dataset_split) #Il reconnait que c'est les données de test

best_res_fit %>%  collect_metrics()
best_res_fit %>%  collect_predictions()

```



## En résumer, il faut surtout faire la partie 1, 2 et 4
01017


toto = base_dataset_full %>% 
  filter(mois == "05" & annee == "2015") %>% 
  filter(code_insee =="01017")


toti = inc_dataset %>% 
  filter(code_insee =="01017")


toti = base_dataset_test %>% 
  filter(presence_feu_m_1 =="1")


test = inc_dataset %>% 
  group_by(annee,mois,code_insee)%>% 
  mutate(med=n())

test = inc_dataset %>% group_by(annee, mois, code_insee) %>% mutate(count = n()) 
