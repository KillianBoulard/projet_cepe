# Projet CEPE : Modelisation des risques climatiques 

# Contributeurs : 
- Laurent BIGAS 
- Mathieu DA SILVA
- Killian BOULARD


# Jalons du projet : 
18/01 : 
Groupes formés
sujet défini 
--> problématique 
--> data

15/02 et 22/03 :
Préparation des données
Modélisation des donénes (problématique de regression, classification, présentation des données..)

19/04 : 
Préparer la présentation du sujet

# Sources des données : 

Données métérologiques (depuis 2010) : 
https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/table/?flg=fr&sort=date

données climat :
https://www.infoclimat.fr/opendata/

Interventions réalisées par les services d'incendie et de secours :
https://www.data.gouv.fr/fr/datasets/interventions-realisees-par-les-services-d-incendie-et-de-secours/#resources

Base de données des incendies : 
https://bdiff.agriculture.gouv.fr/incendies


Télécharger les datasets : 
https://drive.google.com/drive/folders/145zMbCeSOzcLvNtUfKnrgVJeg7An3cEa?usp=sharing


# variables identifiées à date: 

## fichier incendie
![image](https://user-images.githubusercontent.com/122978605/213425363-7739d9a6-1dfb-4a0f-8cd8-a4024d0942e4.png)




# idées de variables utiles :
 - nombre d'incendie meme mois année N-1
- nombre de jour moyen sans pluie precedant le feu 
- nombre d'incendie dans un rayon de x kilometre autour de la commune mois M-1
- nombre d'incendie dans un rayon de x kilometre autour de la commune mois M-12

- proportion de directions de vent de type N/S/E/O/NE... sur le mois -1 / commune lorsqu'il y a eut un incendie.
- vitesse moyenne du vent sur le mois -1 /commune lorsqu'il y a eut incendie

- proportion de directions de vent de type N/S/E/O/NE... sur le mois -12 / commune lorsqu'il y a eut un incendie.
- vitesse moyenne du vent sur le mois -12 /commune lorsqu'il y a eut incendie

- direction moyenne du vent sur le mois precedent/commune (fichier meteo) voir si corrélation avec la direction du vent dans le fichier incendie
- direction moyenne du vent sur le mois precedent/commune (fichier meteo) voir si corrélation avec la direction du vent dans le fichier incendie
- vitesse moyenne du vent sur le mois -1 /commune (fichier meteo)
- vitesse moyenne du vent sur le mois -12 /commune (fichier meteo)
- hygrometrie moyenne sur le mois M / commune
- hygrometrie moyenne sur le mois M-1 / commune


# données a prévoir : 
la surface brulée des mois + 1, 2,3,4
le nombre d"incendies  sur les prochains mois
