test_incendies = incendies %>%
  mutate(
    date_alerte = as.Date(date_alerte, format = "%d/%m/%Y"),
    annee = format(date_alerte," %Y"),
    mois = format(date_alerte," %m")) %>%
  
  group_by(
    annee, mois, code_insee
    ) %>%

  distinct(
      annee, mois, code_insee
    )
