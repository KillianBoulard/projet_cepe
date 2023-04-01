library(shiny)
library(leaflet)
library(shinyalert)
library(shinyWidgets)

navbarPage("Projet Incendie CEPE / LBP", id="main",
           tabPanel("Carte",
                    div(class="outer",
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        useShinyalert(), 
                    leafletOutput("incmap", height=1000),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 150, left = 20, right = "auto", bottom = "auto",
                                  width = 200, height = "auto",
                                  h4("Choix des années"),
                                  actionLink("selectall","Tout cocher / décocher"),
                                  checkboxGroupInput("check_year", "Année : ",
                                                     choices = 2021:2011)),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 75, left = 300, right = "auto", bottom = "auto",
                                  width = 400, height = "auto",
                                  sliderInput("slidersurface", "Seuil de surface parcourue :",
                                              min = 0, max = 75821, value = 2512
                                  ),
                                  actionButton("median_button", "Médiane", icon(""), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                  actionButton("mean_button", "Moyenne", icon(""), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 600, left = 20, right = "auto", bottom = "auto",
                                  width = 410, height = "auto",
                                  h4("Probabilité d'avoir un feu dans ma commune :"),
                                  selectInput(
                                     "input_code_insee",
                                     "Code insee :",
                                     choices = unique(dataset$code_insee)),
                                  selectInput(
                                     "input_mois",
                                     "Mois :",
                                     choices = c(
                                        "Janvier" = "01",
                                        "Février" = "02",
                                        "Mars" = "03",
                                        "Avril" = "04",
                                        "Mai" = "05",
                                        "Juin" = "06",
                                        "Juillet" = "07",
                                        "Août" = "08",
                                        "Septembre" = "09",
                                        "Octobre" = "10",
                                        "Novembre" = "11",
                                        "Décembre" = "12"
                                     )),
                                  selectInput(
                                     "input_model",
                                     "Je choisis mon algorithme de prédiction :",
                                     # "Refresh interval",
                                     choices = c(
                                        "XGboost" = 1,
                                        "Modèle Linéaire" = 2,
                                        "Random Forest" = 3
                                     )),
                                  actionButton("valid_model", "Je prédis", icon("paper-plane"), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                  #textOutput("application_name")
                                  ),
                    tags$div(id="cite",
                             'Travaux réalisés par: ', tags$em('Laurent BIGAS / Killian BOULARD / Matthieu DA SILVA'), '2023'
                    ))
                    ),
           tabPanel("Données du projet", DT::dataTableOutput("data")),
           tabPanel("Data Lab (prévisions)"),
           tabPanel("A propos du projet"))

