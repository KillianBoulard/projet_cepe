library(shiny)
library(leaflet)
#library(shinyalert)
library(shinyWidgets)



navbarPage("Projet Incendie CEPE / LBP", id="main",
           tabPanel("Carte",
                    div(class="outer",
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        useShinyalert(force = TRUE), 
                        leafletOutput("incmap", height=1000),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 150, left = 20, right = "auto", bottom = "auto",
                                      width = 200, height = "auto",
                                      h4("Choix des années"),
                                      actionLink("selectall","Tout cocher / décocher"),
                                      checkboxGroupInput("check_year", "Année : ",
                                                         choices = 2021:2011)),
                        fixedPanel(id = "controls", top = 57, left = "40%", right = "auto", bottom = "auto",
                                   width = NULL, height = NULL, draggable = FALSE, cursor = c("auto",
                                                                                              "move", "default", "inherit"),
                                   sliderInput("slidersurface", "Seuil de surface parcourue :",
                                               min = 0, max = 75821, value = 2512
                                   ),
                                   actionButton("median_button", "Médiane", icon = NULL, 
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   actionButton("mean_button", "Moyenne", icon = NULL, 
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 600, left = 20, right = "auto", bottom = "auto",
                                      width = 410, height = "auto",
                                      h4("Probabilité d'avoir un feu dans ma commune :"),
                                      selectInput("input_insee", 
                                                  label = "Insérez le code insee :", 
                                                  choices = NULL,
                                                  selected = ""
                                      ),
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
                                              "KNN" = 2,
                                              "Neural Network" = 3
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
           navbarMenu("Analyses",
                      tabPanel("Plot 1",
                               plotOutput("plot1")
                      ),
                      tabPanel("Plot 2",
                               plotOutput("plot2")
                      ),
                      tabPanel("Plot 3",
                               plotOutput("plot3")
                      ),
                      tabPanel("Plot 4",
                               plotOutput("plot4")
                      )
           ),
           tabPanel("A propos du projet"))

