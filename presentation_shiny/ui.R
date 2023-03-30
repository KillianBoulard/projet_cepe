library(shiny)
library(leaflet)

vars <- c(
    "2020" = "superzip",
    "Centile score" = "centile",
    "College education" = "college",
    "Median income" = "income",
    "Population" = "adultpop"
)



navbarPage("Projet Incendie CEPE", id="main",
           tabPanel("Carte",
                    div(class="outer",
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                    leafletOutput("map", height=1000),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 150, left = 20, right = "auto", bottom = "auto",
                                  width = 200, height = "auto",
                                  h4("Choix des années"),
                                  actionLink("selectAll","Tout selectionner"),
                                  checkboxGroupInput("checkGroup", "Année : ",
                                                     choices = 2011:2021,
                                                          selected = 1)),
                    
                    tags$div(id="cite",
                             'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                    ))
                    ),
           tabPanel("Data", DT::dataTableOutput("data")))



   #        checkboxGroupInput("checkGroup", "Week Day",
    #                          choices = c("2011" = 2011,
     #                                     "2012" = 2012,
   #                                     "2013" = 2013,
   #                                     "2014" = 2014,
   #                                     "2015" = 2015,
   #                                    "2016" = 2016,
   #                                     "2017" = 2017,
   #                                    "2018" = 2018,
   #                                 "2019" = 2019,
   #                                      "2020" = 2020,
   #                                    "2021" = 2021),
   #                        selected = 1)),
