library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)

shinyServer(function(input, output) {
    # Import Data and clean it
    data_clean <- dataset %>% mutate(incendie = ifelse(Y==0,"incendie=0","incendie=1") ) %>%
        filter (annee != 2022) %>% filter (annee != 2010)
    data_incendies <-data_clean%>% filter(Y==1)
    
    bb_data = data_incendies
    
    #50% des incendies les plus importants (surface_parcourue)
    mean_surface<-mean(data_incendies$superficie)
    mediane_surface<-median(data_incendies$superficie)
    
    data_incendies_majeurs<- data_incendies %>%
        filter(superficie>mediane_surface)
    
    # create a color paletter for category type in the data file
    pal <- colorFactor(
        palette = 'Accent',
        domain = data_incendies$annee
    )
    # create the leaflet map  
    output$map <- renderLeaflet({
        leaflet(data_incendies_majeurs) %>%
            addCircles(lng = ~lat, lat = ~long, weight = 1,
                       group = "mygroup",
                       radius = ~sqrt(superficie) *60,
                       popup = ~paste(sep = "<br/>",
                                      "<b>Surface (hectares) </b>", superficie,
                                      "<b>Commune </b>",code_insee,
                                      "<b>Année </b>",annee),
                       color = ~pal(annee), fillOpacity = 0.75) %>% 
            addTiles() %>%
            addLayersControl(
                position = "bottomright",
                overlayGroups = c("2021", "2020", "2019", "2018", "2017"),
                options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup(c("2021", "2020", "2019", "2018", "2017")) %>% 
            setView(2.792276, 46.461114,zoom=6.25) %>%
            addLegend("topright", pal = pal, values = ~annee,
                      title = " Répartition des incendies majeurs",
                      labFormat = labelFormat(prefix = " "),
                      opacity = 1
            )
    })
    mydata_filtered <- reactive(data_incendies_majeurs[data_incendies_majeurs$annee %in% input$checkGroup, ])
    observeEvent(input$checkGroup, {
        leafletProxy("map", data = mydata_filtered()) %>%
            clearGroup ("mygroup") %>%
            addCircles(lng = ~lat, lat = ~long, weight = 1,
                       group = "mygroup",
                       radius = ~sqrt(superficie) *60,
                       popup = ~paste(sep = "<br/>",
                                      "<b>Surface (hectares) </b>", superficie,
                                      "<b>Commune </b>",code_insee,
                                      "<b>Année </b>",annee),
                       color = ~pal(annee), fillOpacity = 0.75)
    })
    
    
    output$data <-DT::renderDataTable(datatable(
        bb_data[,c(-1,-23,-24,-25,-28:-35)],filter = 'top',
    ))
})
