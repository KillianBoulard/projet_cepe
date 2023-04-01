library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)

shinyServer(function(input, output, session) {
  #dataset = readRDS(file = "c:/Users/User/Desktop/Cours et documents/formation R ensae/DATA/dataset.RDS")
    #Import des paramètres du XGboost
    xgb_model = readRDS("XGboost.rda")
    data_2021 = dataset %>% filter(annee == 2021)
    
    # Import Data and clean it
    data_clean <- dataset %>% mutate(incendie = ifelse(Y==0,"incendie=0","incendie=1") ) %>%
        filter (annee != 2022) %>% filter (annee != 2010)
    data_incendies <-data_clean%>% filter(Y==1)
    
    #50% des incendies les plus importants (surface_parcourue)
    mean_surface<-mean(data_incendies$superficie)
    mediane_surface<-median(data_incendies$superficie)
    min_surface<-min(data_incendies$superficie)
    max_surface<-max(data_incendies$superficie)
    
    
    data_incendies_majeurs<- data_incendies %>%
        filter(superficie>mediane_surface)
    
    # create a color paletter for category type in the data file
    pal <- colorFactor(
        palette = 'Accent',
        domain = data_incendies$annee
    )
    
    output$application_name <- renderText(paste(input$input_code_insee))
    output$application_name2 = renderText(paste(input$input_model))
    
    # create the leaflet map  
    output$incmap <- renderLeaflet({
        leaflet(data_incendies) %>%
            addCircles(lng = ~long, lat = ~lat, weight = 1,
                       group = "mygroup",
                       radius = ~sqrt(superficie) *60,
                       popup = ~paste(sep = "<br/>",
                                      "<b>Surface (hectares) </b>", superficie,
                                      "<b>Commune </b>",code_insee,
                                      "<b>Année </b>",annee),
                       color = ~pal(annee), fillOpacity = 0.75) %>% 
            addTiles() %>%
            setView(2.792276, 46.461114,zoom=6.25) %>%
            addLegend("topright", pal = pal, values = ~annee,
                      title = " Répartition des incendies",
                      labFormat = labelFormat(prefix = " "),
                      opacity = 1)
    })

    observeEvent(input$selectall,{
        if(input$selectall == 0) return(NULL) 
        else if (input$selectall%%2 == 0)
        {
            updateCheckboxGroupInput(session,"check_year", "Année : ",choices = 2021:2011)
        }
        else
        {
            updateCheckboxGroupInput(session,"check_year", "Année : ",choices = 2021:2011,selected=2021:2011)
        }
    })
    
    observeEvent(input$median_button,{
        updateSliderInput(session, "slidersurface", value = 2512)
    })
    
    observeEvent(input$mean_button,{
      updateSliderInput(session, "slidersurface", value = 3709)
    })
    
    observeEvent(eventExpr = {
      input$check_year
      input$slidersurface
      },
      handlerExpr =  {
        leafletProxy("incmap", data = intersect(filteredData(), filteredDataSlider())) %>%
            clearGroup ("mygroup") %>%
            addCircles(lng = ~long, lat = ~lat, weight = 1,
                       group = "mygroup",
                       radius = ~sqrt(superficie) *60,
                       popup = ~paste(sep = "<br/>",
                                      "<b>Surface (hectares) </b>", superficie,
                                      "<b>Commune </b>",code_insee,
                                      "<b>Année </b>",annee),
                       color = ~pal(annee), fillOpacity = 0.75) %>% 
            clearControls() %>% 
            addLegend("topright", pal = pal, values = ~annee,
                      title = " Répartition des incendies majeurs",
                      labFormat = labelFormat(prefix = " "),
                      opacity = 1)
    },ignoreNULL = F)
    

    observeEvent(input$valid_model, {
        code_insee = input$input_code_insee
        mois = input$input_mois
        a_prevoir = data_2021 %>% filter(code_insee == !!code_insee) %>% filter(mois == !!mois)

        a_prevoir = a_prevoir %>% 
            select(-c(annee, code_insee, id_station, -Y))
        
        test = predict(xgb_model, a_prevoir, type="prob")
        result = test[,2]
        result = result*100
        
        if (result >= 50) {
            shinyalert("Résultat de la prédiction :",
                       paste("Vous avez",
                             paste0(round(result,2),
                             "% de chance d'avoir un feu dans votre commune ")), 
                       type = "warning")
        } else {
            shinyalert("Résultat de la prédiction :",
                       paste("Vous avez",
                             paste0(round(result,2),
                             "% de chance d'avoir un feu dans votre commune ")), 
                       type = "success")}
    })
    
    
    filteredData <- reactive({
        if (length(input$check_year) == 0) {
          data_incendies[data_incendies$annee %in% 2010, ]
        } else {
          data_incendies[data_incendies$annee %in% input$check_year, ]
        }
    })
    
    filteredDataSlider <- reactive({
      if (input$slidersurface > 0) {
        data_incendies[data_incendies$superficie <= input$slidersurface, ]
      } else {
        data_incendies[data_incendies$superficie %in% 0, ]
      }
    })
    
    output$data <-DT::renderDataTable(datatable(
      data_incendies,filter = 'top',
    ))
})
