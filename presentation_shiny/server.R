library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output, session) {
  #dataset = readRDS(file = "c:/Users/User/Desktop/Cours et documents/formations/formation R ensae/DATA/dataset.RDS")
  #Import des paramètres du XGboost
  xgb_model = readRDS("XGboost.rda")
  data_2021 = dataset %>% filter(annee == 2021)
  
  # Import Data and clean it
  data_clean <- dataset %>% mutate(incendie = ifelse(Y==0,"incendie=0","incendie=1")) %>%
    filter (annee != 2022) %>% filter (annee != 2010)
  data_incendies <-data_clean%>% filter(Y==1)
  
  
  #50% des incendies les plus importants (surface_parcourue)
  mean_surface<-mean(data_incendies$superficie)
  mediane_surface<-median(data_incendies$superficie)
  min_surface<-min(data_incendies$superficie)
  max_surface<-max(data_incendies$superficie)
  
  
  data_incendies_majeurs<- data_incendies %>%
    filter(superficie>mediane_surface)
  
  #create a color paletter for category type in the data file
  pal <- colorFactor(
    palette = "Set1",
    domain = data_incendies$annee
  )
  
  output$application_name <- renderText(paste(input$input_code_insee))
  output$application_name2 = renderText(paste(input$input_model))
  
  
   #Table incendie
  incendies <- read.csv(file="c:/Users/User/Desktop/Cours et documents/formations/formation R ensae/DATA/Incendies.csv",
                        col.names = c("annee","id","departement","code_insee",
                                      "nom_commune","date_alerte","origine_alerte","moyens_premiere_intervention",
                                      "surface_parcourue","surface_foret","surface_maquis",
                                      "surface_nat_autre_foret","surface_agricole","surface_autre_terre_boisee","surface_non_boisee_nat",
                                      "surface_non_boisee_art","surface_non_boisee","precision_surf","surface_feu_initiale",
                                      "voie_caross_proche","act_hab_proche","type_peupl","connaissance","source_enquete",
                                      "nature","interv_equipe","deces_bat_touches",
                                      "nb_deces","nb_bat_tot_detruit","nb_bat_part_detruit","hygrometrie",
                                      "v_moyenn_vent","dir_ven","temperature","precision_donnee","presence_contour_valide"),
                        header = T, 
                        sep=";",
                        encoding='UTF-8',skip = 6)
  incendies$origine_alerte <- gsub('IndÃÂ©terminÃÂ©', 'Indetermine', incendies$origine_alerte)
  incendies$origine_alerte <- gsub('Ã©', 'e', incendies$origine_alerte)
  incendies$precision_surf <- gsub('EstimÃÂ©es', 'Estimees', incendies$precision_surf)
  incendies$precision_surf <- gsub('Ã©', 'e', incendies$precision_surf)
  incendies$origine_alerte <- gsub('Pompiers prÃÂ©-positionnÃÂ©s', 'Pompiers pre-pos', incendies$origine_alerte)
  incendies$origine_alerte <- gsub('Vigie-camÃÂ©ra', 'Vigie-cameras', incendies$origine_alerte)
  incendies$origine_alerte <- gsub('Ã©', 'e', incendies$origine_alerte)
  incendies$precision_surf <- gsub('MesurÃÂ©es', 'Mesuree', incendies$precision_surf )
  incendies$precision_surf <- gsub('Ã©', 'e', incendies$precision_surf)
  incendies$origine_alerte <-as.factor(incendies$origine_alerte)
  incendies$precision_surf <-as.factor(incendies$precision_surf)
  incendies$origine_alerte <-as.factor(incendies$origine_alerte)
  incendies$precision_surf <-as.factor(incendies$precision_surf)
  incendies$nature <-as.factor(incendies$nature)
  incendies$annee<-as.factor(incendies$annee)
  incendies$departement<-as.factor(incendies$departement)
  incendies$date_alerte <-  as.Date(incendies$date_alerte,format = "%d/%m/%Y")
  incendiestest<-incendies %>% filter (nature != "" & dir_ven !="")
  
  
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
    code_insee = input$input_insee
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
  
  src = "Origine_alerte_graph.png"
  output$picture<-renderText({c('<img src="',src,'">')})
  
  observe({
    choiceList <- unique(dataset$code_insee)
    updateSelectizeInput(session = session, "input_insee", choices = unique(choiceList), server = TRUE)
  })
  
  
  output$data <-DT::renderDataTable(datatable(
    data_incendies,filter = 'top'
  ))
  
  output$plot1<-renderPlot({
    ggplot(data_incendies, aes(x=superficie, xmax=3000)) + geom_boxplot(aes(fill=annee)) 
      #facet_wrap(annee ~ .)
  })
  
  output$plot2<-renderPlot({
    ggplot(data_incendies, aes(x=superficie)) + 
      geom_histogram(aes(y=..density..), alpha=1, binwidth = 30,colour="lightblue", fill="lightblue")+
      geom_density(alpha=.2, fill="blue") 
  })
  
  output$plot3<-renderPlot({
    ggplot(data_clean, aes(x=temperature_14_g)) + 
      geom_histogram(aes(y=..density..), colour="darkblue", fill="lightblue")+
      geom_density(alpha=.2, fill="lightblue") + 
      facet_grid(incendie ~ .)
  })
  
  output$plot4<-renderPlot({
    data_group_by<-data_clean %>% select(annee,mois,code_insee,
                                         Y,id_station,superficie,temperature_23_m_1,
                                         temperature_14_m_1,temperature_23_m_12,temperature_14_m_12,
                                         point_rosee_14_m_1,point_rosee_14_m_12,
                                         humidite_14_m_1,humidite_14_m_12
    ) %>% 
      group_by(annee,mois,Y) %>%
      mutate(mean_temp_23_m_1 =mean(temperature_23_m_1),
             mean_temp_14_m_1 =mean(temperature_14_m_1),
             mean_temp_23_m_12 =mean(temperature_23_m_12),
             mean_temp_14_m_12 =mean(temperature_14_m_12),
             mean_point_rosee_14_m_1=mean(point_rosee_14_m_1),
             mean_point_rosee_14_m_12=mean(point_rosee_14_m_12),
             mean_humidite_14_m_1=mean(humidite_14_m_1),
             mean_humidite_14_m_12=mean(humidite_14_m_12)
      ) %>%
      distinct(annee,mois,Y,mean_temp_23_m_1,mean_temp_14_m_1,mean_temp_23_m_12,
               mean_temp_14_m_12,mean_point_rosee_14_m_1,mean_point_rosee_14_m_12,
               mean_humidite_14_m_1,mean_humidite_14_m_12)
    
    ggplot(data_group_by, aes(x=mois, y=mean_temp_14_m_1, group=Y)) +
      geom_line(aes(linetype=Y, color=Y))+
      geom_point(aes(color=Y))+
      theme(legend.position="top")+facet_wrap(annee~.)
  })
  
  output$plot5<-renderPlot({
    ggplot(incendiestest, aes(x = annee, fill=origine_alerte)) +
      geom_bar(position = position_dodge()) +
      geom_text(stat="count",
                aes(label=stat(count)), 
                position = position_dodge(width=1), 
                vjust=-0.5)+
      labs(title="Origine des alertes", 
           x="Année de l'incendie", y = "nb incendies")
  })
  
  output$plot6<-renderPlot({
    ggplot(incendiestest, aes(x = annee, fill=nature)) +
      geom_bar(position = position_dodge()) +
      geom_text(stat="count",
                aes(label=stat(count)), 
                position = position_dodge(width=1), 
                vjust=-0.5)+
      labs(title="répartition des incendies par nature (si renseignée)", 
           x="Année de l'incendie", y = "nb incendies")
  })
  
})
