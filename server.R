#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(leaflet)
library(googleVis)
library(DT)
library(shinydashboard)
library(ggthemes)
library(scales)


########## map 1 ###########
shinyServer(function(input, output, session) {
  ########## map 1 ###########
  output$map1 = renderLeaflet({
    map_data_1_start = map_data_1 %>% filter(score_class == input$risk)
    leaflet(map_data_1_start) %>% ### watch out for the bug
      addProviderTiles('Esri.WorldStreetMap') %>% 
      setView(lng = -122.441297, lat = 37.767972, zoom = 13) %>% ### watch out for the bug
      addMarkers(lng = ~longitude, lat = ~latitude,
                 clusterOptions = markerClusterOptions(),
                 popup = paste("Name:", map_data_1_start$business_name, "<br>",
                               "Address:", map_data_1_start$business_address, "<br>",
                               "Score", map_data_1_start$inspection_score, "<br>",
                               "Phone Number:", map_data_1_start$business_phone_number),
                 label = map_data_1_start$business_name,
                 icon = ~reIcons[score_class]
                 )
  })    
  observeEvent({input$risk
  }, {
    map_data_1_add = map_data_1 %>% filter(score_class == input$risk)
    leafletProxy('map1', data = map_data_1_add) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(~longitude, ~latitude,
                 clusterOptions = markerClusterOptions(),
                 popup = paste("Name:", map_data_1_add$business_name, "<br>",
                               "Address:", map_data_1_add$business_address, "<br>",
                               "Score:", map_data_1_add$inspection_score, "<br>",
                               "Phone Number:", map_data_1_add$business_phone_number),
                 label = map_data_1_add$business_name,
                 icon = ~reIcons[score_class])
  })
##################################################################################

########## map 2 ###########
  output$map2 = renderLeaflet({
    map_data_2_start = map_data_2 %>% filter(business_postal_code == input$zipcode)
    leaflet(map_data_2_start) %>%
      addProviderTiles('Esri.WorldStreetMap') %>%
      setView(lng = -122.441297, lat = 37.767972, zoom= 12) %>%
      addMarkers(~longitude, ~latitude,
                 popup = paste("Name:", map_data_2_start$business_name, "<br>",
                               "Address:", map_data_2_start$business_address, "<br>",
                               "Score:", map_data_2_start$inspection_score, "<br>",
                               "Phone Number:", map_data_2_start$business_phone_number),
                 label = map_data_2_start$business_name,
                 icon = ~reIcons[score_class])
  })   
  observeEvent({input$zipcode
  }, {
    map_data_2_add = map_data_2 %>% filter(business_postal_code == input$zipcode)
    leafletProxy('map2', data = map_data_2_add) %>%
      clearMarkers() %>%
      addMarkers(~longitude, ~latitude,
                 popup = paste("Name:", map_data_2_add$business_name, "<br>",
                               "Address:", map_data_2_add$business_address, "<br>",
                               "Score:", map_data_2_add$inspection_score, "<br>",
                               "Phone Number:", map_data_2_add$business_phone_number),
                 label = map_data_2_add$business_name,
                 icon = ~reIcons[score_class])
  })

########## map 2 graph ###########
  output$graph2 = renderPlot({
    map_data_2_graph = map_data_2 %>% filter(business_postal_code == input$zipcode) %>%
      group_by(score_class)
    graph_levels = c('100-90', '89-80','79-70','below 70')
    map_data_2_graph$score_class = factor(map_data_2_graph$score_class, levels = graph_levels)
    ggplot(data = map_data_2_graph, aes(x = score_class)) +
      geom_bar(aes(fill = score_class)) + ylab("Number of Restaurants") +
      scale_x_discrete(drop = F) +
      scale_y_continuous(labels = comma) +
      theme_economist() + scale_fill_economist() +
      ggtitle(paste(paste('Restaurants in zipcode',
                          input$zipcode,
                          collapse = ' '),
                    collapse = ' ')) +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.text = element_text(size=16),
            axis.title.y = element_text(size = 16, face="bold")) + xlab(NULL)
  })
  
    
########### map 2 infobox ###########  
  output$noBox <- renderInfoBox({
    map_data_2_count = map_data_2 %>% 
      filter(business_postal_code == input$zipcode) %>%
      group_by(score_class) %>%
      summarise(count=n())
    value_no =  map_data_2_count[map_data_2_count$score_class =='100-90', "count"]
    infoBox(paste('Number of Restaurants with inspection score 100-90 in', input$zipcode), 
            value_no, icon = icon("thumbs-o-up"), 
            fill = TRUE, color = 'aqua')
  })
  output$lowBox <- renderInfoBox({
    map_data_2_count = map_data_2 %>% 
      filter(business_postal_code == input$zipcode) %>%
      group_by(score_class) %>%
      summarise(count=n())
    value_lo =  map_data_2_count[map_data_2_count$score_class =='89-80', "count"]
    infoBox(paste('Number of Restaurants with inspection score 89-80 in', input$zipcode, collapse = ' '), 
            value_lo, icon = icon("cutlery"), 
            fill = TRUE, color = 'teal')
  })
  output$moderateBox <- renderInfoBox({
    map_data_2_count = map_data_2 %>% 
      filter(business_postal_code == input$zipcode) %>%
      group_by(score_class) %>%
      summarise(count=n())
    value_mo =  map_data_2_count[map_data_2_count$score_class =='79-70', "count"]
    infoBox(paste('Number of Restaurants with inspection score 79-70 in', input$zipcode, collapse = ' '), 
            value_mo, icon = icon("thumbs-o-down"), 
            fill = TRUE, color = 'orange')
  })
  output$highBox <- renderInfoBox({
     map_data_2_count = map_data_2 %>% 
      filter(business_postal_code == input$zipcode) %>%
      group_by(score_class) %>%
      summarise(count=n())
    value_hi =  map_data_2_count[map_data_2_count$score_class =='below 70', "count"]
    infoBox(paste('Number of Restaurants with inspection score below 70 in', input$zipcode, collapse = ' '), 
            value_hi, icon = icon("medkit"), 
            fill = TRUE, color = 'red')
    
    })
##########################################################################################

########### map 3 ###########
 updateSelectizeInput(session, 'street', choices = c('Top 5 most sanitary foodie streets', 
                                                      'Top 5 least sanitary foodie streets'), server = T)
  
  
   
  output$map3 = renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldStreetMap') %>% 
      setView(lng = -122.441297, lat = 37.767972, zoom=13)
    
  })
  
  observeEvent({input$street
  }, {
    if (input$street == 'Top 5 least sanitary foodie streets'){
      leafletProxy('map3', data = spl_lst2) %>%
        removeShape(layerId = LETTERS[6:10]) %>%
        clearMarkers() %>%
        addPolylines(color = "red", layerId = LETTERS[1:5]) %>%
        addAwesomeMarkers(lat = 37.781813, lng = -122.436381,
                          icon = icons2, label = 'Ellis St') %>%
        addAwesomeMarkers(lat = 37.801285, lng = -122.400404,
                          icon = icons2, label = 'Front St') %>%
        addAwesomeMarkers(lat = 37.792628, lng = -122.433514,
                          icon = icons2, label = 'Jackson St') %>%
        addAwesomeMarkers(lat = 37.794608, lng = -122.432474,
                          icon = icons2, label = 'Broadway') %>%
        addAwesomeMarkers(lat = 37.803103, lng = -122.409744,
                          icon = icons2, label = 'Stockton St')
      
    }else{
      leafletProxy('map3', data = spl_lst1) %>%
        removeShape(layerId = LETTERS[1:5]) %>%
        clearMarkers() %>%
        addPolylines(color = "blue", layerId = LETTERS[6:10]) %>%
        addAwesomeMarkers(lat = 37.760689, lng = -122.437252,
                          icon = icons1, label = 'Diamond St') %>%
        addAwesomeMarkers(lat = 37.784033, lng = -122.441578,
                          icon = icons1, label = 'Post St') %>%
        addAwesomeMarkers(lat = 37.75515, lng = -122.4258,
                          icon = icons1, label = '22nd St') %>%
        addAwesomeMarkers(lat = 37.758618, lng = -122.419632,
                          icon = icons1, label = '20th St') %>%
        addAwesomeMarkers(lat = 37.76470, lng = -122.4334,
                          icon = icons1, label = 'Noe St')
    }
  })
  
########## map 3 heatmap ########### 
  observeEvent(input$show, {
    if(input$show) {
      heatmap = map_data_1 %>% filter(score_class == 'below 70')
      leafletProxy("map3", data = heatmap) %>% 
        addProviderTiles("CartoDB.DarkMatter", layerId = 'a') %>% 
        setView(lng = -122.413097, lat = 37.790972, zoom=15) %>%
        clearWebGLHeatmap() %>%
        addWebGLHeatmap(~longitude, ~latitude, size = 1000, units = "p", opacity = 0.6) %>%
        removeShape(layerId = LETTERS[1:10]) %>%
        clearMarkers()
    } else {
      leafletProxy("map3") %>% 
        addProviderTiles('Esri.WorldStreetMap') %>%
        clearWebGLHeatmap() %>%
        setView(lng = -122.441297, lat = 37.767972, zoom=13) %>%
        removeShape(layerId = LETTERS[1:10]) %>%
        clearMarkers()
    }
  })
  
  
  
})






