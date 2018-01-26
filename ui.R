jscode <- '
$(document).on("shiny:connected", function(e) {
var jsHeight = window.innerHeight;
Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'
#install_github("/shinydashboard")

library(shinydashboard)



dashboardPage(
  
  skin = 'blue',
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    width = 300,
    sidebarUserPanel('Xiaoyu Yang',
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"
                     ),
    sidebarMenu(
      menuItem('Overview', tabName = 'overview', icon = icon("map")),
      menuItem("Map 1", icon = icon("map"),
               menuItem("Restaurants by inspection score", tabName = "map", icon = icon("map")),
               menuItem(selectizeInput("risk", 
                                          "Select Inspection Score to Display", 
                                          risk, 
                                          selected = 'No Risk'))), ### add things into menusubitem
      menuItem("Map 2", icon = icon("envelope-open"),
               menuItem("Restaurants by zipcode",
                        tabName = 'zipcode',
                        icon = icon('envelope-open')), menuItem(selectizeInput("zipcode", "
                                       Select Zipcode to Display", 
                                       zipcode, 
                                       selected = 94102))),
               
      menuItem("Map 3", icon = icon("street-view"),
               menuItem("Most and least sanitary foodie streets", 
                           tabName = "street", icon = icon("street-view")),
               menuItem(selectizeInput("street",
                              "Select Streets to Display", 
                              choice = NULL, multiple = TRUE)),
               menuItem(checkboxInput("show", 
                                         "Show Heatmap", 
                                         value = FALSE)))
      )
    ),
    
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          column(12, align = 'center', img(src = 'restaurant2.jpg', height = 780, width =900))
        )),
      tabItem(
        tabName = "map",
              fluidRow(
                column(12, align = 'center', leafletOutput("map1", height = 750, width = 1100))
              )),
      tabItem(tabName = "zipcode",
               fluidRow(
                 column(12, align = 'center')
                 ), 
               fluidRow(box(leafletOutput("map2"), width =6),
                        box(plotOutput('graph2'), width =6)
                        ),
              fluidRow(infoBoxOutput("noBox", width =6),
                       infoBoxOutput("lowBox", width =6),
                       infoBoxOutput("moderateBox", width =6),
                       infoBoxOutput("highBox", width =6))
               ),
      tabItem(
        tabName = "street",
              fluidRow(
                column(12, align = 'center', leafletOutput("map3", height = 750, width = 1100))
      )
)
)
))