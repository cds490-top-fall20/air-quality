# importing packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(leafpop))
suppressPackageStartupMessages(library(readr))

#########################################################
# data aggregation
#########################################################
shape <- tigris::counties(state = "VA", class = "sf")
County_Transportation_Profiles <- read_rds("data/counties_mapping_data_simplified.rds")

# Define UI 
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title = "Virginia County Polution Information"),
    
    dashboardSidebar(

    ),
    # Top panel with county name
    dashboardBody(
        
        # wellPanel(textOutput("cnty")),
        
        # the map itself
        fluidRow(
            box(
                varSelectInput("Grade", "What grade would you like to View?", tibble("Pollutant Grade" = full_data$`Pollutant Grade`,
                                                                                 "Ozone Grade" = full_data$`Ozone Grade`,
                                                                                 "Annual Pass/Fail" = full_data$`Annual Pass/Fail`))
                ),
            box(leafletOutput("map"))
        )
    )
)

server <- function(input, output) {
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("Stamen.Toner") %>% 
            addPolygons(data = shape,
                        label = ~NAMELSAD,
                        fillColor = "aliceblue", 
                        color = "grey",
                        layerId = ~COUNTYNS,
                        popup = paste("County:", full_data$County,"<br>",
                                      "Selected Grade:", full_data[[input$Grade]] ,"<br>",
                                      "Amount of People Who Commute In:", full_data$commute_into,"<br>",
                                      "Amount of People who Commute Out:", full_data$commute_out,"<br>",
                                      "Amount of People who Commute Within:", full_data$commute_within))
    })
    
    
    observe({ 
        event <- input$map_shape_click
        output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)