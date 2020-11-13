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
dataset <- read_rds("data/counties_mapping_data_simplified.rds")

# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "Virginia County Polution Information"),
    
    dashboardSidebar(),
    dashboardBody(# wellPanel(textOutput("cnty")),
        
        # the map itself
        fluidRow(
            box(
                varSelectInput(
                    "Grade",
                    "What grade would you like to View?",
                    tibble(
                        "Pollutant Grade" = dataset$`Pollutant Grade`,
                        "Ozone Grade" = dataset$`Ozone Grade`,
                        "Annual Pass/Fail" = dataset$`Annual Pass/Fail`
                    )
                )
            ),
            box(leafletOutput("map"))
        ))
)

server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Stamen.Toner") %>%
            addPolygons(
                data = shape,
                label = ~ NAMELSAD,
                fillColor = "aliceblue",
                color = "grey",
                layerId = ~ COUNTYNS,
                popup = paste(
                    "County:",
                    dataset$County,
                    "<br>",
                    "Selected Grade:",
                    dataset[[input$Grade]] ,
                    "<br>",
                    "Amount of People Who Commute In:",
                    dataset$commute_into,
                    "<br>",
                    "Amount of People who Commute Out:",
                    dataset$commute_out,
                    "<br>",
                    "Amount of People who Commute Within:",
                    dataset$commute_within
                )
            )
    })
    
    
    observe({
        event <- input$map_shape_click
        output$cnty <-
            renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)