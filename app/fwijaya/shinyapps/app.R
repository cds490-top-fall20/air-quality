# importing packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(leafpop))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(RColorBrewer))

# importing datasets
shape <- tigris::counties(state = "VA", class = "sf")
dataset <-
    clean_names(read_rds("data/processed/counties_mapping_data_simplified.rds"))

################################################################################
# ui
################################################################################
ui <- fluidPage(
    titlePanel("shinyapp dashboard"),
    
    navbarPage(
        "navigation bar",
        tabPanel("virginia",
                 selectInput(
                     "variable",
                     label = h4("Select an air pollutant grade to view:"),
                     choices = c(
                         "Pollutant Grade" = 1,
                         "Ozone Grade" = 2,
                         "Annual Pass/Fail" = 3
                     )
                 )),
        
        
        mainPanel(leafletOutput("map"))
    )
)


################################################################################
# pallete
################################################################################
pollutant <- colorFactor(topo.colors(n = 5), dataset$pollutant_grade)
ozone <- colorFactor(topo.colors(n = 5), dataset$ozone_grade)


################################################################################
# server
################################################################################
server <- function(input, output) {
    output$map = renderLeaflet({
        if (input$variable == 1) {
            leaflet(shape) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                
                addPolygons(
                    fillColor = ~ pollutant(dataset$pollutant_grade),
                    weight = 1,
                    color = "white",
                    dashArray = 3
                )
        }
        
        else if (input$variable == 2) {
            leaflet(shape) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                
                addPolygons(
                    fill = ~ ozone(dataset$ozone_grade),
                    weight = 1,
                    color = "white",
                    dashArray = 3
                )
        }
        
        else if (input$variable == 3) {
            leaflet(shape) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                
                addPolygons(
                    fill = ~ ozone(dataset$annual_pass_fail),
                    weight = 1,
                    color = "white",
                    dashArray = 3
                )
        }
        
        
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)