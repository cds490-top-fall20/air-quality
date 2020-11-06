library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(reshape2)
library(leafpop)
shape <- tigris::counties(state = "VA", class = "sf")
County_Transportation_Profiles <- read_csv("County_Transportation_Profiles.csv")
sota_fips <- read_csv("SOTA_FIPS_COMPLETE.csv")

sota_fips[sota_fips == "DNC"] <- NA

shape$GEOID <- as.numeric(shape$GEOID)

shape <- select(
  shape,
  GEOID,
  NAMELSAD,
  COUNTYNS,
  geometry
)

Transit <- County_Transportation_Profiles %>%
  rename(
    GEOID = `County FIPS`
  )

Transit$GEOID <- as.numeric(Transit$GEOID)

Transit <- select(
  Transit, 
  GEOID, 
  `Number of workers from other counties who commute to work in the county`,
  `Number of resident workers who commute to work in other counties`,
  `Number of resident workers who commute within county`,
  `Number of residents`
)

y <- left_join(shape,Transit, by = "GEOID")


SOTA <- left_join(y, sota_fips, by = "GEOID")

#nei_virginia_highway$GEOID <- with(nei_virginia_highway, paste0(STATE_FIPS,COUNTY_FIPS))

#nei_virginia_offhighway$GEOID <- with(nei_virginia_offhighway, paste0(STATE_FIPS,COUNTY_FIPS))

full_data <- SOTA %>%
  rename(
    commute_within = `Number of resident workers who commute within county`,
    commute_into = `Number of workers from other counties who commute to work in the county`,
    commute_out = `Number of resident workers who commute to work in other counties`,
    num_residents = `Number of residents`
  )

full_data$`Adult Asthma`
full_data$`Total Pop`

full_data <- mutate(
  full_data,
  smoker_ratio = `Ever Smoker`/`Total Pop`,
  pediatric_asthma_ratio = `Pediatric Asthma`/`Total Pop`,
  adult_asthma_ratio = `Adult Asthma`/`Total Pop`
)

######################################################################################################

# Define UI
ui <- fluidPage(
  

  # Top panel with county name
  verticalLayout(
    
    wellPanel(textOutput("cnty")),
    
    # the map itself
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  
  # Function for generation a popup based on the area clicked by the user
  makePopupPlot <- function (clickedArea, full_data) {
    # prepare the df for ggplot
    noGeom <- st_drop_geometry(full_data)
    plotData <- noGeom[c("NAMELSAD","smoker_ratio", "pediatric_asthma_ratio","adult_asthma_ratio")]
    plotDataSubset <- subset(plotData, plotData['NAMELSAD'] == clickedArea) 
    plotDataMelt = melt(plotDataSubset, id.vars = "NAMELSAD")
    
    popupPlot <- ggplot(data = plotDataMelt,  aes(x = variable, y = value, fill=value)) + 
      geom_bar(position="stack", stat="identity", width = 0.9) +
      scale_fill_steps2(
        low = "#ff0000",
        mid = "#fff2cc",
        high = "#70ad47",
        midpoint = 5) +
      coord_flip() +
      ggtitle(paste0("Score overview in ", clickedArea)) + 
      theme(legend.position = "none") +
      theme(plot.margin = unit(c(0,0.5,0,0), "cm"), plot.title = element_text(size = 10))
    
    return (popupPlot)
  }
  
  p <- as.list(NULL)
  p <- lapply(1:nrow(full_data), function(i) {
    p[[i]] <- makePopupPlot(full_data$NAMELSAD[i], full_data)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = shape,
                  label = ~NAMELSAD,
                  fillColor = "aliceblue", 
                  color = "grey",
                  layerId = ~COUNTYNS,
                  popup =  popupGraph(p, type = "svg"))
  })
  
  
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

######################################################################################################

ui <- fluidPage(
  
  # Application title
  titlePanel("Virginia County Polution Information"),
  
  # Choose data to present
  varSelectInput("Grade", "What grade would you like to View?", tibble("Particle Pollution Grade" = full_data$`Particle Pollution Grade`,
                                                                       "High Ozone Day Grade" = full_data$`High Ozone Day Grade`,
                                                                       "Particle Pollution Grade (Annual)" = full_data$`Particle Pollution Particle Pollution Grade (Annual)`)),
  
  # Top panel with county name
  verticalLayout(
    
    wellPanel(textOutput("cnty")),
    
    # the map itself
    mainPanel(
      leafletOutput("map")
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