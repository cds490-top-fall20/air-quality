# import libraries
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(geojson))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(binr))

# importing dataset
map = clean_names(read_csv('map.csv'))
states = geojsonio::geojson_read('tempgeo.json', what = 'sp')

###########################################################################
# BINS
###########################################################################
pal_pop <-
    colorBin(
        'BuPu',
        domain = map$total_population,
        bins = 9,
        pretty = FALSE
    )

pal_under18 <-
    colorBin('YlGn',
             domain = map$under_18,
             bins = 9,
             pretty = FALSE)

pal_65over <-
    colorBin(
        'YlOrRd',
        domain = map$x65_over,
        bins = 9,
        pretty = FALSE
    )

pal_pedasthma <-
    colorBin(
        'PiYG',
        domain = map$pediatric_asthma_sufferers,
        bins = 9,
        pretty = FALSE
    )

pal_adlasthma <-
    colorBin(
        'BrBG',
        domain = map$adult_asthma_sufferers,
        bins = 9,
        pretty = FALSE
    )

pal_copd <-
    colorBin(
        'PuOr',
        domain = map$copd_sufferers,
        bins = 9,
        pretty = FALSE
    )

pal_lngcancer <-
    colorBin(
        'Greens',
        domain = map$lung_cancer_sufferers,
        bins = 9,
        pretty = FALSE
    )

pal_hrtdisease <-
    colorBin(
        'RdGy',
        domain = map$heart_disease,
        bins = 9,
        pretty = FALSE
    )

pal_smoked <-
    colorBin(
        'Blues',
        domain = map$ever_smoked,
        bins = 9,
        pretty = FALSE
    )

pal_poverty <-
    colorBin('PuRd',
             domain = map$poverty,
             bins = 9,
             pretty = FALSE)

pal_poc <-
    colorBin(
        'Reds',
        domain = map$people_of_color,
        bins = 9,
        pretty = FALSE
    )

pal_orgozone <-
    colorFactor(topo.colors(15), map$orange_ozone_days)

pal_redozone <-
    colorFactor(topo.colors(3), map$red_ozone_days)

pal_purpozone <-
    colorFactor(topo.colors(2), map$purple_ozone_days)

pal_wgtozone <-
    colorFactor(topo.colors(15), map$wgt_avg_ozone_days)

pal_ozone <-
    colorFactor(topo.colors(6), map$ozone_grade)

pal_orgpol <-
    colorFactor(topo.colors(6), map$orange_pollutant_days)

pal_redpol <-
    colorFactor(topo.colors(4), map$red_pollutant_days)

pal_purppol <-
    colorFactor(topo.colors(2), map$purple_pollutant_days)

pal_marpol <-
    colorFactor(topo.colors(2), map$maroon_pollutant_days)

pal_wgtpol <-
    colorFactor(topo.colors(6), map$wgt_avg_pollutant_days)

pal_pollutant <-
    colorFactor(topo.colors(4), map$pollutant_grade)

pal_anndsg <-
    colorBin(
        'Accent',
        domain = map$annual_design_value,
        bins = 9,
        pretty = FALSE
    )

pal_annpf <-
    colorFactor(topo.colors(3), map$annual_pass_fail)
###########################################################################
# UI
###########################################################################
ui = fluidPage(
    titlePanel('EDA on State of the Air (SOTA) 2020 Data'),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                'select1',
                label = h4('select a category:'),
                choices = c(
                    'total population' = 1,
                    'under 18' = 2,
                    '65 and over' = 3,
                    'pediatric asthma sufferers' = 4,
                    'adult asthma sufferers' = 5,
                    'COPD sufferers' = 6,
                    'lung cancer sufferers' = 7,
                    'heart disease' = 8,
                    'ever smoked' = 9,
                    'poverty' = 10,
                    'people of color' = 11,
                    'orange ozone days' = 12,
                    'red ozone days' = 13,
                    'purple ozone days' = 14,
                    'weighted average ozone days' = 15,
                    'ozone grade' = 16,
                    'orange pollutant days' = 17,
                    'red pollutant days' = 18,
                    'purple pollutant days' = 19,
                    'maroon pollutant days' = 20,
                    'weighted average pollutant days' = 21,
                    'pollutant grade' = 22,
                    'annual design value' = 23,
                    'annual pass/fail' = 24
                ),
                multiple = FALSE,
                selectize = TRUE
            ),
            
            selectInput(
                'select2',
                label = h4('select a category:'),
                choices = c(
                    'total population' = 1,
                    'under 18' = 2,
                    '65 and over' = 3,
                    'pediatric asthma sufferers' = 4,
                    'adult asthma sufferers' = 5,
                    'COPD sufferers' = 6,
                    'lung cancer sufferers' = 7,
                    'heart disease' = 8,
                    'ever smoked' = 9,
                    'poverty' = 10,
                    'people of color' = 11,
                    'orange ozone days' = 12,
                    'red ozone days' = 13,
                    'purple ozone days' = 14,
                    'weighted average ozone days' = 15,
                    'ozone grade' = 16,
                    'orange pollutant days' = 17,
                    'red pollutant days' = 18,
                    'purple pollutant days' = 19,
                    'maroon pollutant days' = 20,
                    'weighted average pollutant days' = 21,
                    'pollutant grade' = 22,
                    'annual design value' = 23,
                    'annual pass/fail' = 24
                ),
                multiple = FALSE,
                selectize = TRUE
            )
        ),
        
        mainPanel(leafletOutput('displaymap1'),
                  leafletOutput('displaymap2'))
    )
)

###########################################################################
# SERVER
###########################################################################

server = function(input, output) {
    output$displaymap1 = renderLeaflet({
        if (input$select1 == 1)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                
                addPolygons(
                    fillColor = ~ pal_pop(map$total_population),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 2)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_under18(map$under_18),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 3)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_65over(map$x65_over),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 4)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_pedasthma(map$pediatric_asthma_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 5)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_adlasthma(map$adult_asthma_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 6)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_copd(map$copd_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 7)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_lngcancer(map$lung_cancer_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 8)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_hrtdisease(map$heart_disease),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 9)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_smoked(map$ever_smoked),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 10)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_poverty(map$poverty),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 11)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_poc(map$people_of_color),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 12)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_orgozone(map$orange_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 13)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_redozone(map$red_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 14)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_purpozone(map$purple_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 15)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_purpozone(map$purple_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 16)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_ozone(map$ozone_grade),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 17)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_orgpol(map$orange_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 18)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_redpol(map$red_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 19)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_purppol(map$purple_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 20)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_marpol(map$maroons_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 21)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_wgtpol(map$wgt_avg_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 22)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_pollutant(map$pollutant_grade),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 23)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_anndsg(map$annual_design_value),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select1 == 24)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_annpf(map$annual_pass_fail),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
    })
    
    output$displaymap2 = renderLeaflet({
        if (input$select2 == 1)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                
                addPolygons(
                    fillColor = ~ pal_pop(map$total_population),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 2)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_under18(map$under_18),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 3)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_65over(map$x65_over),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 4)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_pedasthma(map$pediatric_asthma_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 5)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_adlasthma(map$adult_asthma_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 6)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_copd(map$copd_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 7)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_lngcancer(map$lung_cancer_sufferers),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 8)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_hrtdisease(map$heart_disease),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 9)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_smoked(map$ever_smoked),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 10)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_poverty(map$poverty),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 11)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_poc(map$people_of_color),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 12)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_orgozone(map$orange_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 13)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_redozone(map$red_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 14)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_purpozone(map$purple_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 15)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_purpozone(map$purple_ozone_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 16)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_ozone(map$ozone_grade),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 17)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_orgpol(map$orange_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 18)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_redpol(map$red_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 19)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_purppol(map$purple_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 20)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_marpol(map$maroons_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 21)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_wgtpol(map$wgt_avg_pollutant_days),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 22)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_pollutant(map$pollutant_grade),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 23)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_anndsg(map$annual_design_value),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
        else if (input$select2 == 24)
        {
            leaflet(states) %>%
                setView(-96, 37.8, 3) %>%
                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                addPolygons(
                    fillColor = ~ pal_annpf(map$annual_pass_fail),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7
                ) %>%
                
                fitBounds(
                    ~ min(map$longitude),
                    min(map$latitude),
                    ~ max(map$longitude),
                    max(map$latitude)
                )
        }
    })
}
###########################################################################
# SHINY OBJECT
###########################################################################
shinyApp(ui = ui, server = server)