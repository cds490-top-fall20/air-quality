#########################################################
# data aggregation
#########################################################
library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(reshape2)
library(leafpop)
shape <- tigris::counties(state = "VA", class = "sf")
sota_fips <- read_csv("SOTA_FIPS_COMPLETE.csv")
nei <- read_csv("nei_virginia.csv")
hub_info <- read_csv("hub_info.csv")

shape$GEOID <- as.numeric(shape$GEOID)

shape <- select(
  shape,
  STATEFP,
  COUNTYFP,
  GEOID,
  NAMELSAD,
  COUNTYNS,
  geometry
)

Transit <- hub_info %>%
  rename(
    GEOID = County_FIPS
  )

Transit$GEOID <- as.numeric(Transit$GEOID)
Transit$`Total Marinas`


Transit <- select(
  Transit,
  GEOID,
  `Non-Commercial -Civil Public Use Airports and Seaplane base`,
  `Non-Commercial -Other Aerodromes`,
  `Number of resident workers`:`Number of workers from other counties who commute to work in th`,
  `Percent of resident workers who commute by transit`,
  `Primary and Commercial Airports`,
  `Total Docks`,
  `Total Marinas`
)



y <- left_join(shape,Transit, by = "GEOID")

SOTA <- left_join(y, sota_fips, by = "GEOID")

SOTA_nei <- left_join(SOTA, nei, by = "GEOID")

full_data <- select(
  SOTA_nei,
  NAMELSAD,
  GEOID,
  `Total Pop`:`Ever Smoker`,
  `Number of resident workers`:`Number of workers from other counties who commute to work in th`,
  `Percent of resident workers who commute by transit`,
  `Non-Commercial -Civil Public Use Airports and Seaplane base`,
  `Non-Commercial -Other Aerodromes`,
  `Total Docks`,
  `Total Marinas`,
  `High Ozone Day Grade`:`Particle Pollution Design Value`,
  TIER,
  POLLUTANT:EMISSIONS,
  geometry
)

#full_data <- mutate(
#  SOTA_nei,
#  smoker_ratio = `Ever Smoker`/`Total Pop`,
#  pediatric_asthma_ratio = `Pediatric Asthma`/`Total Pop`,
#  adult_asthma_ratio = `Adult Asthma`/`Total Pop`
#)
