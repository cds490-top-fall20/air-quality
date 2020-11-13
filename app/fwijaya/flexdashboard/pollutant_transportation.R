

#This data is taken from NEI website for the transportation Pollutant that 
#we would like to see in each county.

library(dplyr)

library(readxl)

#the pollutant that are being considered is Nitrogen Oxide and pm2.5

nei_transportation <- read_excel("app/dominic/flexdashboard/data/nei_transportation .xlsx")
View(nei_transportation)

nei_pm2.5_transport <- read_excel("app/dominic/flexdashboard/data/nei_pm2.5_transport.xlsx")
View(nei_pm2.5_transport)

#the next step is joining two datasets via state_fips = statefips and county = county
# by all the rows that match each other. 


NEW <- full_join(nei_transportation,nei_pm2.5_transport, by = c("STATE_FIPS", "COUNTY_FIPS"))



nei_pm2.5_transport













