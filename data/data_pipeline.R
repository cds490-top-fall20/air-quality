library(readxl)
library(rmapshaper)
library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)

############################
## County-level SOTA data ##
############################

# Get housing costs & county outlines from Census Bureau
options(tigris_use_cache = TRUE)
national_county_house_prices <- get_acs(
  geography = "county",
  variables = c("DP04_0089", "DP04_0134"),
  geometry = TRUE
  )
national_counties <- national_county_house_prices %>%
  separate(
    NAME, c("county_name", "state_name"), sep = ", "
  ) %>%
  select(-moe) %>%
  spread(variable, estimate) %>%
  rename(
    median_house_value = DP04_0089,
    median_rent = DP04_0134
  )
# national_counties %>%
#   filter(GEOID == "01003")
national_counties_simplified <- national_counties %>%
  rmapshaper::ms_simplify(keep = 0.2)

sota_2020 <- read_csv("data/raw/SOTA-2020.csv")

# sota_head <- head(sota_2020)

county_fips <- NA
for (r in 1:nrow(sota_2020)){
  matching_row <- national_counties %>%
    filter(
      str_detect(state_name, sota_2020$State[r]),
      str_detect(
        str_to_lower(county_name), 
        str_to_lower(sota_2020$County[r])
      )
    )
  # print(matching_row)
  if(nrow(matching_row) == 1) {
    county_fips[r] <- matching_row$GEOID
  } else if(nrow(matching_row) > 1) {
    matching_row2 <- matching_row %>%
      filter(
        str_detect(state_name, sota_2020$State[r]),
        str_detect(
          str_to_lower(county_name), 
          str_to_lower(paste(sota_2020$County[r], "County"))
        )
      )
    if(nrow(matching_row2) == 1) {
      county_fips[r] <- matching_row2$GEOID
    }else{
      county_fips[r] <- NA
    }
  } else{
    county_fips[r] <- NA
  }
}
sota_2020$GEOID <- county_fips
sota_2020 <- select(
  sota_2020,
  GEOID, County, State, `Ozone Grade`, `Pollutant Grade`, `Annual Pass/Fail`, everything()
)


## Data from Bureau of Transportation Statistics, downloaded from
## https://data.bts.gov/Research-and-Statistics/County-Transportation-Profiles/qdmf-cxm3
county_transportation_profiles <- read_csv("data/raw/County_Transportation_Profiles.csv") %>%
  select(
    GEOID = `County FIPS`,
    commute_within = `Number of resident workers who commute within county`,
    commute_into = `Number of workers from other counties who commute to work in the county`,
    commute_out = `Number of resident workers who commute to work in other counties`,
    num_residents = `Number of residents`
  )

## NEI statistics
nei_pm2.5 <- read_excel("data/raw/nei_pm2.5_transport.xlsx")
trans_2.5 <- nei_pm2.5 %>%
  unite("GEOID", STATE_FIPS, COUNTY_FIPS, sep="") %>%
  select(GEOID, transportation_pm2.5 = EMISSIONS)


nei_trans <- read_excel("data/raw/nei_transportation .xlsx")
trans_nox <- nei_trans %>% 
  unite("GEOID", STATE_FIPS, COUNTY_FIPS, sep="") %>%
  select(GEOID, transportation_nitrogen_oxides = EMISSIONS)



census_plus_sota <- left_join(
  national_counties_simplified, 
  sota_2020, 
  by = "GEOID") %>%
  select(
    -County, -State
  )


final_df <- census_plus_sota %>%
  left_join(county_transportation_profiles, by = "GEOID") %>%
  left_join(trans_2.5, by = "GEOID") %>%
  left_join(trans_nox, by = "GEOID")

saveRDS(final_df, "app/data/counties_mapping_data_simplified.rds")


#############################
# SIMPLIFY URBAN BOUNDARIES #
#############################

urban <- st_read("data/raw/urban_areas_shapefile_2019/cb_2018_us_ua10_500k.shp") %>%
  select(NAME10) %>%
  mutate(
    name = as.character(NAME10),
    area = st_area(geometry),
    centroid = st_coordinates(st_centroid(geometry)),
    lat = centroid[,2],
    long = centroid[,1]
  ) %>%
  arrange(desc(area)) %>%
  slice(1:500)

attr(urban$lat, "names") <- NULL
attr(urban$long, "names") <- NULL

urban %>%
  select(-area, -centroid) %>%
  rmapshaper::ms_simplify(keep = 0.1) %>%
  saveRDS("app/data/urban_areas_simplified.rds")


#################################
# Process EPA monitor locations #
#################################

# Data scource: http://files.airnowtech.org/?prefix=airnow/2019/20191231/
# Schema: https://docs.airnowapi.org/docs/MonitoringSiteFactSheet.pdf

epa_sites <- read_delim("~/Downloads/monitoring_site_locations.dat", "|", col_names=F) %>%
  select(
    id = X1,
    pollutant = X2,
    status = X5,
    lat = X9,
    long = X10
  )
epa_sites %>%
  filter(
    pollutant == "PM2.5" | pollutant == "O3"#, status == "Active"
  ) %>%
  pivot_wider(
    names_from = pollutant,
    values_from = id
  ) %>%
  select(
    lat, long
  ) %>%
  saveRDS("app/data/epa_sensor_locations.rds")

