#install the following packages and load the libraries
library(tidycensus)
library(tidyverse)
library(tidymodels)
library(tigris)
library(randomForest)
library(ranger)
library(parsnip)
library(janitor)
library(sf)
library(ggiraph)
theme_set(theme_minimal())

#Data Extraction


#````{r, echo = FALSE}
#1. Load Sea Level Rise Maps (2050s 500-year Floodplain)

# Extracting the zip folder and unzipping it
floodplain <- "https://data.cityofnewyork.us/api/geospatial/qwca-zqw3?method=export&format=Shapefile"

download.file( url = floodplain,
               destfile = "data/sea_level_map.zip",
               mode = "wb"
)

unzip(zipfile = "data/sea_level_map.zip", exdir = "data/sea_level_map")
file.remove("data/sea_level_map.zip")

floodplain <- list.files(path = "data", pattern="\\.shp$", full.names=TRUE)

floodplain %>%
  rename_all(~str_to_lower(.)) %>%
  #Replaces the white space in column names with underscores
  rename_all(~str_replace_all(., " ", "_"))
#````


#````{r, echo = FALSE}

#2. Load NYC Stormwater Flood Map - Extreme Flood Data

# Extracting the zip folder and unzipping it
stormwater <- "https://data.cityofnewyork.us/download/w8eg-8ha6/application%2Fzip"
download.file( url = stormwater,
               destfile = "data/NYC Stormwater Flood Map - Extreme Flood.gdb.zip",
               mode = "wb"
)

unzip(zipfile = "data/NYC Stormwater Flood Map - Extreme Flood.gdb.zip", exdir = "data")

file.remove("~/Desktop/FinalProject/data/NYC Stormwater Flood Map - Extreme Flood.gdb.zip")

sw_gdbfile <- list.files(path = "data", pattern="\\.gdb$", full.names=TRUE)

#Read in NYC Stormwater Flood Map Data 
#This is a 3-D object, we should convert to 2-D
stormwater <- st_read(sw_gdbfile)  %>%
  rename_all(~str_to_lower(.)) %>%
  #Replaces the white space in column names with underscores
  rename_all(~str_replace_all(., " ", "_"))

stormwater_layers <- st_layers(dsn = sw_gdbfile, options = character(0), do_count = FALSE)

#````


#````{r, echo = FALSE}
#3. Load NYC Demographic Data

credential <- Sys.getenv("census_api_key")

lookup_var <- load_variables(2020, "acs5", cache = TRUE)
glimpse(lookup_var)

race <- c(white = "B02001_002", #race
          black = "B02001_003",
          indian_alaska = "B02001_004",
          asian = "B02001_005",
          pacific = "B02001_006")

education_attainment <- c(no_schooling = "B15003_002", #education_attainment
                          elementary_school = "B15003_009",
                          secondary_school = "B15003_012",
                          high_school = "B15003_017",
                          bachelor = "B15003_022")

employment_status <- c(employed = "B23025_004", # employment_status
                       unemployed = "B23025_005",
                       no_in_labor_force = "23025_007")
sex_by_age <- c(total_by_age ="B01001_001",  #sex_by_age
                male_by_age_total = "B01001_002", 
                female_by_age_total = "B01001_026")


demogdata <- get_acs(
  geography = "tract",
  year = 2020,
  variables = c(hhincome = "B19013_001", # median household income
                poverty = "B17020_002", # poverty level
                sex_by_age,
                employment_status,
                education_attainment,
                race
  ),
  state =  36,
  county = c(005,047,051,081,085),
  key = credentia
)


geo <- tracts(
  state = 36,
  county = c(005,047,051,081,085)
) %>%
  #Writes all column names in lower_case  
  rename_all(~str_to_lower(.)) %>%
  #Replaces the white space in column names with underscores
  rename_all(~str_replace_all(., " ", "_")) 


#Data Cleaning 
demogdata_clean <- demogdata %>%
  select(-moe) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  #Writes all column names in lower_case  
  rename_all(~str_to_lower(.)) %>%
  #Replaces the white space in column names with underscores
  rename_all(~str_replace_all(., " ", "_"))  %>%
  
#names <-  demogdata_clean  %>% 
#  select(geoid, name) %>% 
  #  mutate(name=strsplit(name, ",")) %>% 
  #  unnest(name)  %>% 
  # subset(name != " New York")  %>% 
  
  
# how to deal with missing value for household income especially since it is important?

geo <- geo %>%
  select(geoid, geometry)

demogdata_sf <- left_join(
  x = demogdata_clean, 
  y = geo
) %>%
  st_as_sf() %>%
  mutate(
    geoid = as.numeric(geoid),
    county = case_when(
      geoid < 36047000000 ~ "Bronx",
      geoid < 36051000000 ~ "Kings",
      geoid < 36081000000 ~ "Livingston",
      geoid < 36085000000 ~ "Queens",
      TRUE ~ "Richmood"
    )
  ) 



#Will make interactive 
floodplain %>%
  ggplot () + 
  geom_sf()
#Some missing data here too

demogdata_sf %>%
  filter(county == "Kings") %>%
  ggplot () + 
  geom_sf()
scale_fill_viridis_d() 
