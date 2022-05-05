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
library(ggplot2)
library(caret)
library(purrr)
library(repurrrsive)
library(glmnet)
library(ggiraph)
library(scales)
library(patchwork)
library(naniar)
library(tmap)
theme_set(theme_minimal())
options(scipen = 999)
#Who will survive the New York Floods of 2100?

#Data Extraction

#1. Load Sea Level Rise Maps (2050s 500-year Floodplain)
# Extracting the zip folder and unzipping it
floodplain500_url <- "https://data.cityofnewyork.us/api/geospatial/qwca-zqw3?method=export&format=Shapefile"

download.file( url = floodplain500_url,
destfile = "data/Sea Level Rise Maps (2050s 500-year Floodplain).zip",
mode = "wb"
)

unzip(zipfile = "data/Sea Level Rise Maps (2050s 500-year Floodplain).zip", exdir = "data/Sea Level Rise Maps (2050s 500-year Floodplain)")
file.remove("data/Sea Level Rise Maps (2050s 500-year Floodplain).zip")

#This code is better because the file name changes everytime 
floodplain500 <- list.files(path = "data", pattern="\\.shp$", full.names=TRUE)

floodplain500 <- st_read(paste0("data/Sea Level Rise Maps (2050s 500-year Floodplain)/", floodplain500)) %>%
  st_transform(crs = 4326)
class(floodplain500)

floodplain500 %>% 
rename_all(~str_to_lower(.)) %>%
#Replaces the white space in column names with underscores
rename_all(~str_replace_all(., " ", "_"))


#2. Load Sea Level Rise Maps (2050s 100-year Floodplain)
# Extracting the zip folder and unzipping it
floodplain100_url  <- "https://data.cityofnewyork.us/api/geospatial/hbw8-2bah?method=export&format=Shapefile"

download.file( url = floodplain100_url,
               destfile = "data/Sea Level Rise Maps (2050s 100-year Floodplain).zip",
               mode = "wb"
)

unzip(zipfile = "data/Sea Level Rise Maps (2050s 100-year Floodplain).zip", exdir = "data/Sea Level Rise Maps (2050s 100-year Floodplain)")
file.remove("data/Sea Level Rise Maps (2050s 100-year Floodplain).zip")


floodplain100 <- list.files(path = "data", pattern="\\.shp$", full.names=TRUE)
floodplain100 <- st_read(paste0("data/Sea Level Rise Maps (2050s 100-year Floodplain)/", floodplain100)) %>%
  st_transform(crs = 4326)
class(floodplain100)

floodplain100 %>% 
  rename_all(~str_to_lower(.)) %>%
  #Replaces the white space in column names with underscores
  rename_all(~str_replace_all(., " ", "_"))

#3. Load NYC Stormwater Flood Map - Extreme Flood Data

# Extracting the zip folder and unzipping it
stormwater_url <- "https://data.cityofnewyork.us/download/w8eg-8ha6/application%2Fzip"
download.file( url = stormwater_url,
destfile = "data/NYC Stormwater Flood Map - Extreme Flood.gdb.zip",
mode = "wb"
)
unzip(zipfile = "data/NYC Stormwater Flood Map - Extreme Flood.gdb.zip", exdir = "data")
file.remove("~/Desktop/FinalProject/data/NYC Stormwater Flood Map - Extreme Flood.gdb.zip")

stormwater <- list.files(path = "data", pattern="\\.gdb$", full.names=TRUE)
stormwater <- st_read(paste0("data/NYC Stormwater Flood Map - Extreme Flood/", stormwater)) %>%
  st_transform(crs = 4326)
class(stormwater)

#stormwater_layers <- st_layers(dsn = sw_gdbfile, options = character(0), do_count = FALSE)
#4. Load NYC Demographic Data
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
  variables = c(race,
                education_attainment,
                employment_status,
                sex_by_age,
    hhincome = "B19013_001", # median household income
    poverty = "B17020_002"), # poverty level
    key = credential,
    state = 36,
    county = c(005, 047, 061, 081, 085))

geo <- tracts(
  state = 36,
  county = c(005, 047, 061, 081, 085),
  cb = TRUE
) %>%
  st_transform(crs = 4326) %>%
  #Writes all column names in lower_case
  rename_all(~str_to_lower(.))


#```{r Data cleaning}

#Data Cleaning
demo_clean <- demogdata %>%
  select(-moe) %>%
  #Writes all column names in lower_case
  rename_all(~str_to_lower(.)) %>%
  #Replaces the white space in column names with underscores
  rename_all(~str_replace_all(., " ", "_")) %>%
  separate(name, c("census_tract","county", "state"), sep = ", ")  %>%
  pivot_wider(names_from = variable,
            values_from = estimate)
glimpse()

#To Impute Missing Data

#To see the number of missing data 
#how many?
n_miss(demo_clean)
prop_miss(demo_clean)
n_miss(demo_clean$hhincome)
# Which variables?
demo_clean %>% is.na() %>% colSums()
miss_case_summary(demo_clean)
miss_case_table(demo_clean)
vis_miss(demo_clean)

#replacing NAs with income average per county
mean_impute <- demo_clean %>%
  select(geoid,county, hhincome) %>%
  group_by(county) %>%
  filter(!is.na(hhincome)) %>%
  summarise(hhincome = mean(hhincome))
mean_impute

hhincome_test <- demo_clean %>%
  mutate(missing_hhincome = is.na(hhincome))

#testing imputations
missing_bronx <- demo_clean %>%
  filter(county == "Bronx County") %>%
  pull(missing_hhincome)

missing_mn <- demo_clean %>%
  filter(county == "New York County") %>%
  pull(missing_hhincome)

missing_rc <- demo_clean %>%
  filter(county == "Richmond County") %>%
  pull(missing_hhincome)

missing_kc <- demo_clean %>%
  filter(county == "Kings County") %>%
  pull(missing_hhincome)

missing_qc <- hhincome_test %>%
  filter(county == "Queens County") %>%
  pull(missing_hhincome)

t.test(missing_mn, missing_bronx)
t.test(missing_rc, missing_bronx)
t.test(missing_rc, missing_kc)
t.test(missing_kc, missing_qc)


#Add labs to this graph 
testing_graph <- function(county = "Queens County"){
  testing_imp <- demo_clean %>%
  filter(county == county) %>%
  select(geoid, census_tract, hhincome) %>%
  bind_shadow(only_miss = TRUE) %>%
  impute_mean_all() %>%
  add_label_shadow()  %>%
  ggplot(aes(x = hhincome, fill = any_missing)) + 
  geom_density(alpha = 0.3)
  return(testing_imp)
  }
testing_graph(county = c("Queens County","Kings County", "Bronx County","New YorkCounty"))


#Final Imputation
demo_clean <- demo_clean %>%
  left_join(mean_impute, by = c("county")) %>%
  mutate(hhincome = ifelse(is.na(hhincome.x), hhincome.y, hhincome.x)) %>%
  select(-hhincome.y, -hhincome.x)

#Prep for Demographic Mapping

#calculating the percentage of minorities
demo_clean$rate_of_minorities <- round((demo_clean$black + demo_clean$asian + demo_clean$indian_alaska + demo_clean$pacific)/(demo_clean$white + demo_clean$black + demo_clean$asian + demo_clean$indian_alaska + demo_clean$pacific) * 100, 1) %>%
  replace(., is.nan(.), 0)

#The employment rate in the tract 
demo_clean$employment_rate <- round((demo_clean$employed)/(demo_clean$employed + demo_clean$unemployed) * 100, 1) %>%
  replace(., is.nan(.), 0)

#The income level 
demo_clean %>% 
  group_by(hhincome) %>% 
  mutate(income_levels = case_when(
      hhincome < 50000 ~ "Low Income",
      hhincome < 100000 ~ "Lower Middle Income",
      hhincome <  150000 ~ "Middle Income",
      hhincome <  200000 ~ "Higher Middle Income",
      TRUE ~ "High Income"
  ))
glimpse(demo_clean)

#Plotting to explore demographic data 
#Function 
geo <- geo %>%
  select(geoid, geometry)

demography_tract <- left_join(
  x = demo_clean,
  y = geo) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

NYC_demo <- function (demo_var) {
  demo_data <- demography_tract %>%
  group_by()
  rename("estimate" = demo_var) %>%
  mutate(tooltip = str_glue(geoid, demo_var, sep = ":" ))  %>%
  ggplot(demo_data) +
  geom_sf_interactive(aes(data_id = geoid, fill = estimate, tooltip = tooltip), size = 0.2, color = NA, lwd = 0.3) +
  scale_fill_gradient(
    low = "#cfe8f3",
    high = "#062635"
  ) + 
  labs( title = paste0("Estimated", demo_var, " in ", county_name),
        subtitle = "Based on the 2020 ACS Data",
        caption = "Data Source: The US Census Bureau",
        fill = demo_var) 
  

  demo_data <- girafe(ggobj = demo_data) %>%
    girafe_options(opts_hover(css = "fill:red;"),
                 opts_zoom(max = 15))
  return(demo_data)
}

NYC_demo(unemployed)
map(.x = names(demography_tract)[4:24], .f = NYC_demo)


# label floodplain100 and floodplain500 with county name
geo_county <- geo %>%
  mutate(
    geoid = as.numeric(geoid),
    county = case_when(
      geoid < 36047000000 ~ "Bronx",
      geoid < 36061000000 ~ "Kings",
      geoid < 36081000000 ~ "New_York",
      geoid < 36085000000 ~ "Queens",
      TRUE ~ "Richmond"
    )
  )


```{r}
sf_use_s2(FALSE)
floodplain_county <- function(county_name){
  
  x <- st_union(filter(geo_county, county == county_name)) %>%
    st_as_sf(crs = 4326)
  
  y1 <- st_filter(floodplain100, x)
  y2 <- st_filter(floodplain500, x)
  
  assign(paste("floodplain100_", county_name, sep = ""), y1, envir = .GlobalEnv)
  assign(paste("floodplain500_", county_name, sep = ""), y2, envir = .GlobalEnv)
}

map(.x = c("Bronx", "Kings", "New_York", "Queens", "Richmond"), .f = floodplain_county)
``


```{r Maps}
# test
boundary_Bronx %>%
  ggplot() +
  geom_sf()

floodplain100_Bronx %>%
  ggplot() +
  geom_sf()
````


# Plot NYC demographic data with floodplain100 and floodplain 500
NYC_demo_floodplain <- function(demo_var){
  
  demo_NYU <- demogdata_sf %>%
    rename("estimate" = demo_var)
  
  ggplot() +
    geom_sf(
      data = demogdata_sf,
      mapping = aes(fill = estimate),
      color = "white"
    ) +
    scale_fill_gradient(
      low = "#cfe8f3",
      high = "#062635",
      na.value = "grey50"
    ) +
    geom_sf(
      data = floodplain500,
      fill = "red",
      color = "red",
      alpha = 0.1
    ) +
    geom_sf(
      data = floodplain100,
      fill = "yellow",
      color = "yellow",
      alpha = 0.1
    ) +
    labs(title = "New York City Floods Plain in 100 Years",
        subtitle = "Data Source: The US Census Bureau",
        caption = "Source: The City of New York",
        fill = demo_var)
  
  
# ggsave(str_glue("image/plot_", demo_var, ".png", seq = ""), width = 12, height = 8)
}

map(.x = names(demogdata_sf)[3:16], .f = NYC_demo_floodplain)


# Plot NYC demographic data by counties with floodplain100 and floodplain 500
county_demo_floodplain <- function(county_name, demo_var){
  
  demo_county <- demogdata_sf %>%
    filter(county == county_name) %>%
    rename("estimate" = demo_var)
  
  ggplot() +
    geom_sf(
      data = demo_county,
      mapping = aes(fill = estimate),
      color = "white"
    ) +
    scale_fill_gradient(
      low = "#cfe8f3",
      high = "#062635",
      na.value = "white"
    ) +
    geom_sf(
      data = get(paste("floodplain500", county_name, sep = "_")),
      fill = "red",
      color = "red",
      alpha = 0.1
    ) +
    geom_sf(
      data = get(paste("floodplain100", county_name, sep = "_")),
      fill = "yellow",
      color = "yellow",
      alpha = 0.1
    )
  
  ggsave(str_glue("image/plot", county_name, demo_var, ".png", sep = "_"), width = 12, height = 8)
  
}

map2<- map2(
  .x = rep(c("Bronx", "Kings", "New_York", "Queens", "Richmood"), each = 14),
  .y = rep(names(demogdata_sf)[3:16], times = 5),
  .f = county_demo_floodplain
)







#{r Setting Up Models}
# joins floodplain with census tract map and codes tracts by whether they are in floodplain
floodtracts <- st_join(geo,floodplain500, join = st_intersects, left = FALSE)
demogdataflood <- demogdata_clean %>%
  mutate(flood = ifelse(GEOID %in% floodtracts$GEOID,1, 0))

# plot unified data showing census tracts contained within floodplain
ggplot() +
  geom_sf(
    data= floodtracts
  )

# add county name to data indicaing which tracts are within floodplain
demogdatafloodboro <- demogdataflood %>%
  mutate(
    GEOID = as.numeric(GEOID), #Label county name
    county = case_when(
      GEOID < 36047000000 ~ "Bronx",
      GEOID < 36061000000 ~ "Kings",
      GEOID < 36081000000 ~ "New_York",
      GEOID < 36085000000 ~ "Queens",
      TRUE ~ "Richmond"))

smalldemogdataflood <- subset(demogdatafloodboro, select = -c(GEOID, NAME, county))

nadrop <- na.omit(smalldemogdataflood)
nadrop$flood <- factor(nadrop$flood)


#Predicting Part
# splits data into training and testing groups
split <- initial_split(nadrop, prop = 0.7, strata = "flood")
floodtracts_train <- training (split)
floodtracts_test <- testing (split)

# creates a recipe for the models using all variables as predictors
floodtracts_rec <-
  recipe(flood ~ ., data = floodtracts_train) %>%
  # center and scale all predictors
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # drop near zero variance for all predictors
  step_nzv(all_predictors())

# sets up resampling using 10-fold cross validation
folds <- vfold_cv(data = floodtracts_train, v = 10, repeats = 1)


#``{r CART Model}
# creates cart model
cart_mod <-
  decision_tree() %>%
  set_engine(engine = "rpart") %>%
  set_mode(mode = "classification")

# creates cart workflow
cart_wf <- workflow() %>%
  add_recipe(floodtracts_rec) %>%
  add_model(cart_mod)

# fits training data to workflow
cart_fit <- cart_wf %>%
  fit(data = floodtracts_train)

# plots decision tree
rpart.plot::rpart.plot(x = cart_fit$fit$fit$fit)

# applies model to testing data
predictions <- bind_cols(
  floodtracts_test,
  predict(object = cart_fit, new_data = floodtracts_test),
  predict(object = cart_fit, new_data = floodtracts_test, type = "prob")
)

# shows confusion matrix derived from model application
conf_mat(data = predictions,
         truth = flood,
         estimate = .pred_class)

# displays accuracy of cart model
accuracy(data = predictions,
         truth = flood,
         estimate = .pred_class)

# displays precision of cart model
precision(data = predictions,
          truth = flood,
          estimate = .pred_class)



#```{r Logistic Regression Model}
# run an initial logistic regression to get data for variable importance
floodlogit <- glm(flood ~., data = floodtracts_train, family = "binomial")

# displays importance of 10 most important variables
importances <- varImp(floodlogit)
importances %>%
  arrange(desc(Overall)) %>%
  top_n(10)

# sets up logistic regression model
logistic_mod <- logistic_reg() %>%
  set_engine("glm")

# creates LR model workflow
logistic_wf <- workflow() %>%
  add_model(logistic_mod) %>%
  add_recipe(floodtracts_rec)

# fits training data to logistic model
logistic_fit <- logistic_wf %>%
  fit(data = floodtracts_train)

# show most important variables (according to decision tree model) by borough
demogdatafloodboro %>%
  group_by(county, flood) %>%
  summarize_at(vars("employed", "poverty", "high_school", "white", "bachelor"), mean)

# applies model to testing data
predictionslog <- bind_cols(
  floodtracts_test,
  predict(object = logistic_fit, new_data = floodtracts_test),
  predict(object = logistic_fit, new_data = floodtracts_test, type = "prob")
)

# displays confusion matrix generated by logistic regression model
conf_mat(data = predictionslog,
         truth = flood,
         estimate = .pred_class)

# displays accuracy of logistic regression model
accuracy(data = predictionslog,
         truth = flood,
         estimate = .pred_class)

# displays precision of logistic regression model
precision(data = predictionslog,
          truth = flood,
          estimate = .pred_class)


#```{r KNN Model}
# sets up K-nearest neighbors model
knn_mod <-
  nearest_neighbor(neighbors = 5) %>%
  set_engine(engine = "kknn") %>%
  set_mode(mode = "classification")

# creates a knn workflow
knn_wf <-
  workflow() %>%
  add_recipe(floodtracts_rec) %>%
  add_model(knn_mod)

# fits the knn model on the training data
knn_fit <- knn_wf %>%
  fit(data = floodtracts_train)

# applies knn model to training data
predictionsknn <- bind_cols(
  floodtracts_test,
  predict(object = knn_fit, new_data = floodtracts_test)
)

# displays confusion matrix generated by knn model
conf_mat(data = predictionsknn,
         truth = flood,
         estimate = .pred_class)

# displays accuracy of knn model
accuracy(data = predictionsknn,
         truth = flood,
         estimate = .pred_class)

# displays precision of knn model
precision(data = predictionsknn,
          truth = flood,
          estimate = .pred_class)

