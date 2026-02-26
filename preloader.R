library(shiny)      ##for shiny dashboard
library(reactlog)   #log inputs and reactions in shiny
library(tidyverse)  #includes: ggplot2 · dplyr · tidyr · readr · purrr · tibble · stringr · forcats · lubridate
library(tidycensus) #download census data from census bureau api and clean to tidy format
library(scales)     #additional options for plot scales
library(mapview)    #show map of geo located data
library(gridlayout) # for gridded layout in shiny
library(bslib)      
library(DT)
library(waiter)     #show spinners and messages while dashboard is loading
library(shinyjs)    #I'm not sure I need this, it was in the 'waiter' demo, but I'm not sure if it's needed or if it was just for the pop-up in their sample
library(htmltools)
library(leaflet)    #Leaflet mapping packages
library(cols4all)
library(censusapi) #another census package that works differently from tidycensus

#c4a_gui()  ##see colors for all gui




stateName <- "OH"

countyNamesList <- c("Adams", "Allen", "Ashland", "Ashtabula", "Athens", "Auglaize", "Belmont", "Brown", "Butler", "Carroll", "Champaign", "Clark", "Clermont", "Clinton", "Columbiana", "Coshocton", "Crawford", "Cuyahoga", "Darke", "Defiance", "Delaware", "Erie", "Fairfield", "Fayette", "Franklin", "Fulton", "Gallia", "Geauga", "Greene", "Guernsey", "Hamilton", "Hancock", "Hardin", "Harrison", "Henry", "Highland", "Hocking", "Holmes", "Huron", "Jackson", "Jefferson", "Knox", "Lake", "Lawrence", "Licking", "Logan", "Lorain", "Lucas", "Madison", "Mahoning", "Marion", "Medina", "Meigs", "Mercer", "Miami", "Monroe", "Montgomery", "Morgan", "Morrow", "Muskingum", "Noble", "Ottawa", "Paulding", "Perry", "Pickaway", "Pike", "Portage", "Preble", "Putnam", "Richland", "Ross", "Sandusky", "Scioto", "Seneca", "Shelby", "Stark", "Summit", "Trumbull", "Tuscarawas", "Union", "Van Wert", "Vinton", "Warren", "Washington", "Wayne", "Williams", "Wood", "Wyandot ")


countyAgeBySex <- 
  get_estimates( ## Get data for 2024 from the 2024 Census Population Estimate Program
    geography = "county",
    state = stateName,
    product = "characteristics",
    breakdown = c("SEX", "AGEGROUP"),
    breakdown_labels = TRUE,
    vintage = 2024,
    year = 2024,
    cache_table = TRUE
  )

ohioAgeBySex <- get_estimates( ## Get data for 2024 from the 2024 Census Population Estimate Program
  geography = "state",
  state = "OH",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  vintage = 2024,
  year = 2024,
  cache_table = TRUE
) 


medianHousholdIncome_county <- 
  get_acs(
    geography = "county",
    state = "OH",
    ## county = input$countySelect,
    variable = "B19013_001",
    year = 2024,  
    survey = "acs5",
    ## output = "wide",
    geometry = TRUE,
    cache_table = TRUE
  )

populationTotalEst_county <- get_estimates( ## Get data for 2024 from the 2024 Census Population Estimate Program
  geography = "county",
  state = "OH",
  #    county = input$countySelect,
  variables = "POPESTIMATE",
  vintage = 2024,
  year = 2024,
  cache_table = TRUE
)

povertyRate_county <- 
  get_acs(
    geography = "county",
    state = "OH",
    #    county = "Clinton",
    #    table ="S1701",
    variable = "S1701_C03_001",
    year = 2024,  
    survey = "acs5",
    ## output = "wide",
    cache_table = TRUE
  )

countySubdivisionsPopulationStatewide <<- 
  get_acs(
    geography = "county subdivision",
    state = "OH",
    #county = "Clinton",
    #    county = input$countySelect,
    variable = "B01001A_001", ## Total Population from ACS 5 year Sex and Age Table
    year = 2024,  
    survey = "acs5",
    ## output = "wide",
    geometry = TRUE,
    cache_table = TRUE
  )

countySubdivisionsPopulationStatewide <<- countySubdivisionsPopulationStatewide %>%  ##Split NAME into subdivision, county name, and state
  mutate(
    subdivision = str_to_title(str_split_i(NAME, ",\\s", 1)),
    county = str_replace(str_split_i(NAME, ",\\s", 2), " County",""),
    state = str_split_i(NAME, ",\\s", 3)
  )

countySubdivisionsPopulationStatewide_noGeo <<- 
  get_acs(
    geography = "county subdivision",
    state = "OH",
    #county = "Clinton",
    #    county = input$countySelect,
    variable = "B01001A_001", ## Total Population from ACS 5 year Sex and Age Table
    year = 2024,  
    survey = "acs5",
    ## output = "wide",
    #geometry = TRUE,
    cache_table = TRUE
  )

countySubdivisionsPopulationStatewide_noGeo <<- countySubdivisionsPopulationStatewide_noGeo %>%  ##Split NAME into subdivision, county name, and state
  mutate(
    subdivision = str_to_title(str_split_i(NAME, ",\\s", 1)),
    county = str_replace(str_split_i(NAME, ",\\s", 2), " County",""),
    state = str_split_i(NAME, ",\\s", 3)
  )

povertyRateTimeSeries <- getCensus(
  name = "timeseries/poverty/saipe",
  vars = c("NAME", "SAEPOVRTALL_PT"),
  region = "county",
  regionin = "state:39",
  time = "from 2000")

##race

race_county <- 
  get_acs(
    geography = "county",
    state = "OH",
    #    county = input$countySelect,
    variable = c("B02001_001",	"B02001_002",	"B02001_003",	"B02001_004",	"B02001_005",	"B02001_006",	"B02001_007",	"B02001_008",	"B02001_009",	"B02001_010"),
    year = 2024,  
    survey = "acs5",
    ## output = "wide",
    cache_table = TRUE
  )

