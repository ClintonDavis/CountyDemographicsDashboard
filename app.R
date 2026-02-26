## Run Preload Script before using!

## Shiny code adapted from R for the Rest of us Course Demo (Ted Laderas)
## 
## TidyCensus code adapted from Kyle Walker's sample code from this 2025 UM-SSDAN Webinar and website:
## 2025 Webinar 1 Analyzing Data from the 2023 American Community Survey in R with Kyle Walker - https://www.youtube.com/watch?v=9a8_p_q4Z34
## Analyzing US Census Data: Methods, Maps, and Models in R - https://walker-data.com/census-r/exploring-us-census-data-with-visualization.html
##
## Population Pyramid formatting from R for the Rest of us (David Keyes) - https://rfortherestofus.com/2024/07/population-pyramid-part-1
##
## Last update 2/26/2026 - cd

library(shiny)      ##for shiny dashboard
library(reactlog)   #log inputs and reactions in shiny
library(tidyverse)  #includes: ggplot2 · dplyr · tidyr · readr · purrr · tibble · stringr · forcats · lubridate
library(tidycensus) #download census data from census bureau api and clean to tidy format
library(scales)     #additional options for plot scales
library(bslib)      #boostrap
library(DT)         #data tables
library(waiter)     #show spinners and messages while dashboard is loading
library(shinyjs)    #I'm not sure I need this, it was in the 'waiter' demo, but I'm not sure if it's needed or if it was just for the pop-up in their sample
library(htmltools)
library(tmap)       #mapping package (Tennekes 2018)
library(cols4all)
library(censusapi) #another census package that works differently from tidycensus

## Libraries that are not currently in use, but either were used in a different version or may become useful

##library(plotly)          ### interactive plots - I can't get my axis formatting to follow from ggplot to ggplotly, so I'm not using it until I figure out a solution
## library(export)          ### saving for later to export charts to office formats, not for shiny dashboard
## library(gghighlight)     ### highlight individual data in a plot
## library(shinycssloaders) ### loading animations, but changes plot sizes in gridlayout
## library(patchwork)       ### for better population pyramid, but can't get to work in Shiny
## library(export)          ### saving for later to export charts to office formats, not for shiny dashboard
## library(leaflet)    #Leaflet mapping packages
## library(mapview)    #show map of geo located data
## library(gridlayout) # for gridded layout in shiny
## library(mapboxapi)

options(tigris_use_cache = TRUE) #I assume this caches the geography geometry for census data

## census_api_key("f4baa8d96c220aecb6fc984f12d36da696473348", install = TRUE)  #Atriloba Limited's Census API Key

## Load list of variables available from TidyCensus, not needed for Shiny Dashboard, but helpful during development
# vars <- load_variables(2024,"acs5")
# View(vars)

reactlog_enable()

stateName <- "OH"

countyNamesList <- c("Adams", "Allen", "Ashland", "Ashtabula", "Athens", "Auglaize", "Belmont", "Brown", "Butler", "Carroll", "Champaign", "Clark", "Clermont", "Clinton", "Columbiana", "Coshocton", "Crawford", "Cuyahoga", "Darke", "Defiance", "Delaware", "Erie", "Fairfield", "Fayette", "Franklin", "Fulton", "Gallia", "Geauga", "Greene", "Guernsey", "Hamilton", "Hancock", "Hardin", "Harrison", "Henry", "Highland", "Hocking", "Holmes", "Huron", "Jackson", "Jefferson", "Knox", "Lake", "Lawrence", "Licking", "Logan", "Lorain", "Lucas", "Madison", "Mahoning", "Marion", "Medina", "Meigs", "Mercer", "Miami", "Monroe", "Montgomery", "Morgan", "Morrow", "Muskingum", "Noble", "Ottawa", "Paulding", "Perry", "Pickaway", "Pike", "Portage", "Preble", "Putnam", "Richland", "Ross", "Sandusky", "Scioto", "Seneca", "Shelby", "Stark", "Summit", "Trumbull", "Tuscarawas", "Union", "Van Wert", "Vinton", "Warren", "Washington", "Wayne", "Williams", "Wood", "Wyandot ")

##Download Data from Census Bureau, can either be called from within dashboard or preloaded prior to launching Shiny App
downloadDataFromCensusBureau <- function() { # create a function with the name my_function
  
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
}

##########################################################################
##############################       #####################################
##############################  UI   #####################################
##############################       #####################################
##########################################################################

ui <- page_fluid(
  page_navbar(
    title = "County Demographics - Ohio", 
    sidebar = card(                 
      selectInput(inputId = "countySelect", label = "Select County", choices = countyNamesList),
      plotOutput(outputId = "selectedCountyMap", height = 250),
      htmlOutput(outputId = "countyPopulation2024"),
      htmlOutput(outputId = "mhi_county"),
      htmlOutput(outputId = "povRate_county")
    ),
    navset_tab(
      nav_panel(title = "Population", 
                layout_columns(
                  card(height = 800, plotOutput(outputId = "countyPopulationsPlot")),
                  card(plotOutput(outputId = "countyPopulationPyramid_2024"),
                       plotOutput(outputId = "OhioPopulationPyramid_2024"),
                  ),
                  card(
                    card(height = 600, tmapOutput(outputId = "subdivisionTMap")),
                    card(height = 200, div(dataTableOutput(outputId = "subdivisionTable"), style = "font-size:70%"))
                  ),
                  col_widths = c(3, 3, 6)
                ),
      ),
      nav_panel(title = "Income", 
                layout_columns(
                  card(height = 800, plotOutput(outputId = "mhi_countyPlot")),
                  card(plotOutput(outputId = "MHIMap")),
                  col_widths = c(3, 9)
                ),
                #Add map of counties by MHI
      ),
      #      nav_panel(title = "Race and Ethnicity"),
      nav_panel(title = "Poverty",
                layout_columns(
                  card(height = 800, plotOutput(outputId = "pov_countyPlot")),
                  card(height = 400, plotOutput(outputId = "povRate_CountyTimeseries")),
                  col_widths = c(3, 9)
                ),
      ),
      #      nav_panel(title = "Education"),
      #      nav_panel(title = "Workforce"),
      #      nav_panel(title = "Industry"),
      
    ),
    
    footer = htmlOutput(outputId = "notes"), 
  ),
  
  ##wait screen
  
  autoWaiter(color= "#006699"),
  waiterPreloader(
    html = "<h1>County Demographics Dashboard </h1> <br> <h3>Loading Census Data</h3> <br> <h4>This might take a minute...</h4>",
    color= "#006699", 
    fadeout = TRUE
  ),
  
)

##########################################################################
##############################           #################################
##############################  SERVER   #################################
##############################           #################################
##########################################################################

server <- function(input, output) {
  
  #  downloadDataFromCensusBureau() #comment out if data is preloaded in environment
  
  countyAgeBySex_filtered <- reactive({
    filter(countyAgeBySex, str_detect(AGEGROUP, "^Age"),
           SEX != "Both sexes" & NAME == paste(input$countySelect, "County, Ohio")) %>%
      mutate(value = ifelse(SEX == "Male", -value, value))
  })
  
  
  output$countyPopulationPyramid_2024 <- renderPlot(
    ggplot(countyAgeBySex_filtered(),
           aes(x = value,
               y = AGEGROUP,
               fill = SEX)) +
      geom_col(width = 0.75, alpha = 0.75) +
      theme_minimal(base_size = 12) +
      scale_x_continuous(
        labels = ~ label_comma()(abs(.x))
      ) +
      scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
      scale_fill_manual(values = c("#CC0066", "#006699"), guide = FALSE) + # Colors from the NCAP Community Action Style Guide: https://communityactionpartnership.com/branding/
      labs(x = "",
           y = "",
           title = paste(input$countySelect, "County, Ohio Population Pyramid - 2024"),
           subtitle = "",
           fill = "",
           caption = "Data: 2024 Census Population Estimate Prog. via tidycensus"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot",
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            plot.caption.position = "plot"
      )
    
  )
  
  ## State Pyramid
  
  ohioAgeBySex_filtered <- filter(ohioAgeBySex, str_detect(AGEGROUP, "^Age"),
                                  SEX != "Both sexes") %>%
    mutate(value = ifelse(SEX == "Male", -value, value))
  
  output$OhioPopulationPyramid_2024 <- renderPlot(
    ggplot(ohioAgeBySex_filtered,
           aes(x = value,
               y = AGEGROUP,
               fill = SEX)) +
      geom_col(width = 0.75, alpha = 0.75) +
      theme_minimal(base_size = 12) +
      scale_x_continuous(
        labels = ~ label_comma()(abs(.x)),
      ) +
      scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
      scale_fill_manual(values = c("#CC0066", "#006699"), guide = FALSE) + # Colors from the NCAP Community Action Style Guide: https://communityactionpartnership.com/branding/
      labs(x = "",
           y = "",
           title = "Ohio Population Pyramid - 2024",
           subtitle = "",
           fill = "",
           caption = "Data: 2024 Census Population Estimate Prog. via tidycensus"
      )+
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot",
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            plot.caption.position = "plot"
      )
  )
  
  ###### Total Populations
  
  #### County Population Graph
  
  populationTotalEst_county <- arrange(populationTotalEst_county, value)
  pop <- populationTotalEst_county$value
  names <- factor(gsub(' County, Ohio', '', populationTotalEst_county$NAME))
  highlight <- reactive({
    ifelse(names == input$countySelect, "#CC0066", "#006699")
  })
  
  countyPopulationsPlot <- reactive({  
    ggplot(populationTotalEst_county) +
      geom_col(
        aes(
          x = pop, 
          y = reorder(names, pop), 
          pop, 
          fill = highlight(), 
          width = 0.6)) +
      labs(x = "", y = "", 
           title = "2024 County Populations",
           subtitle = "Census Population Estimate Program",
           caption = "Data: 2024 Census Population Estimate Prog. via tidycensus") +
      scale_fill_manual( 
        values = c( "#CC0066" = "#CC0066", "#006699" = "#006699"), 
        guides(color = "none") ) +
      theme_minimal() +
      scale_x_continuous(labels = label_comma()) +
      theme(legend.position="none") +
      theme(
        axis.text.y = element_text( color = highlight()),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        plot.caption.position = "plot"
      )
  })
  
  output$countyPopulationsPlot <- renderPlot(countyPopulationsPlot())  
  
  ### County Subdivisions Population Map
  
  selectedCountySubdivisionsPopulation <- reactive({
    filter(countySubdivisionsPopulationStatewide, county == input$countySelect)
  })
  
  selectedCountySubdivisionsPopulationTMap <-
    renderTmap({
      tm_shape(selectedCountySubdivisionsPopulation()) +
        tm_polygons(fill = "estimate",
                    fill.scale = tm_scale_intervals(values = "kovesi.blue"),
                    fill_alpha = 1,
                    fill.legend = 
                      tm_legend(title = "2024 Population Estimates",
                                show = TRUE,
                                orientation = "landscape",
                                position = tm_pos_out("center", "bottom", pos.h = "center")
                      )
        ) +
        tm_text("subdivision") +  #removed, but kept for later:  options = opt_tm_text(remove_overlap = TRUE)
        tm_title(paste(input$countySelect, "County, Ohio - 2024 Population Estimates"),
                 position = tm_pos_out("center", "top")) +
        tm_layout(
          frame = FALSE,
          legend.outside = TRUE
        )+
        tm_credits("Data: 2024 Census ACS 5yr Est. via tidycensus")
    })
  
  output$subdivisionTMap <- selectedCountySubdivisionsPopulationTMap
  
  ## Subdiv table  
  
  selectedCountySubdivisionsPopulation_noGeo <- reactive({
    filter(countySubdivisionsPopulationStatewide_noGeo, county == input$countySelect)
  })
  
  subPopTable <- reactive({
    datatable(
      arrange(selectedCountySubdivisionsPopulation_noGeo()[c("NAME", "estimate")], desc(selectedCountySubdivisionsPopulation_noGeo()$"estimate")),
      colnames = c("2024 Population Estimate"),
      rownames = FALSE,
      options = list(dom = 't')  #column names not working and I can't get rid of [object Object] column
    )
  })
  
  output$subdivisionTable <- renderDataTable(subPopTable())
  
  #### County information Text
  
  populationTotalEst_county_filtered <- reactive({
    filter(populationTotalEst_county,
           NAME == paste(input$countySelect, "County, Ohio"))
  })
  
  output$countyPopulation2024 <- renderText({
    paste("<h5>2024 Total Population:</h5>", "<h1 style = color:#006699>", format(populationTotalEst_county_filtered()$value, big.mark=","), "</h1>")
  })
  
  
  ###### Median Household Income
  
  medianHousholdIncome_county <- arrange(medianHousholdIncome_county, estimate)
  MHInames <- factor(gsub(' County, Ohio', '', medianHousholdIncome_county$NAME))
  MHI <- medianHousholdIncome_county$estimate
  MHImoe <- medianHousholdIncome_county$moe
  
  MHIhighlight <- reactive({
    ifelse(MHInames == input$countySelect, "#CC0066", "#006699")
  })
  
  medianHousholdIncome_county_filtered <- reactive({
    filter(medianHousholdIncome_county,
           NAME == paste(input$countySelect, "County, Ohio"))
  })
  
  ## MHI-County text as html
  
  output$mhi_county <- renderText({
    paste("<h5> 2024 Median Household Income:</h5>", "<h1 style = color:#006699>", "$", format(medianHousholdIncome_county_filtered()$estimate, big.mark=","), "</h1>")
  })
  
  ### MHI Plot with error bars adapted from Kyle Walker SSDAN 2025 webinar
  
  ohioMHIPlot_errorbar <- reactive({
    ggplot(medianHousholdIncome_county, aes(x = MHI, y = reorder(MHInames, MHI), MHI)) + 
      geom_errorbar(aes(xmin = MHI - MHImoe, xmax = MHI + MHImoe), width = 0.5, linewidth = 0.5) +
      geom_point(color = "darkgrey", shape = 21, size = 3) + 
      scale_x_continuous(labels = label_dollar()) + 
      scale_y_discrete(labels = MHInames) +
      labs(x = "", y = "",
           title = "2024 Median Household Income",
           subtitle = "Census ACS 5-yr Estimate",
           caption = "Data: 2024 Census ACS 5yr Est. via tidycensus"
      ) + 
      aes(fill = MHIhighlight()) +
      scale_fill_manual( values = c( "#CC0066" = "#CC0066", "#006699" = "#006699"), guide = FALSE)+
      theme_minimal()+
      theme(axis.text.y = element_text( color = MHIhighlight()),
            plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot",
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            plot.caption.position = "plot"
      )
  })
  
  output$mhi_countyPlot <- renderPlot(ohioMHIPlot_errorbar())
  
  
  ### MHI Map
  
  medianHousholdIncome_county <- medianHousholdIncome_county %>%
    mutate(
      county = str_replace(str_split_i(NAME, ",\\s", 1), " County","")
    )
  
  MHIMap_counties <-
    renderTmap({
      tm_shape(medianHousholdIncome_county) +
        tm_polygons(fill = "estimate",
                    fill.scale = tm_scale_intervals(n=7, values = c("#ffe6f3ff", "#006699")),  #, label.format = label_currency(prefix = "$", big.mark = ",")
                    fill_alpha = 1,
                    fill.legend = 
                      tm_legend(title = "2024 Median Household Income",
                                show = TRUE,
                                orientation = "landscape",
                                position = tm_pos_out("center", "bottom", pos.h = "center")
                      )
        ) +
        tm_text("county") +  #removed, but kept for later:  options = opt_tm_text(remove_overlap = TRUE)
        tm_title(paste(input$countySelect, "County, Ohio - 2024 Median Household Income"),
                 position = tm_pos_out("center", "top")) +
        tm_layout(
          frame = FALSE,
          legend.outside = TRUE
        )+
        tm_credits("Data: 2024 Census ACS 5yr Est. via tidycensus")
    })
  
  output$MHIMap <- MHIMap_counties
  
  
  
  
  #### Selected County Map
  
  selectedCountyMap <- reactive({ 
    ggplot(medianHousholdIncome_county) +
      geom_sf(fill = MHIhighlight(), color = "grey", linewidth = 0.3) +
      theme_void()
  })  
  
  output$selectedCountyMap <- renderPlot(selectedCountyMap())
  
  
  #### Poverty Rate
  
  povertyRate2024_county <- filter(povertyRateTimeSeries, time == "2024")
  
  povertyRate2024_county <- arrange(
    povertyRate2024_county, SAEPOVRTALL_PT) 
  
  POVnames <- factor(gsub(' County', '', povertyRate2024_county$NAME))
  POV <- povertyRate2024_county$SAEPOVRTALL_PT
  
  POVhighlight <- reactive({
    ifelse(POVnames == input$countySelect, "#CC0066", "#006699")
  })
  
  
  povertyRate2024_countiesPlot <- reactive({
    ggplot(povertyRate2024_county, aes(x = (POV/100), y = reorder(POVnames, POV), POV)) +
      geom_point(color = "darkgrey", shape = 21, size = 3) + 
      scale_x_continuous(labels = label_percent()) + 
      scale_y_discrete(labels = POVnames) +
      labs(x = "", y = "",
           title = "2024 Poverty Rate",
           subtitle = "Census SAIPE Estimate",
           caption = "Data: 2024 Census SAIPE via censusapi"
      ) + 
      aes(fill = POVhighlight()) +
      scale_fill_manual( values = c( "#CC0066" = "#CC0066", "#006699" = "#006699"), guide = FALSE)+
      theme_minimal()+
      theme(axis.text.y = element_text( color = POVhighlight()),
            plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot",
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            plot.caption.position = "plot"
      )
  })
  
  output$pov_countyPlot <- renderPlot(povertyRate2024_countiesPlot())
  
  
  POVhighlight <- reactive({
    ifelse(POVnames == input$countySelect, "#CC0066", "#006699")
  })
  
  povertyRate_county_filtered <- reactive({
    filter(povertyRateTimeSeries,
           NAME == paste(input$countySelect, "County"), time == "2024"
    )
  })  
  
  output$povRate_county <- renderText({
    paste("<h5>2024 Poverty Rate:</h5>", "<h1 style = color:#006699>", format(povertyRate_county_filtered()$SAEPOVRTALL_PT, big.mark= ","), "% </h1>")
  }) 
  
  povertyRateTimeSeries_county_filtered <- reactive({
    filter(povertyRateTimeSeries,
           NAME == paste(input$countySelect, "County"))
  })
  
  povRate_CountyTimeseries <- reactive({
    ggplot(
      povertyRateTimeSeries_county_filtered(), 
      aes(x = time, y = SAEPOVRTALL_PT/100)) +
      geom_area(fill="#006699", alpha=0.4)+
      geom_line(color="#006699", size=2)+
      geom_point(color="#006699", size=3)+
      scale_y_continuous(labels = label_percent())+
      labs(x = "", y = "",
           title = paste(input$countySelect, "County", "Poverty Rate: 2000-2024"),
           subtitle = "Census SAIPE Estimate",
           caption = "Data: 2024 Census SAIPE via censusapi"
      ) + 
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.title.position = "plot",
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            plot.caption.position = "plot"
      )
  })
  
  output$povRate_CountyTimeseries <- renderPlot(povRate_CountyTimeseries())
  
  
  #### Note Block
  
  output$notes <- renderText({
    paste("<b>Do not actually rely on this dashboard for any purpose. <br> This is very much a learning activity and almost certainly contains error.</b>", "Data was preloaded from the US Census Bureau API using the tidycensus R package.", "<br><br>", "I've borrowed heavily from:", "<br>", "Ted Laderas's sample code from his R for the Rest of Us Shiny Course at https://rfortherestofus.com and", "<br>", "Kyle Walker's samples from his website and tidycensus webinar at U-M SSDAN: https://walker-data.com </small> ")
  })
  
  waiter_hide()  ## Removes loading screen once the startup process are done
  
}

# Run the application 
shinyApp(ui = ui, server = server)
