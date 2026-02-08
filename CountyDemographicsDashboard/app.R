## Run Preload Script before using!

## Shiny code adapted from R for the Rest of us Course Demo (Ted Laderas)
## 
## TidyCensus code adapted from Kyle Walker's sample code from this 2025 UM-SSDAN Webinar and website:
## 2025 Webinar 1 Analyzing Data from the 2023 American Community Survey in R with Kyle Walker - https://www.youtube.com/watch?v=9a8_p_q4Z34
## Analyzing US Census Data: Methods, Maps, and Models in R - https://walker-data.com/census-r/exploring-us-census-data-with-visualization.html
##
## Population Pyramid formatting from R for the Rest of us (David Keyes) - https://rfortherestofus.com/2024/07/population-pyramid-part-1
##
## Last update 2/8/2026 - cd

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

## Libraries that are not currently in use, but either were used in a different version or may become useful

##library(plotly)          ### interactive plots - I can't get my axis formatting to follow from ggplot to ggplotly, so I'm not using it until I figure out a solution

## library(export)          ### saving for later to export charts to office formats, not for shiny dashboard
## library(gghighlight)     ### highlight individual data in a plot
## library(shinycssloaders) ### loading animations, but changes plot sizes in gridlayout
## library(patchwork)       ### for better population pyramid, but can't get to work in Shiny
## library(export)          ### saving for later to export charts to office formats, not for shiny dashboard


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
  
  countyAgeBySex <<- 
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
  
  ohioAgeBySex <<- get_estimates( ## Get data for 2024 from the 2024 Census Population Estimate Program
    geography = "state",
    state = "OH",
    product = "characteristics",
    breakdown = c("SEX", "AGEGROUP"),
    breakdown_labels = TRUE,
    vintage = 2024,
    year = 2024,
    cache_table = TRUE
  ) 
  
  medianHousholdIncome_county <<- 
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
  
  populationTotalEst_county <<- get_estimates( ## Get data for 2024 from the 2024 Census Population Estimate Program
    geography = "county",
    state = "OH",
    #    county = input$countySelect,
    variables = "POPESTIMATE",
    vintage = 2024,
    year = 2024,
    cache_table = TRUE
  )
  
  povertyRate_county <<-
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
}

ui <- grid_page(
  
  ##wait screen
  
  autoWaiter(color= "#006699"),
  waiterPreloader(
    html = "<h1>County Demographics Dashboard </h1> <br> <h3>Loading Census Data</h3> <br> <h4>This might take a minute...</h4>",
    color= "#006699", 
    fadeout = TRUE
  ),
  
  
  layout = c(
    "header               header                       header                       header                       header                            ",
    "sidebar              countyPopulationsPlot        mhi_countyPlot               countyPopulationPyramid_2024 OhioPopulationPyramid_2024        ",
    "sidebar              countyPopulationsPlot        mhi_countyPlot               countyPopulationPyramid_2024 OhioPopulationPyramid_2024        ",
    "countyPopulation2024 countyPopulationsPlot        mhi_countyPlot               countyPopulationPyramid_2024 OhioPopulationPyramid_2024        ",
    "mhi_county           countyPopulationsPlot        mhi_countyPlot               .                            .                                 ",
    "povRate_county       countyPopulationsPlot        mhi_countyPlot               notes                        notes                             ",
    "selectedCountyMap    countyPopulationsPlot        mhi_countyPlot               notes                        notes                             "
  ),
  row_sizes = c(
    "75px",
    "1fr",
    "1fr",
    "1fr",
    "1fr",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr",
    "1fr",
    "1fr",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_body(
      selectInput(
        inputId = "countySelect",
        label = "Select County",
        choices = countyNamesList
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "County Demographics - Ohio",
    alignment = "center",
    is_title = FALSE
  ),
  
  grid_card(
    area = "OhioPopulationPyramid_2024",
    card_body(plotOutput(outputId = "OhioPopulationPyramid_2024"))
    
  ),
  grid_card(
    area = "countyPopulationPyramid_2024",
    card_body(plotOutput(outputId = "countyPopulationPyramid_2024"))
  ),
  grid_card(
    area = "countyPopulation2024",
    card_body(htmlOutput(outputId = "countyPopulation2024"))
  ),
  grid_card(
    area = "countyPopulationsPlot",
    card_body(plotOutput(outputId = "countyPopulationsPlot"))
  ),
  grid_card(
    area = "mhi_county",
    card_body(htmlOutput(outputId = "mhi_county"))
  ),
  grid_card(
    area = "povRate_county",
    card_body(htmlOutput(outputId = "povRate_county"))
  ),  
  grid_card(
    area = "mhi_countyPlot",
    card_body(plotOutput(outputId = "mhi_countyPlot"))
  ),
  grid_card(
    area = "selectedCountyMap",
    card_body(plotOutput(outputId = "selectedCountyMap"))
  ),
  grid_card(
    area = "notes",
    card_body(htmlOutput(outputId = "notes"))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    downloadDataFromCensusBureau() #comment out if data is preloaded in environment
  
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
           caption = "Data source: 2024 US Census Bureau Population Estimates via the tidycensus R package"
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
           caption = "Data source: 2024 US Census Bureau Population Estimates via the tidycensus R package"
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
           caption = "Data source: 2024 US Census Bureau Population Estimates via the tidycensus R package") +
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
  
  
  #### County information Text
  
  populationTotalEst_county_filtered <- reactive({
    filter(populationTotalEst_county,
           NAME == paste(input$countySelect, "County, Ohio"))
  })
  
  output$countyPopulation2024 <- renderText({
    paste("2024 Total Population:", "<h1 style = color:#006699>", format(populationTotalEst_county_filtered()$value, big.mark=","), "</h1>")
  })
  
  #  output$MHIMap <- renderPlot(plot(medianHousholdIncome_county))
  
  output$selectedCountyMap <- renderPlot(selectedCountyMap())
  
  
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
    paste("2024 Median Household Income:", "<h1 style = color:#006699>", "$", format(medianHousholdIncome_county_filtered()$estimate, big.mark=","), "</h1>")
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
           caption = "Data source: 2024 US Census ACS 5 Year Estimates via the tidycensus R package"
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
  
  #### Selected County Map
  
  selectedCountyMap <- reactive({ 
    ggplot(medianHousholdIncome_county) +
      geom_sf(fill = MHIhighlight(), color = "grey", linewidth = 0.3) +
      theme_void()
  })  
  
  #### Poverty Rate
  
  povertyRate_county <- arrange(povertyRate_county, estimate)
  POVnames <- factor(gsub(' County, Ohio', '', povertyRate_county$NAME))
  POV <- povertyRate_county$estimate
  POVmoe <- povertyRate_county$moe
  
  POVhighlight <- reactive({
    ifelse(POVnames == input$countySelect, "#CC0066", "#006699")
  })
  
  povertyRate_county_filtered <- reactive({
    filter(povertyRate_county,
           NAME == paste(input$countySelect, "County, Ohio"))
  })  
  
  output$povRate_county <- renderText({
    paste("2024 Poverty Rate:", "<h1 style = color:#006699>", format(povertyRate_county_filtered()$estimate, big.mark= ","), "% </h1>")
  }) 
  
  
  #### Note Block
  
  output$notes <- renderText({
    paste("<b>Do not rely on this dashboard for any purpose.</b> <small> This is very much a learning activity and a work in progress as I learn RShiny and may contain errors.", "<br>", "Data was obtained from the US Census Bureau API using the tidycensus R package and has been preloaded to minimize startup time.", "<br><br>", "I've borrowed heavily from:", "<br>", "Ted Laderas's sample code from his R for the Rest of Us Shiny Course at https://rfortherestofus.com and", "<br>", "Kyle Walker's samples from his website and tidycensus webinar at U-M SSDAN: https://walker-data.com </small> ")
  })
  
  waiter_hide()  ## Removes loading screen once the startup process are done
  
}


# Run the application 
shinyApp(ui = ui, server = server)
