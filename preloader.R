stateName <- "OH"

countyNamesList <- c("Adams", "Allen", "Ashland", "Ashtabula", "Athens", "Auglaize", "Belmont", "Brown", "Butler", "Carroll", "Champaign", "Clark", "Clermont", "Clinton", "Columbiana", "Coshocton", "Crawford", "Cuyahoga", "Darke", "Defiance", "Delaware", "Erie", "Fairfield", "Fayette", "Franklin", "Fulton", "Gallia", "Geauga", "Greene", "Guernsey", "Hamilton", "Hancock", "Hardin", "Harrison", "Henry", "Highland", "Hocking", "Holmes", "Huron", "Jackson", "Jefferson", "Knox", "Lake", "Lawrence", "Licking", "Logan", "Lorain", "Lucas", "Madison", "Mahoning", "Marion", "Medina", "Meigs", "Mercer", "Miami", "Monroe", "Montgomery", "Morgan", "Morrow", "Muskingum", "Noble", "Ottawa", "Paulding", "Perry", "Pickaway", "Pike", "Portage", "Preble", "Putnam", "Richland", "Ross", "Sandusky", "Scioto", "Seneca", "Shelby", "Stark", "Summit", "Trumbull", "Tuscarawas", "Union", "Van Wert", "Vinton", "Warren", "Washington", "Wayne", "Williams", "Wood", "Wyandot ")

##Download Data from Census Bureau, can either be called from within dashboard or preloaded prior to launching Shiny App

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