
config <- yaml::read_yaml("config/settings.yml")

connect_to_db <- function() {
  dbConnect(RMariaDB::MariaDB(), user = use, password = passwor, dbname = db, host = hos)
}

disconnect_from_db <- function(connection) {
  dbDisconnect(connection)
}

fetch_from_db <- function(connection, query) {
  res <- dbSendQuery(connection, query)
  data <- dbFetch(res, n = -1)
  dbClearResult(res)
  return(data)
}

read_data <- function(year = 0) {
  if(year != 0) {year_for_comparison <- year}



  
  # import countries from databse
  kanali <- connect_to_db()
  qurry_countries <- "SELECT name, abbr2L, eu_abbr2L from countries where EU =1"
  res <- dbSendQuery(kanali, qurry_countries) # send query to database
  countries <- dbFetch(res, n = -1) # fetch all data from querry
  dbClearResult(res) # clear result
  country_names <- countries[, 1]
  country_eu_abbr2L <- countries[, 3]
  dbDisconnect(kanali)
  df_1 <- data.frame(list_eur_countries)
  zero_vector <- rep(1, length(list_eur_countries))
  for (i in 1:10) {
    df_1 <- cbind(df_1, data.frame(zero_vector))
  }
  colnames(df_1) <- c("GEO","Free", "Verified_emissions","Total_energy_supply","GDPpc", "Population", "Inflation", "Agriculture", "Industry", "Manufacturing","Energy_Intensity")
  
  
  if (will_use_energy_intensity){
    # Load Verified data from csv file
    # Path: Data
    # File: nrg_ind_ei_linear.csv
    # Source: https://ec.europa.eu/eurostat/databrowser/view/NRG_IND_EI__custom_5726612/default/table?lang=en
    # Field in question on nrg_bal is "EI_GDP_PPS"
    # Country: All countries
    # Year: 1996 - 2021
    # Unit: KGOE_TEUR_PPS

    
    df_ei <- read.csv(file = "./data/csv_data/nrg_ind_ei_linear.csv",
                        header = TRUE,
                        as.is = TRUE)
    df_ei <- df_ei[which(df_ei$nrg_bal=="EI_GDP_PPS"),]
    df_ei <- df_ei[-c(1,2,3,4,5,9)]
    
    
    for (i in 1:nrow(df_1)) {
      if(year_for_comparison == 2020 && df_1$GEO[i] == "United Kingdom"){
        df_1$Energy_Intensity[i] <- 82.1
        next
      }
      temp <- df_ei$OBS_VALUE[which(df_ei$geo==countries$eu_abbr2L[which(countries$name==df_1$GEO[i])]  & df_ei$TIME_PERIOD==year_for_comparison)]
      if (length(temp) == 0){
        df_1$Energy_Intensity[i] <- 0
      }
      else{
        df_1$Energy_Intensity[i] <- as.numeric(temp)
      }
    }
  }
  if (will_use_verified_emisions){
    # Load Verified data from csv file
    # Path: Data
    # File: Historical emissions_data.csv
    # Source: https://www.eea.europa.eu/data-and-maps/dashboards/emissions-trading-viewer-1
    # Country: All countries
    # Year: 1990 - 2021
    # Unit: K tons of Co2 equivalent
    
    df_emis <- read.csv(file = "./data/csv_data/Historical emissions_data_2.csv",
                        header = TRUE,
                        as.is = TRUE)
    names(df_emis) <- c("Main.Activity", "Country","Year","ETS.information","Emissions.Unit","ETS.Information")
    # Replace "United Kingdom (excl. NI)" with "United Kingdom" in df_emis$Country
    df_emis <- df_emis %>% mutate(Country = ifelse(Country == "United Kingdom (excl. NI)", "United Kingdom", Country))
    # Replace idiotic values
    # Remove spaces in df_emis$Information
    df_emis$ETS.Information <- gsub(" ", "", df_emis$ETS.Information)
    for (i in 1:nrow(df_1)) {
        aviation <- df_emis$ETS.Information[which(df_emis$Country==df_1$GEO[i] & df_emis$Year==year_for_comparison & df_emis$ETS.information == "2. Verified emissions" & df_emis$Main.Activity == "10 Aviation")]
        all_stable_installations <- df_emis$ETS.Information[which(df_emis$Country==df_1$GEO[i] & df_emis$Year==year_for_comparison & df_emis$ETS.information == "2. Verified emissions" & df_emis$Main.Activity == "20-99 All stationary installations")]
        if (length(aviation) == 0){
          aviation <- 0
        }
        if (length(all_stable_installations) == 0){
          all_stable_installations <- 0
        }
          df_1$Verified_emissions[i] <- as.numeric(aviation) + as.numeric(all_stable_installations)
          #df_1$Verified_emissions[i] <- as.numeric(all_stable_installations)
          #print("EXEIS VGALEI TO AVIATION")
    }
  }
  if (will_use_free){
    # Load Verified data from csv file
    # Path: Data
    # File: Historical emissions_data.csv
    # Source: https://www.eea.europa.eu/data-and-maps/dashboards/emissions-trading-viewer-1
    # Country: All countries
    # Year: 1990 - 2021
    # Unit: K tons of Co2 equivalent
    
    df_free <- read.csv(file = "./data/csv_data/Historical emissions_data.csv",
                        header = TRUE,
                        as.is = TRUE)
    names(df_free) <- c("Main.Activity", "Country","Year","ETS.information","Emissions.Unit","ETS.Information")
    # Replace "United Kingdom (excl. NI)" with "United Kingdom" in df_emis$Country
    df_free <- df_emis %>% mutate(Country = ifelse(Country == "United Kingdom (excl. NI)", "United Kingdom", Country))
    # Replace idiotic values
    # Remove spaces in df_emis$Information
    df_free$ETS.Information <- gsub(" ", "", df_emis$ETS.Information)
    
    for (i in 1:nrow(df_1)) {
      aviation <- df_free$ETS.Information[which(df_free$Country==df_1$GEO[i] & df_free$Year==year_for_comparison & df_free$ETS.information == "1.1 Freely allocated allowances" & df_free$Main.Activity == "10 Aviation")]
      all_stable_installations <- df_free$ETS.Information[which(df_emis$Country==df_1$GEO[i] & df_free$Year==year_for_comparison & df_free$ETS.information == "1.1 Freely allocated allowances" & df_free$Main.Activity == "20-99 All stationary installations")]
      if (length(aviation) == 0){
        aviation <- 0
      }
      if (length(all_stable_installations) == 0){
        all_stable_installations <- 0
      }
      df_1$Free[i] <- as.numeric(aviation) + as.numeric(all_stable_installations)
      #df_1$Free[i] <- as.numeric(all_stable_installations)
      #print("EXEIS VGALEI TO AVIATION")
    }
  }
  
  
  if (will_use_GDPpc) {
    # Load Data from csv file
    # Path: Data
    # File: GDP_per_capita_1960_2021.csv
    # Source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
    # Data: GDP per capita (current US$)
    # Country: All countries
    # Year: 1960 - 2021
    # Unit: US$
    
    headers <- read.csv(file = "./data/csv_data/GDP_per_capita_1960_2021.csv",
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_GDPpc <- read.csv(file = "./data/csv_data/GDP_per_capita_1960_2021.csv",
                         skip = 5, header = FALSE)
    colnames(df_GDPpc) <- headers
    # Autos o ilithios tropos diavasmatos ginetai gia na apofigoume na peiraxthoun ta headers
    
    df_GDPpc <- subset(df_GDPpc, select = -c(2, 3, 4))
    df_GDPpc <- t(df_GDPpc)
    colnames(df_GDPpc) <- df_GDPpc[1, ]
    df_GDPpc <- df_GDPpc[-1, ]
    # To select precific country and year use the following syntax
    # df_GDPpc[year-1959, which(colnames(df_GDPpc) == "country_name")]
    for (i in 1:nrow(df_1)) {
      df_1[i, "GDPpc"] <- as.numeric(df_GDPpc[year_for_comparison-1959, which(colnames(df_GDPpc) == df_1[i, "GEO"])])
    }
  }
  
  if (will_use_inflation) {
    # Load Inflation data from csv file
    # Path: Data
    # File: Inflation_1960_2021.csv
    # Source: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
    # Data: Inflation, consumer prices (annual %)
    # Country: All countries
    # Year: 1960 - 2021
    # Unit: %
    
    # Headers are on the 4rth row
    headers <- read.csv(file = "./data/csv_data/Inflation_1960_2021.csv",
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_inflation <- read.csv(file = "./data/csv_data/Inflation_1960_2021.csv",
                             skip = 4, header = FALSE)
    colnames(df_inflation) <- headers
    
    # Create new dataframe, where colnames are the country names
    # and the rows are the years
    df_inflation <- subset(df_inflation, select = -c(2, 3, 4))
    df_inflation <- t(df_inflation)
    colnames(df_inflation) <- df_inflation[1, ]
    df_inflation <- df_inflation[-1, ]
    for (i in 1:nrow(df_1)) {
      
      df_1$"Inflation"[i] <- as.numeric(df_inflation[year_for_comparison-1959, which(colnames(df_inflation) == df_1$"GEO"[i])])
    }
  }
  
  
  if (will_use_population){
    # Load Population data from csv file
    # Path: Data
    # File: API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv
    # Source: https://data.worldbank.org/indicator/SP.POP.TOTL
    # Country: All countries
    # Year: 1960 - 2021
    # Unit: Persons
    
    # Headers are on the 4rth row
    headers <- read.csv(file = "./data/csv_data/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv",
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_population <- read.csv(file = "./data/csv_data/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv",
                              skip = 4, header = FALSE)
    colnames(df_population) <- headers
    
    # Create new dataframe, where colnames are the country names
    # and the rows are the years
    df_population <- subset(df_population, select = -c(2, 3, 4))
    df_population <- t(df_population)
    colnames(df_population) <- df_population[1, ]
    df_population <- df_population[-1, ]
    for (i in 1:nrow(df_1)) {
      
      df_1$Population[i] <- as.numeric(df_population[year_for_comparison-1959, which(colnames(df_population) == df_1$"GEO"[i])])
    }
  }
  
  

  if (will_use_total_energy_supply) {
    # Load Energy Balance data from csv file
    # Path: Data
    # File: nrg_bal_s__custom_4143365_linear.csv
    # Source: Eurostat
    # Data tree :  All data -> Environment and energy -> Energy -> Energy statistics -> quantities Energy statistics -> quantities, annual data -> Energy balances
    # Data name on Eurostat : Simplified energy balances 
    # Data: Energy balance
    # Country: All countries
    # Year: 1990 - 2020
    # Unit: Thousand tonnes of oil equivalent
    # nrg_bal codes:
    # Primary production     		      -> PPRD
    # Imports                	      	-> IMP
    # Exports                         -> EXP
    # Gross Available Energy          -> GAE
    # Total energy supply             -> NRGSUP
    # Available for final consumption -> AFC
    
    
    # Για το 2019: 66.440000 people * 105,1 GJoule/ capita * 0.0239 (GJ to ktoe) -> 166731180 ktoe
    # Στη βάση λέει 170540 gtoe δηλαδή 2.2% απόκλιση
    # Άρα για το 2020: 67.026.292 people * 95,9,9 GJoule/ capita * 0.0239 -> 153624931.52 ktoe -> 153,624.931 gtoe. Άρα το περνώ αυτό καρφωτά στην στον κώδικά.
    # https://www.iea.org/data-and-statistics?country=UNITED%20KINGDOM&fuel=Energy%20supply&indicator=Totals
    
    d <- read.csv(file = "./data/csv_data/nrg_bal_s__custom_4143365_linear.csv",
                  header = TRUE)
    d <- d[-c(1, 2, 3, 5, 6, 10)]
    df_total_energy_supply <- subset(d, d$nrg_bal == "NRGSUP")[-c(1)]
    
    for (i in 1:nrow(df_1)) {
      #check for UK in 2020
      if (year_for_comparison == 2020 && df_1$GEO[i]=="United Kingdom"){
        df_1$Total_energy_supply[i] <- 153624.931 # https://www.iea.org/countries/united-kingdom
        next
      }
      df_1$"Total_energy_supply"[i] <- df_total_energy_supply$OBS_VALUE[which(df_total_energy_supply$TIME_PERIOD == year_for_comparison & df_total_energy_supply$geo == countries$eu_abbr2L[which(countries$name == df_1$"GEO"[i])])]
    }
    df_1$Total_energy_supply <- as.numeric(gsub(",", "", df_1$Total_energy_supply))
    
  }
  
  if (will_use_agriculture | will_use_industry | will_use_manufacturing){
    # import data from World Bank
    # Path: Data
    # File: 4.2_Structure_of_value_added.csv
    # Source: http://wdi.worldbank.org/table/4.2
    # https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.CD,NV.AGR.TOTL.ZS,NV.IND.TOTL.ZS,NV.IND.MANF.ZS,NV.SRV.TETC.ZS,NV.SRV.TOTL.ZS#
    # Data: GDP, Argicalture, Industry, Manufacturing, Services
    # Country: All countries
    # Year: 1990 - 2020
    # Unit: Billions USD and percentage
    # Opened teh excel file on microsoft excel and coverted it into csv file AND CALCULATED BY HAND BULAGRIA MANUFACTURING 2020
    # Had to select the whole dataset 
    my_data <- read.csv(file = "./data/csv_data/2dbe830a-5afc-4ed9-b478-f5349450364b_Data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
    
    buffer <- my_data[my_data$"Country.Name" %in% df_1$"GEO",]
    buffer_GDP <- buffer[buffer$"Series.Name" == "GDP (current US$)",]
    buffer_Agriculture <- buffer[buffer$"Series.Name" == "Agriculture, forestry, and fishing, value added (% of GDP)",]
    buffer_Industry <- buffer[buffer$"Series.Name" == "Industry (including construction), value added (% of GDP)",]
    buffer_Manufacturing <- buffer[buffer$"Series.Name" == "Manufacturing, value added (% of GDP)",]
    buffer_Services <- buffer[buffer$"Series.Name" == "Services, value added (% of GDP)",]
  }
  
  if (will_use_agriculture){
    if (Manufacturing_Industry_Agriculture_as_percentage){
      for (i in 1:nrow(df_1)){
        df_1$Agriculture[i] <- as.numeric(buffer_Agriculture[buffer_Agriculture$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
      }
    }
    else {
      for (i in 1:nrow(df_1)){
        GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
        df_1$Agriculture[i] <- as.numeric(buffer_Agriculture[buffer_Agriculture$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100
      }
    }
  }
  if (will_use_industry){
    if(Manufacturing_Industry_Agriculture_as_percentage){
      for (i in 1:nrow(df_1)){
        df_1$Industry[i] <- as.numeric(buffer_Industry[buffer_Industry$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
      } 
    }
    else{
      for (i in 1:nrow(df_1)){
        GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
        df_1$Industry[i] <- as.numeric(buffer_Industry[buffer_Industry$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100
      }        
    }
  }
  if (will_use_manufacturing){
    if(Manufacturing_Industry_Agriculture_as_percentage){
      for (i in 1:nrow(df_1)){
        if (df_1$GEO[i] == "Bulgaria"){
          # Estimating Bulgaria's manufacturing value added from the rest of the GDP components
          df_1$Manufacturing[i] <- 100 - (as.numeric(buffer_Agriculture[buffer_Agriculture$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])) - (as.numeric(buffer_Industry[buffer_Industry$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])) - (as.numeric(buffer_Services[buffer_Services$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]))
          next
        }
        df_1$Manufacturing[i] <- as.numeric(buffer_Manufacturing[buffer_Manufacturing$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
      }
    }
    else{
      for (i in 1:nrow(df_1)){
        if (df_1$GEO[i] == "Bulgaria"){
          # Estimating Bulgaria's manufacturing value added from the rest of the GDP components
          GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
          df_1$Manufacturing[i] <- GDP_multiplier / 100 * (100 - as.numeric(buffer_Agriculture[buffer_Agriculture$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) 
                                                           - as.numeric(buffer_Industry[buffer_Industry$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
                                                           - as.numeric(buffer_Services[buffer_Services$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]))
          next
        }
        GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
        df_1$Manufacturing[i] <- as.numeric(buffer_Manufacturing[buffer_Manufacturing$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100
      }
    }
  }
  
  if (use_mean_for_missing_data){
    for (i in 1:ncol(df_1)){
      
      for(j in 1:nrow(df_1)){
        if (is.na(df_1[j,i])){
          information_text <- append(information_text, paste("Missing data in ", colnames(df_1)[i], " for ", df_1$GEO[j], " in year ", year_for_comparison, ". Replaced with mean value.", sep = "" ))
        }
      }
      if (sum(is.na(df_1[,i])) > 0){
        df_1[,i][is.na(df_1[,i])] <- mean(df_1[,i], na.rm = TRUE)
      }
    }
  }
  
  if (will_use_log){
    df_1$Total_energy_supply <- log(abs(df_1$Total_energy_supply))
    
    df_1$Inflation <- log(df_1$Inflation/100 + 1)*100
    df_1$GDPpc <- log(abs(df_1$GDPpc))
    df_1$Population <- log(abs(df_1$Population))
    df_1$Verified_emissions <- log(abs(df_1$Verified_emissions))
    df_1$Agriculture <- log(abs(df_1$Agriculture))
    df_1$Industry <- log(abs(df_1$Industry))
    df_1$Manufacturing <- log(abs(df_1$Manufacturing))
  }
  if (will_normalise){ 
    max_total_energy_supply <- max(df_1$Total_energy_supply)
    max_inflation <- max(abs(df_1$Inflation))
    max_GDPpc <- max(abs(df_1$GDPpc))
    max_population <- max(abs(df_1$Population))
    max_verified_emissions <- max(abs(df_1$Verified_emissions))
    max_Agriculture <- max(abs(df_1$Agriculture))
    max_Industry <- max(abs(df_1$Industry))
    max_Manufacturing <- max(abs(df_1$Manufacturing))
    
    # devide by max value of each column to normalise data unless max value is 0
    if(max_total_energy_supply != 0) df_1$Total_energy_supply <- df_1$Total_energy_supply / max_total_energy_supply
    if(max_inflation != 0) df_1$Inflation <- df_1$Inflation / max_inflation
    if(max_GDPpc != 0) df_1$GDPpc <- df_1$GDPpc / max_GDPpc
    if(max_population != 0) df_1$Population <- df_1$Population / max_population
    if(max_verified_emissions != 0) df_1$Verified_emissions <- df_1$Verified_emissions / max_verified_emissions
    if(max_Agriculture != 0) df_1$Agriculture <- df_1$Agriculture / max_Agriculture
    if(max_Industry != 0) df_1$Industry <- df_1$Industry / max_Industry
    if(max_Manufacturing != 0) df_1$Manufacturing <- df_1$Manufacturing / max_Manufacturing
  }
  # print(information_text)
  

  return(df_1)
}


get_free_allocation <- function(connection, country, year) {
  query <- sprintf(
    "SELECT SUM(freeAlloc) FROM `eutl_compliance` WHERE country = '%s' AND etos ='%s'",
    country, year
  )
  data <- fetch_from_db(connection, query)
  return(data[1, 1])
}

read_free <- function(year = 0) {
  if (year != 0) {year_for_comparison <- year}
  
  text_s <- generate_filename(year_for_comparison)
  
  if (!force_fresh_data & !is.null(cached_free[[text_s]])) {
    return(cached_free[[text_s]])
  }

  kanali <- connect_to_db()

  # Fetching countries
  countries <- fetch_from_db(kanali, "SELECT name, eu_abbr2L FROM countries WHERE EU = 1")
  
  # Fetching free allocations
  free_data <- sapply(list_eur_countries, function(country) {
    abbr <- countries$eu_abbr2L[which(countries$name == country)]
    get_free_allocation(kanali, abbr, year_for_comparison)
  })
  
  df_free <- data.frame("GEO" = list_eur_countries, "Free" = free_data)
  
  disconnect_from_db(kanali)
  
  df_free$Free <- as.numeric(df_free$Free)
  if (will_use_log) df_free$Free <- log(abs(df_free$Free))
  if (will_normalise) df_free$Free <- df_free$Free / max(df_free$Free)

  
  return(df_free)
}
