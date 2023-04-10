library("hash")
library("xlsx")
library("xtable") # Load xtable package
library("stringr")
library("DBI")
library("RMySQL")
library("ggplot2")
library("factoextra")
library("NbClust")
library("dplyr")
  # INITIAL DECLARATIONS

will_use_log <- FALSE
year_for_comparison <- 2017
will_use_total_energy_supply <- TRUE
will_use_inflation <- TRUE
will_use_GDPpc <- TRUE 
will_use_population <- TRUE
will_use_agriculture <- TRUE
will_use_verified_emisions <- TRUE
will_use_  <- TRUE
will_use_industry <- TRUE
will_use_manufacturing <- TRUE
will_normalise <- FALSE
force_fresh_data <- TRUE
use_mean_for_missing_data <- TRUE
will_use_free <- TRUE
Manufacturing_Industry_Agriculture_as_percentage <- TRUE
cached_data <- hash()
cached_free <- hash()


  # DEFINITIONS FOR DATABASE
db <- "eu_ets"           # name of database
use <- "root"           # user name
passwor <- ""     # password
hos <- "localhost"       # host name

  # LIST OF COUNTRIES WITH DATA
  
list_eur_countries <- c("Austria","Belgium", "Bulgaria","Cyprus",        
 "Denmark","Estonia", "Finland","France",   
 "Germany","Greece", "Hungary","Ireland",
 "Italy", "Latvia", "Lithuania","Luxembourg",   
 "Malta", "Netherlands", "Poland", "Portugal",
 "Romania", "Slovenia", "Spain", "Sweden",     
 "United Kingdom")
  
  #Clustering on features as derived from the 2017 features.

clusters <- list(c("France","Germany","Italy","Poland","Spain","United Kingdom"), 
              c("Bulgaria","Estonia","Hungary","Latvia","Lithuania","Romania"), 
              c("Austria","Belgium","Cyprus","Denmark","Finland","Greece","Ireland",
                "Luxembourg","Malta","Netherlands","Portugal","Slovenia","Sweden" ))

read_data_2 <- function(year = 0) {
  if(year != 0) {year_for_comparison <- year}

  # Try to find the file in the folder "Data/created_csvs"
  # If it is not found, create it
  will_use <- c(will_use_total_energy_supply, will_use_GDPpc, will_use_population, will_use_inflation,will_use_verified_emisions, will_use_agriculture, will_use_industry, will_use_manufacturing)
  text_s <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
                  (if(will_normalise) "_norm" else ""),
                  (if(will_use_total_energy_supply) "_tes" else ""),
                  (if(will_use_verified_emisions) "_ve" else ""),
                  (if(will_use_GDPpc) "_gdp" else ""),
                  (if(will_use_population) "_pop" else ""),
                  (if(will_use_inflation) "_inf" else ""),
                  (if(will_use_agriculture) "_agr" else ""),
                  (if(will_use_industry) "_ind" else ""),
                  (if(will_use_manufacturing) "_man" else ""),
                  ".csv", sep = "")
  if(!force_fresh_data & !is.null(cached_data[[text_s]])){
    #print (paste("Using Cached Data from hash for: ", year_for_comparison))
    return(cached_data[[text_s]])
  }
  if (!force_fresh_data & file.exists(paste("./Data/created_csvs/",text_s, sep = ""))) {
    #print ("Using Cached Data from csv")
    cached_data[[text_s]] <- data.frame(read.csv(file = paste("./Data/created_csvs/",text_s, sep = ""), header = TRUE)[-c(1)])
    return(data.frame(read.csv(file = paste("./Data/created_csvs/",text_s, sep = ""),
                               header = TRUE)[-c(1)]))
  }
  
  # import countries from databse
  kanali <- dbConnect(RMariaDB::MariaDB(),
                      user = use,
                      password = passwor,
                      dbname = db,
                      host = hos)
  qurry_countries <- "SELECT name, abbr2L, eu_abbr2L from countries where EU =1"
  res <- dbSendQuery(kanali, qurry_countries) # send query to database
  countries <- dbFetch(res, n = -1) # fetch all data from querry
  dbClearResult(res) # clear result
  country_names <- countries[, 1]
  country_eu_abbr2L <- countries[, 3]
  
  df_1 <- data.frame(list_eur_countries)
  zero_vector <- rep(1, length(list_eur_countries))
  for (i in 1:9) {
    df_1 <- cbind(df_1, data.frame(zero_vector))
  }
  colnames(df_1) <- c("GEO","Total_energy_supply","GDPpc", "Population", "Inflation", "Verified_emissions", "Agriculture", "Industry", "Manufacturing","Free")
  
  if (will_use_verified_emisions){
    # Load Verified data from csv file
    # Path: Data
    # File: Historical emissions_data.csv
    # Source: https://www.eea.europa.eu/data-and-maps/dashboards/emissions-trading-viewer-1
    # Country: All countries
    # Year: 1990 - 2021
    # Unit: K tons of Co2 equivalent
    
    df_emis <- read.csv(file = "./Data/Historical emissions_data.csv",
                        header = TRUE,
                        as.is = TRUE)
    # Replace "United Kingdom (excl. NI)" with "United Kingdom" in df_emis$Country
    df_emis <- df_emis %>% mutate(Country = ifelse(Country == "United Kingdom (excl. NI)", "United Kingdom", Country))
    # Replace idiotic values
    # Remove spaces in df_emis$Information
    df_emis$ETS.Information <- gsub(" ", "", df_emis$ETS.Information)
    for (i in 1:nrow(df_1)) {
        temp <- df_emis$ETS.Information[which(df_emis$Country==df_1$GEO[i] & df_emis$Year==year_for_comparison & df_emis$ETS.information == "2. Verified emissions")]
        if (length(temp) == 0){
          df_1$Free[i] <- 0
        }
        else{
          df_1$Free[i] <- as.numeric(temp)
        }
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
    
    df_free <- read.csv(file = "./Data/Historical emissions_data.csv",
                        header = TRUE,
                        as.is = TRUE)
    # Replace "United Kingdom (excl. NI)" with "United Kingdom" in df_emis$Country
    df_free <- df_emis %>% mutate(Country = ifelse(Country == "United Kingdom (excl. NI)", "United Kingdom", Country))
    # Replace idiotic values
    # Remove spaces in df_emis$Information
    df_free$ETS.Information <- gsub(" ", "", df_emis$ETS.Information)
    for (i in 1:nrow(df_1)) {
      temp <- df_free$ETS.Information[which(df_free$Country==df_1$GEO[i] & df_free$Year==year_for_comparison & df_free$ETS.information == "1.1 Freely allocated allowances")]
      if (length(temp) == 0){
        df_1$Free[i] <- 0
      }
      else{
        df_1$Free[i] <- as.numeric(temp)
      }
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
    
    headers <- read.csv(file = "./Data/GDP_per_capita_1960_2021.csv",
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_GDPpc <- read.csv(file = "./Data/GDP_per_capita_1960_2021.csv",
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
    headers <- read.csv(file = "./Data/Inflation_1960_2021.csv",
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_inflation <- read.csv(file = "./Data/Inflation_1960_2021.csv",
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
    headers <- read.csv(file = "./Data/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv",
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_population <- read.csv(file = "./Data/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv",
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
    
    d <- read.csv(file = "./Data/nrg_bal_s__custom_4143365_linear.csv",
                  header = TRUE)
    d <- d[-c(1, 2, 3, 5, 6, 10)]
    df_total_energy_supply <- subset(d, d$nrg_bal == "NRGSUP")[-c(1)]
    
    for (i in 1:nrow(df_1)) {
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
    my_data <- read.csv(file = "./Data/2dbe830a-5afc-4ed9-b478-f5349450364b_Data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
    
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
  
  # save csv file of df_1
  text_t <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
                  (if(will_normalise) "_norm" else ""),
                  (if(will_use_total_energy_supply) "_tes" else ""),
                  (if(will_use_verified_emisions) "_ve" else ""),
                  (if(will_use_GDPpc) "_gdp" else ""),
                  (if(will_use_population) "_pop" else ""),
                  (if(will_use_inflation) "_inf" else ""),
                  (if(will_use_agriculture) "_agr" else ""),
                  (if(will_use_industry) "_ind" else ""),
                  (if(will_use_manufacturing) "_man" else ""),
                  ".csv", sep = "")
  write.csv(df_1, file = paste("./Data/created_csvs/",text_t, sep = "" ), row.names = TRUE)
  cached_data[[text_t]] <- df_1
  return(df_1)
}


read_data <- function(year = 0) {
  if(year != 0) {year_for_comparison <- year}
    information_text <- list() # To kef errors and missing data

    # Try to find the file in the folder "Data/created_csvs"
    # If it is not found, create it
    will_use <- c(will_use_total_energy_supply, will_use_GDPpc, will_use_population, will_use_inflation,will_use_verified_emisions, will_use_agriculture, will_use_industry, will_use_manufacturing)
    text_s <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
                    (if(will_normalise) "_norm" else ""),
                    (if(will_use_total_energy_supply) "_tes" else ""),
                    (if(will_use_verified_emisions) "_ve" else ""),
                    (if(will_use_GDPpc) "_gdp" else ""),
                    (if(will_use_population) "_pop" else ""),
                    (if(will_use_inflation) "_inf" else ""),
                    (if(will_use_agriculture) "_agr" else ""),
                    (if(will_use_industry) "_ind" else ""),
                    (if(will_use_manufacturing) "_man" else ""),
                    ".csv", sep = "")
    if(!force_fresh_data & !is.null(cached_data[[text_s]])){
      #print (paste("Using Cached Data from hash for: ", year_for_comparison))
      return(cached_data[[text_s]])
  }
    if (!force_fresh_data & file.exists(paste("./Data/created_csvs/",text_s, sep = ""))) {
        #print ("Using Cached Data from csv")
        cached_data[[text_s]] <- data.frame(read.csv(file = paste("./Data/created_csvs/",text_s, sep = ""), header = TRUE)[-c(1)])
        return(data.frame(read.csv(file = paste("./Data/created_csvs/",text_s, sep = ""),
                        header = TRUE)[-c(1)]))
    }
    # import countries from databse
    kanali <- dbConnect(RMariaDB::MariaDB(),
                        user = use,
                        password = passwor,
                        dbname = db,
                        host = hos)
    qurry_countries <- "SELECT name, abbr2L, eu_abbr2L from countries where EU =1"
    res <- dbSendQuery(kanali, qurry_countries) # send query to database
    countries <- dbFetch(res, n = -1) # fetch all data from querry
    dbClearResult(res) # clear result
    country_names <- countries[, 1]
    country_eu_abbr2L <- countries[, 3]

    df_1 <- data.frame(list_eur_countries)
    zero_vector <- rep(1, length(list_eur_countries))
    for (i in 1:8) {
        df_1 <- cbind(df_1, data.frame(zero_vector))
    }
    colnames(df_1) <- c("GEO","Total_energy_supply","GDPpc", "Population", "Inflation", "Verified_emissions", "Agriculture", "Industry", "Manufacturing")

    if (will_use_GDPpc) {
        # Load Data from csv file
        # Path: Data
        # File: GDP_per_capita_1960_2021.csv
        # Source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
        # Data: GDP per capita (current US$)
        # Country: All countries
        # Year: 1960 - 2021
        # Unit: US$

        headers <- read.csv(file = "./Data/GDP_per_capita_1960_2021.csv",
                            skip = 4,
                            header = FALSE,
                            nrows = 1,
                            as.is = TRUE)
        df_GDPpc <- read.csv(file = "./Data/GDP_per_capita_1960_2021.csv",
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
        headers <- read.csv(file = "./Data/Inflation_1960_2021.csv",
                            skip = 4,
                            header = FALSE,
                            nrows = 1,
                            as.is = TRUE)
        df_inflation <- read.csv(file = "./Data/Inflation_1960_2021.csv",
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
        headers <- read.csv(file = "./Data/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv",
                            skip = 4,
                            header = FALSE,
                            nrows = 1,
                            as.is = TRUE)
        df_population <- read.csv(file = "./Data/API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv",
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

    if (will_use_verified_emisions){
      if (year_for_comparison < 1990){
        for (i in 1:nrow(df_1)){
          df_1$Verified_emissions[i] <- 1
        }
        print(paste("NO CO2 emissions were found for year", year_for_comparison,"."))
      }
      else if(year_for_comparison < 2004){
        # Load Population data from csv file
        # Path: Data
        # File: API_EN.ATM.GHGT.KT.CE_DS2_en_csv_v2_4770432.csv
        # Source: https://data.worldbank.org/indicator/EN.ATM.GHGT.KT.CE
        # Country: All countries
        # Year: 1990 - 2021
        # Unit: K tons of Co2 equivalent
        # Headers are on the 5th row
        
        headers <- read.csv(file = "./Data/API_EN.ATM.GHGT.KT.CE_DS2_en_csv_v2_4770432.csv",
                            skip = 4,
                            header = TRUE,
                            nrows = 1,
                            as.is = TRUE)
        df_emis <- read.csv(file = "./Data/API_EN.ATM.GHGT.KT.CE_DS2_en_csv_v2_4770432.csv",
                                  skip = 4, header = FALSE)
        colnames(df_emis) <- headers
        
        # Create new dataframe, where colnames are the country names
        # and the rows are the years
        df_emis <- subset(df_emis, select = -c(2, 3, 4))
        df_emis <- t(df_emis)
        colnames(df_emis) <- df_emis[1, ]
        df_emis <- df_emis[-1, ]
        for (i in 1:nrow(df_1)) {
          
          df_1$Verified_emissions[i] <- as.numeric(df_emis[year_for_comparison-1959, which(colnames(df_emis) == df_1$"GEO"[i])])
        }
      } else {
      # Taking data drom the database as well


      df_verified_emisions <- data.frame(nrow = 50,ncol = 2)
      colnames(df_verified_emisions) <- c("GEO", "verified_emisions")
      for (i in 1:length(country_names)) { # nolint
          querr <- paste(
          "SELECT SUM(verified) FROM `eutl_compliance` WHERE country = '",
          country_eu_abbr2L[i], "' AND etos ='",year_for_comparison,"'", sep = "", collapse = NULL)
          res <- dbSendQuery(kanali, querr) # send query to database
          verified <- dbFetch(res, n = -1) # fetch all data from querry
          dbClearResult(res) # clear result
          df_verified_emisions <- rbind(df_verified_emisions, data.frame("GEO" = country_names[i], "verified_emisions" = verified[1,1]))
      }
      dbDisconnect(kanali)
      colnames(df_verified_emisions) <- c("GEO", "verified_emisions")


      # Merge the two dataframes
      for (i in 1:nrow(df_1)){
          df_1$Verified_emissions[i] <- as.integer(df_verified_emisions$verified_emisions[which(df_verified_emisions$GEO == df_1$GEO[i])])
      }
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
    
    d <- read.csv(file = "./Data/nrg_bal_s__custom_4143365_linear.csv",
                  header = TRUE)
    d <- d[-c(1, 2, 3, 5, 6, 10)]
    df_total_energy_supply <- subset(d, d$nrg_bal == "NRGSUP")[-c(1)]
    
    for (i in 1:nrow(df_1)) {
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
      my_data <- read.csv(file = "./Data/2dbe830a-5afc-4ed9-b478-f5349450364b_Data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

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
    
    # save csv file of df_1
    text_t <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
                        (if(will_normalise) "_norm" else ""),
                        (if(will_use_total_energy_supply) "_tes" else ""),
                        (if(will_use_verified_emisions) "_ve" else ""),
                        (if(will_use_GDPpc) "_gdp" else ""),
                        (if(will_use_population) "_pop" else ""),
                        (if(will_use_inflation) "_inf" else ""),
                        (if(will_use_agriculture) "_agr" else ""),
                        (if(will_use_industry) "_ind" else ""),
                        (if(will_use_manufacturing) "_man" else ""),
                        ".csv", sep = "")
    write.csv(df_1, file = paste("./Data/created_csvs/",text_t, sep = "" ), row.names = TRUE)
    cached_data[[text_t]] <- df_1
    return(df_1)
}

read_free <- function(year = 0){
  if(year != 0) {year_for_comparison <- year}
  text_s <- paste("df_free_",year_for_comparison,(if (will_use_log) "_log" else ""),
                  (if(will_normalise) "_norm" else ""),
                  (if(will_use_total_energy_supply) "_tes" else ""),
                  (if(will_use_verified_emisions) "_ve" else ""),
                  (if(will_use_GDPpc) "_gdp" else ""),
                  (if(will_use_population) "_pop" else ""),
                  (if(will_use_inflation) "_inf" else ""),
                  (if(will_use_agriculture) "_agr" else ""),
                  (if(will_use_industry) "_ind" else ""),
                  (if(will_use_manufacturing) "_man" else ""),
                  ".csv", sep = "")
  if(!force_fresh_data & !is.null(cached_free[[text_s]])){
    #print (paste("Using Cached Free Data from hash for: ", year_for_comparison))
    return(cached_free[[text_s]])
  }
    kanali <- dbConnect(RMariaDB::MariaDB(),
                      user = use,
                      password = passwor,
                      dbname = db,
                      host = hos)
  ##################################
  # include verified emissions data
  ##################################
  qurry_countries <- "SELECT name, abbr2L, eu_abbr2L from countries where EU =1"
  res <- dbSendQuery(kanali, qurry_countries) # send query to database
  countries <- dbFetch(res, n = -1) # fetch all data from querry
  dbClearResult(res) # clear result

  for (i in 1:length(list_eur_countries)) { # nolint
    querr <- paste(
      "SELECT SUM(freeAlloc) FROM `eutl_compliance` WHERE country = '",
      countries$eu_abbr2L[which (countries$name == list_eur_countries[i])], "' AND etos ='",year_for_comparison,"'", sep = "", collapse = NULL)
    res <- dbSendQuery(kanali, querr) # send query to database
    free <- dbFetch(res, n = -1) # fetch all data from querry
    dbClearResult(res) # clear result
    if (i == 1){
      df_free <- data.frame("GEO" = list_eur_countries[i], "Free" = free[1,1])
    } else {
      df_free <- rbind(df_free, data.frame("GEO" = list_eur_countries[i], "Free" = free[1,1]))
    }
  }

  df_free$Free <- as.numeric(df_free$Free)
  dbDisconnect(kanali)
  if (will_use_log){
    df_free$Free <- log(abs(df_free$Free))
  }
  if (will_normalise){
    df_free$Free <- df_free$Free/max(df_free$Free)
  }

  cached_free[[text_s]] <- df_free
  
  return (df_free)
}

find_slopes <- function(year = 0, weight_population = 1, weight_GDPpc = 1, weight_inflation = 1, weight_agriculture = 1, weight_industry = 1, weight_manufacturing = 1, weight_total_energy_supply = 1, weight_verified_emisions = 1){
  if(year != 0) {year_for_comparison <- year}
  

  #df_data <-read_data( TRUE, 2016,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  df_data <-read_data(year_for_comparison)
  #df_free <- read_free(df_data$GEO, year = 2016 ,TRUE, TRUE)
  df_free <- read_free(year_for_comparison)


  text_s <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
                (if(will_normalise) "_norm" else ""),
                (if(will_use_total_energy_supply) "_tes" else ""),
                (if(will_use_verified_emisions) "_ve" else ""),
                (if(will_use_GDPpc) "_gdp" else ""),
                (if(will_use_population) "_pop" else ""),
                (if(will_use_inflation) "_inf" else ""),
                (if(will_use_agriculture) "_agr" else ""),
                (if(will_use_industry) "_ind" else ""),
                (if(will_use_manufacturing) "_man" else ""),
                ".csv", sep = "")

  
  # Create a 2 D dataframe with countries as rows and countries as columns and the value of the Eukleidian Distance between all the values the two countries
  df_distance <- data.frame(matrix(NA, nrow = nrow(df_data), ncol = nrow(df_data)))
  rownames(df_distance) <- df_data$"GEO"
  colnames(df_distance) <- df_data$"GEO"
  for (i in 1 : nrow(df_distance)) { #nolint
    for (j in 1 : ncol(df_distance)) { # nolint 
      df_distance[i, j] <- sqrt((df_data[i, 2] - df_data[j, 2])^2 * weight_total_energy_supply 
      + (df_data[i, 3] - df_data[j, 3])^2 * weight_GDPpc
      + (df_data[i, 4] - df_data[j, 4])^2 * weight_population
      + (df_data[i, 5] - df_data[j, 5])^2 * weight_inflation
      + (df_data[i, 6] - df_data[j, 6])^2 * weight_verified_emisions
      + (df_data[i, 7] - df_data[j, 7])^2 * weight_agriculture
      + (df_data[i, 8] - df_data[j, 8])^2 * weight_industry
      + (df_data[i, 9] - df_data[j, 9])^2 * weight_manufacturing
      )
    }
  }
  
  # save a file "distance_2015.tex" with the content of the dataframe df_distance
  #<<results=tex>>
   #   xtable(df_distance[,21:26], caption = "Distance between countries in 2015", label = "tab:distance_2015")
  #@ 
  
  # Calculate Eukleidian distance of df_free_allocation and store them in df_actual_distance
  df_free_distance <- data.frame(matrix(NA, nrow = nrow(df_free), ncol = nrow(df_free)))
  rownames(df_free_distance) <- df_free$"GEO"
  colnames(df_free_distance) <- df_free$"GEO"
  for (i in 1 : nrow(df_free_distance)) { #nolint
    for (j in 1 : ncol(df_free_distance)) { # nolint 
      df_free_distance[i, j] <- sqrt((df_free[i, 2] - df_free[j, 2])^2 )
    }
  }
  
  
  
  # Create a 1D dataframe containing a tuple with 1 value from df_distance and 1 value from df_actual_distance
  df_1D <- data.frame(matrix(NA, nrow = nrow(df_distance) * (ncol(df_distance)-1)/2, ncol = 3))
  colnames(df_1D) <- c("df_distance", "df_free_distance", "pair")
  for (i in 1 : nrow(df_distance)) { #nolint
    for (j in i : ncol(df_distance)) { # nolint 
      df_1D[(i-1)*ncol(df_distance) + j, 1] <- df_distance[i, j]
      df_1D[(i-1)*ncol(df_distance) + j, 2] <- df_free_distance[i, j]
      df_1D[(i-1)*ncol(df_distance) + j, 3] <- paste(rownames(df_distance)[i], colnames(df_distance)[j], sep = " - ")
    }
  }
  
  
  # Remove rows with NA and 0
  df_1D <- df_1D[!is.na(df_1D$"df_distance"),]
  df_1D <- df_1D[!is.na(df_1D$"df_free_distance"),]
  df_1D <- df_1D[df_1D$"df_distance" != 0,]
  df_1D <- df_1D[df_1D$"df_free_distance" != 0,]
  
  lm <- lm(df_1D$"df_free_distance" ~ df_1D$"df_distance")
  summary(lm)
  
  #write that summary to a file
  sink(paste("linear_regration_summary_for", text_s, ".txt"))
  summary(lm)
  sink()
  # plot the regression line
  plot(df_1D$"df_distance", df_1D$"df_free_distance", xlab = "Distance between countries in 2015", ylab = "Distance between countries in 2015 with free allocation", main = paste("Distance between countries in 2015 and in 2015 with free allocation", year_for_comparison))
  abline(lm, col = "red")
  return (list( data = df_1D, linear =  lm))
}

#XRISIMOPOIEITAI ETSI:
# print(find_slopes(year = 2014))
# X2014 <- find_slopes(year = 2014)
# print(summary(X2014$linear)$r.squared)
# print(X2014$data)
#####################################
create_graph <- function (year = 0, name = "default", weights = c(1,1,1,1,1,1,1,1)){
    if(year != 0) {year_for_comparison <- year}
  df_1D <- find_slopes(year = year_for_comparison, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])$data
  lm <- find_slopes(year = year_for_comparison, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])$linear
  print(df_1D)
  # Create png with the regression line
  #png(paste("Newscatterplot_with_regression_line_",year_for_comparison,"_with_all_data_and_log=", will_use_log, ".png") , width = 1000, height = 1000)
  png(paste(name,".png",sep=""), width = 1000, height = 1000)
  #plot(df_1D$"df_distance", df_1D$"d f_free_distance", xlab = "Combined calculated distance", ylab = "Free distance", main = paste( "Scatterplot of calculated distance and actual distance for the year ", year_for_comparison, sep = ""))  # Color red the points of the scatterpolit where df_1D[3,] contains "Germany"
  plot(df_1D$"df_distance", df_1D$"df_free_distance", xlab = "Combined calculated distance", ylab = "Free distance")
  points(df_1D[ str_detect(df_1D$"pair", regex(".Germany")), 1], df_1D[str_detect(df_1D$"pair", regex(".Germany")), 2], col = "red")
  points(df_1D[ str_detect(df_1D$"pair", regex("Germany.")), 1], df_1D[str_detect(df_1D$"pair", regex("Germany.")), 2], col = "red")

  # Color blue the points of the scatterpolit where df_1D[3,] contains "Greece"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Greece")), 1], df_1D[str_detect(df_1D$"pair", regex(".Greece")), 2], col = "blue")
  points(df_1D[ str_detect(df_1D$"pair", regex("Greece.")), 1], df_1D[str_detect(df_1D$"pair", regex("Greece.")), 2], col = "blue")

  # Color green the points of the scatterpolit where df_1D[3,] contains "Italy"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Italy")), 1], df_1D[str_detect(df_1D$"pair", regex(".Italy")), 2], col = "green")
  points(df_1D[ str_detect(df_1D$"pair", regex("Italy.")), 1], df_1D[str_detect(df_1D$"pair", regex("Italy.")), 2], col = "green")

  # Color yellow the points of the scatterpolit where df_1D[3,] contains "Ukraine"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Ukraine")), 1], df_1D[str_detect(df_1D$"pair", regex(".Ukraine")), 2], col = "yellow")
  points(df_1D[ str_detect(df_1D$"pair", regex("Ukraine.")), 1], df_1D[str_detect(df_1D$"pair", regex("Ukraine.")), 2], col = "yellow")

  # Color Yellow the points of the scatterpolit where df_1D[3,] contains "France"
  points(df_1D[ str_detect(df_1D$"pair", regex(".France")), 1], df_1D[str_detect(df_1D$"pair", regex(".France")), 2], col = "yellow")
  points(df_1D[ str_detect(df_1D$"pair", regex("France.")), 1], df_1D[str_detect(df_1D$"pair", regex("France.")), 2], col = "yellow")

  # Color orange the points of the scatterpolit where df_1D[3,] contains "United Kingdom"
  points(df_1D[ str_detect(df_1D$"pair", regex(".United Kingdom")), 1], df_1D[str_detect(df_1D$"pair", regex(".United Kingdom")), 2], col = "orange")
  points(df_1D[ str_detect(df_1D$"pair", regex("United Kingdom.")), 1], df_1D[str_detect(df_1D$"pair", regex("United Kingdom.")), 2], col = "orange")

  # Color purple the points of the scatterpolit where df_1D[3,] contains "Luxembourg"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Luxembourg")), 1], df_1D[str_detect(df_1D$"pair", regex(".Luxembourg")), 2], col = "purple")
  points(df_1D[ str_detect(df_1D$"pair", regex("Luxembourg.")), 1], df_1D[str_detect(df_1D$"pair", regex("Luxembourg.")), 2], col = "purple")

  #Create a legend with the colors of the points
  legend("topright", legend = c("Germany", "Greece", "Italy", "France", "United Kingdom", "Luxembourg"), col = c("red", "blue", "green", "yellow", "orange", "purple"), pch = 20)

  abline(lm, col = "red")
  dev.off()
}

find_slopes_with_weights <- function(weights){
  return (find_slopes(weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8]))
}

find_slopes_with_one_country <- function(year = 0, weight_population = 1, weight_GDPpc = 1, weight_inflation = 1, weight_agriculture = 1, weight_industry = 1, weight_manufacturing = 1, weight_total_energy_supply = 1, weight_verified_emisions = 1, country = "none"){
  if(year != 0) {year_for_comparison <- year}
  

  #df_data <-read_data( TRUE, 2016,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  df_data <-read_data(year_for_comparison)
  #df_free <- read_free(df_data$GEO, year = 2016 ,TRUE, TRUE)
  df_free <- read_free(year_for_comparison)

  if(country == "none"){ #find the median country with respect to the df_data and the values in it
   ranking <- data.frame(country = df_data$GEO, value = 0)
   for (i in 2:ncol(df_data)){
    buffer <- data.frame(country = df_data$GEO, value = df_data[,i])
    # print (colnames(df_data)[i])
    buffer <- buffer[order(buffer$value),]
    # print(buffer)
    for (j in 1:nrow(buffer)){
      # print(buffer[j,1])
      ranking[which(buffer[j,1] == ranking$country ),2] <- ranking[which(buffer[j,1] == ranking$country ),2] + j
    }
   }
    ranking <- ranking[order(ranking$value),]
    # print(ranking)
    country <- ranking[nrow(ranking)/2,1]
    # print(paste("The median country is ", country, sep = ""))
  }
  country_index <- which(df_data$GEO == country)

  text_s <- paste("df_all_compared_to_country_",year_for_comparison,(if (will_use_log) "_log" else ""),
                (if(will_normalise) "_norm" else ""),
                (if(will_use_total_energy_supply) "_tes" else ""),
                (if(will_use_verified_emisions) "_ve" else ""),
                (if(will_use_GDPpc) "_gdp" else ""),
                (if(will_use_population) "_pop" else ""),
                (if(will_use_inflation) "_inf" else ""),
                (if(will_use_agriculture) "_agr" else ""),
                (if(will_use_industry) "_ind" else ""),
                (if(will_use_manufacturing) "_man" else ""),
                ".csv", sep = "")

  
  # Create a 2 D dataframe with countries as rows and countries as columns and the value of the Eukleidian Distance between all the values the two countries
  df_distance <- data.frame(matrix(NA, nrow = nrow(df_data) - 1, ncol = 3))
  colnames(df_distance) <- c("GEO", "df_distance", "df_free_distance")
  for (i in 1 : nrow(df_distance)) { #nolint
  df_distance[i, 1] <- df_data[i, 1]
      df_distance[i, 2] <- sqrt((df_data[i, 2] - df_data[country_index, 2])^2 * weight_total_energy_supply 
      + (df_data[i, 3] - df_data[country_index, 3])^2 * weight_GDPpc
      + (df_data[i, 4] - df_data[country_index, 4])^2 * weight_population
      + (df_data[i, 5] - df_data[country_index, 5])^2 * weight_inflation
      + (df_data[i, 6] - df_data[country_index, 6])^2 * weight_verified_emisions
      + (df_data[i, 7] - df_data[country_index, 7])^2 * weight_agriculture
      + (df_data[i, 8] - df_data[country_index, 8])^2 * weight_industry
      + (df_data[i, 9] - df_data[country_index, 9])^2 * weight_manufacturing
      )
  }
  
  # save a file "distance_2015.tex" with the content of the dataframe df_distance
  #<<results=tex>>
   #   xtable(df_distance[,21:26], caption = "Distance between countries in 2015", label = "tab:distance_2015")
  #@ 
  
  # Calculate Eukleidian distance of df_free_allocation and store them in df_actual_distance
  country_index <- which(df_free$GEO == country)
  for (i in 1 : nrow(df_distance)) { #nolint
    df_distance[i, 3] <- sqrt((df_free[country_index, 2] - df_free[i, 2])^2 )
  }
  # Remove rows with NA and 0
  df_distance <- df_distance[!is.na(df_distance$"df_distance"),]
  df_distance <- df_distance[!is.na(df_distance$"df_free_distance"),]
  df_distance <- df_distance[df_distance$"df_distance" != 0,]
  df_distance <- df_distance[df_distance$"df_free_distance" != 0,]
  
  lm <- lm(df_distance$"df_free_distance" ~ df_distance$"df_distance")
  summary(lm)
  
  #write that summary to a file
  sink(paste("linear_regration_summary_for", text_s, ".txt"))
  summary(lm)
  sink()
  # plot the regression line
  # plot(df_distance$"df_distance", df_distance$"df_free_distance", xlab = "Distance between countries in 2015", ylab = "Distance between countries in 2015 with free allocation", main = paste("Distance between countries in 2015 and in 2015 with free allocation", year_for_comparison))
  # abline(lm, col = "red")
  return (list( data = df_distance, linear =  lm, country = country))
}
  
find_slopes_with_one_country_with_weights <- function( year = 0, country = "none", weights = c(1,1,1,1,1,1,1,1)){
  if(year != 0) { year_for_comparison <- year}
  buffer <- find_slopes_with_one_country(country = country, year = year, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])
  return (buffer)
}

create_graph_for_one <- function (year = 0, name = "", country = "none", weights = c(1,1,1,1,1,1,1,1)){
    if(year != 0) {year_for_comparison <- year}
    buffer <- find_slopes_with_one_country(country = country, year = year_for_comparison, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])
  df_1D <- buffer$data
  lm <- buffer$linear
  country <- buffer$country
  print(country)
  # print(df_1D)
  # Create png with the regression line
  #png(paste("Newscatterplot_with_regression_line_",year_for_comparison,"_with_all_data_and_log=", will_use_log, ".png") , width = 1000, height = 1000)
  png(paste(name, country, " ", year_for_comparison,".png",sep=""), width = 1000, height = 1000)
  #plot(df_1D$"df_distance", df_1D$"d f_free_distance", xlab = "Combined calculated distance", ylab = "Free distance", main = paste( "Scatterplot of calculated distance and actual distance for the year ", year_for_comparison, sep = ""))  # Color red the points of the scatterpolit where df_1D[3,] contains "Germany"
  plot(df_1D$"df_distance", df_1D$"df_free_distance", xlab = "Combined calculated distance", ylab = "Free distance",main = paste( country, " year: ", year_for_comparison, " with r^2: ", summary(lm)$r.squared, sep = ""))
  abline(lm, col = "blue")
  text(df_1D$"df_distance",df_1D$"df_free_distance"+0.02,df_1D$GEO,col='red')
  dev.off()
}

# Create a table for all the countries with the the r^2 value for each country and each year 2005 to 2018 on the columns and the countries on the rows

create_table_for_all_countries <- function(year = 0, weights = c(1,1,1,1,1,1,1,1)){
  if(year != 0) {year_for_comparison <- year}
  df <- data.frame(GEO = character(), slope = numeric(), r_squared = numeric())
  df_free <- read_free()
  for (i in 1 : nrow(df_free)) { #nolint
    buffer <- find_slopes_with_one_country(country = df_free[i, 1], year = year_for_comparison, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])
    df[i, 1] <- df_free[i, 1]
    df[i, 2] <- summary(buffer$linear)$coefficients[2, 1]
    df[i, 3] <- summary(buffer$linear)$r.squared
  }
  return(df)
}

find_the_best_combo <- function(){
  weights <- rep(1,8)
  r_squared <- summary(find_slopes()$linear)$r.squared
  step <- 1
  low <- 0
  high <- 10
  for (i in 1:40){
    # index <- i %% 8 +1
    index <- sample(1:8, 1)
    worth_doing_it <- TRUE
    while(worth_doing_it){
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- summary(find_slopes_with_weights(weights)$linear)$r.squared
        weights[index] <- weights[index] + step
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- summary(find_slopes_with_weights(weights)$linear)$r.squared
        weights[index] <- weights[index] - step
      }
      if (lowered > r_squared){
        r_squared <- lowered
        weights[index] <- weights[index] - step
      } else if (raised > r_squared){
        r_squared <- raised
        weights[index] <- weights[index] + step
      } else {
        worth_doing_it <- FALSE
      }
    }
  }
  print(weights)
  print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  print(paste("R^2:", r_squared))
}

find_the_best_combo_with_one <- function(){
  weights <- rep(1,8)
  r_squared <- summary(find_slopes_with_one_country_with_weights(weights = weights)$linear)$r.squared
  step <- 1
  low <- 0
  high <- 100
  for (i in 1:100){
    index <- i %% 8 +1
    
    #if (i %% 2 == 0 ){
     # index <- (i/2) %% 8+1
    #} else {
    #  index <- sample(1:8, 1)
    #}
    
    #index <- sample(1:8, 1)
    for (j in 1:10){
      random_val <- sample((low + (j-1)*high/10):( j*high/10), 1)
      weight_with_random_val <- weights
      weight_with_random_val[index] <- random_val
      r_squared_with_random_val <- summary(find_slopes_with_one_country_with_weights(weights = weight_with_random_val)$linear)$r.squared
      if (r_squared_with_random_val > r_squared){
        r_squared <- r_squared_with_random_val
        weights[index] <- random_val
        shit <- data.frame(type = c(paste("Population: ", weights[1]), paste("GDPpc: ", weights[2]), paste( "Inflation: ", weights[3]), paste("Agriculture: ", weights[4]), paste("Industry: ", weights[5]), paste("Manufacturing: ", weights[6]), paste("Total Energy Supply: ", weights[7]), paste("Verified Emissions: ", weights[8]))
                           , value = weights)
        
        print(ggplot(shit, aes(x = type, y=value)) +
                geom_bar(stat = "identity") + 
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                ggtitle(paste("R^2 = ", r_squared)))
      }
    }
    worth_doing_it <- TRUE
    while(worth_doing_it){
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- summary(find_slopes_with_one_country_with_weights(weights = weights)$linear)$r.squared
        weights[index] <- weights[index] + step
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- summary(find_slopes_with_one_country_with_weights(weights = weights)$linear)$r.squared
        weights[index] <- weights[index] - step
      }
      if (lowered > r_squared){
        r_squared <- lowered
        weights[index] <- weights[index] - step
      } else if (raised > r_squared){
        r_squared <- raised
        weights[index] <- weights[index] + step
      } else {
        worth_doing_it <- FALSE
      }
      shit <- data.frame(type = c(paste("Population: ", weights[1]), paste("GDPpc: ", weights[2]), paste( "Inflation: ", weights[3]), paste("Agriculture: ", weights[4]), paste("Industry: ", weights[5]), paste("Manufacturing: ", weights[6]), paste("Total Energy Supply: ", weights[7]), paste("Verified Emissions: ", weights[8])), value = weights)
      print(ggplot(shit, aes(x = type, y=value)) +
              geom_bar(stat = "identity") + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1))+
              ggtitle(paste("R^2 = ", r_squared)))
    }
  }
  print(weights)
  print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  print(paste("R^2:", r_squared))
}

p_val <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
MSE <- function(modelobject){
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  return(mean(summary(modelobject)$residuals^2))
}

is_first_linear_regration_better <- function(lm1, lm2){
  return(summary(lm1)$r.squared > summary(lm2)$r.squared && p_val(lm1) < 0.05 && MSE(lm1) < MSE(lm2)*1.5)
}

how_much_good <-function(lm){
  return(paste("R^2:", summary(lm)$r.squared, "p-value:", p_val(lm), "MSE:", MSE(lm)))
}

find_the_better_best_combo <- function(){
  weights <- rep(1,8)

  old <- find_slopes_with_one_country()$linear
  step <- 1
  low <- 0
  high <- 10
  for (i in 1:40){
    # index <- i %% 8 +1
    index <- sample(1:8, 1)
    worth_doing_it <- TRUE
    while(worth_doing_it){
      worth_doing_it <- FALSE
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- find_slopes_with_one_country_with_weights(weights)$linear
        weights[index] <- weights[index] + step
        if(is_first_linear_regration_better(lowered, old)){
          old <- lowered
          weights[index] <- weights[index] - step
          worth_doing_it <- TRUE
      }
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- find_slopes_with_one_country_with_weights(weights)$linear
        weights[index] <- weights[index] - step
        if(is_first_linear_regration_better(raised, old)){
          old <- raised
          weights[index] <- weights[index] + step
          worth_doing_it <- TRUE
      }
    }
    }
  }
  print(weights)
  print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  print(how_much_good(old))
}

All_the_middle_countries <- function(){
    dat <-data.frame(matrix(ncol=5, nrow = 11))
    colnames(dat) <- c("Year", "R^2", "p-value", "MSE", "Country")
    dat$Year <- c(2008:2018)
    for (i in 1:11){
      dat$Country[i] <- find_slopes_with_one_country(year = i + 2007)$country
      dat$"R^2"[i] <- summary(find_slopes_with_one_country(year = i + 2007)$linear)$r.squared
      dat$"p-value"[i] <- p_val(find_slopes_with_one_country(year = i + 2007)$linear)
      dat$MSE[i] <- MSE(find_slopes_with_one_country(year = i + 2007)$linear)
    }
  #  <<results=tex>>
  #    xtable(dat)
  #@
}

All_the_countries_throught_the_years <- function(){
  dat <- data.frame(matrix(ncol=13, nrow = length(list_eur_countries)))
  colnames(dat) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "max p-value", "max MSE")
  rownames(dat) <- list_eur_countries
  for (i in 1:length(list_eur_countries)){
    pv <- 0
    mse <- 0
    for (j in 1:11){
      gg <- find_slopes_with_one_country(year = j + 2007, country = list_eur_countries[i])$linear
      dat[i,j] <- summary(gg)$r.squared
      if (p_val(gg) > pv){
        pv <- p_val(gg)
      }
      if (MSE(gg) > mse){
        mse <- MSE(gg)  
    }
    dat[i,12] <- pv
    dat[i,13] <- mse
  }
  }
  #<<results=tex>>
  #  xtable(dat)
  #@
  #short the rows of the data frame "dat" by the sum of the row values
  dat2 <- dat[ ,-c(12,13)]
  dat2 <- dat2[order(rowSums(dat2), decreasing = TRUE),]
  for( j in 1:nrow(dat2)){
    g <- 0
    for( i in 1:11){
      g <- g + as.numeric(dat2[j,i])
    }
    dat2[j,1] <- g/11
  }
  dat2 <- dat2[,-c(3:11)]
  dat3 <- data.frame(rownames(dat2), dat2$"2008")
  colnames(dat3) <- c("Country", "Average R^2")
 #   <<results=tex>>
 #     xtable(dat3)
 # @
  #make a heatmap of the countries and their R^2 values for each year

  gg <- dat[,-c(12,13)]
  matrixgg <- as.matrix(gg)
  heatmap(matrixgg, Rowv = NA, Colv = NA, scale = "none", col = colorRampPalette(c("red","white", "blue"))(100), margins = c(5, 10), trace = "none", xlab = "Year", ylab = "Country", main = "R^2 values for each country and year")
}

let_s_compare_problematic_poland_france <-function(){
  gg <- read_data(year = 2012)
  gg <- gg[gg$GEO == "Poland",]
  g1 <- read_data(year = 2013)
  g1 <- g1[g1$GEO == "Poland",]
  gg <- rbind(gg, g1)
  g1 <- read_data(year = 2012)
  g1 <- g1[g1$GEO == "France",]
  g2 <- read_data(year = 2013)
  g2 <- g2[g2$GEO == "France",]
  gg <- rbind(gg, g1,g2)
  # transpose gg
  gg <- t(gg)
  colnames(gg) <- c("2012", "2013", "2012", "2013")
  #    <<results=tex>>
  #    xtable(gg)
  #@
}

find_the_better_best_combo_with_one <- function(country = "Hungary", year = 2015 ){
  weights <- rep(1,8)
  old <- find_slopes_with_one_country_with_weights(country = country, year = year, weights = weights)$linear
  step <- 10
  low <- 0
  high <- 1000
  for (i in 1:100){
    #index <- i %% 8 +1
    index <- sample (c(1:8), size=1)

    worth_doing_it <- TRUE
    while(worth_doing_it){
      worth_doing_it <- FALSE
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- find_slopes_with_one_country_with_weights(country = country, year = year, weights = weights)$linear
        weights[index] <- weights[index] + step
        if (is_first_linear_regration_better(lowered, old)){
          old <- lowered
          worth_doing_it <- TRUE
          weights[index] <- weights[index] - step
        }
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- find_slopes_with_one_country_with_weights(country = country, year = year, weights = weights)$linear
        weights[index] <- weights[index] - step
        if (is_first_linear_regration_better(raised, old)){
          old <- raised
          worth_doing_it <- TRUE
          weights[index] <- weights[index] + step
        }
      }
    }
  }
  # print(weights)
  # print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  # print(how_much_good(old))
  return(list(old, weights))
}

All_the_countries_throught_the_years_with_best_combo <- function(){
  dat <- data.frame(matrix(ncol=13, nrow = length(list_eur_countries)))
  colnames(dat) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "max p-value", "max MSE")
  rownames(dat) <- list_eur_countries
  all_weights <- list()
  newi = 0
  for (i in 1:length(list_eur_countries)){
    pv <- 0
    mse <- 0
    if(newi !=i){
      newi <- i
      print(paste("Working on: ",list_eur_countries[i]))
    }
    for (j in 1:11){
      gg<- find_the_better_best_combo_with_one(country = list_eur_countries[i], year = j + 2007)
      all_weights <- append(all_weights, list(gg[[2]]))
      gg <- gg[[1]]
      dat[i,j] <- summary(gg)$r.squared
      if (p_val(gg) > pv){
        pv <- p_val(gg)
      }
      if (MSE(gg) > mse){
        mse <- MSE(gg)  
    }
    dat[i,12] <- pv
    dat[i,13] <- mse
  }
  }
  #<<results=tex>>
  #  xtable(dat)
  #@
    
    
  average_weights <- c(0,0,0,0,0,0,0,0)
  for (i in 1:length(all_weights)){
    average_weights <- average_weights + all_weights[[i]]
  }
  average_weights <- average_weights/length(all_weights)
  different_weights <- hash()
  for (i in 1:length(all_weights)){
    tet <- paste(all_weights[[i]][[1]], all_weights[[i]][[2]], all_weights[[i]][[3]], all_weights[[i]][[4]], all_weights[[i]][[5]], all_weights[[i]][[6]], all_weights[[i]][[7]], all_weights[[i]][[8]],sep = " ")
    if(is.null(different_weights[[tet]])){
      different_weights[[tet]] <- 1
    }
    else {
      different_weights[[tet]] <- different_weights[[tet]] + 1
    }
      
  }
    
  #short the rows of the data frame "dat" by the sum of the row values
  dat2 <- dat[ ,-c(12,13)]
  dat2 <- dat2[order(rowSums(dat2), decreasing = TRUE),]
  for( j in 1:nrow(dat2)){
    g <- 0
    for( i in 1:11){
      g <- g + as.numeric(dat2[j,i])
    }
    dat2[j,1] <- g/11
  }
  dat2 <- dat2[,-c(3:11)]
  dat3 <- data.frame(rownames(dat2), dat2$"2008")
  colnames(dat3) <- c("Country", "Average R^2")
  #  <<results=tex>>
  #    xtable(dat3)
  #@
  #make a heatmap of the countries and their R^2 values for each year

  #gg <- dat[,-c(12,13)]
  #matrixgg <- as.matrix(gg)
  #heatmap(matrixgg, Rowv = NA, Colv = NA, scale = "none", col = colorRampPalette(c("red","white", "blue"))(100), margins = c(5, 10), trace = "none", xlab = "Year", ylab = "Country", main = "R^2 values for each country and year")
}

visualize_population <- function(){
  will_normalise <- FALSE
  pop <- data.frame(matrix(ncol = 3))
  colnames(pop) <- c("Country", "Population","Year")
  for(i in (1:11)){
    print(i)
    gg <-read_data(year = 2007+i)[-c(2,3,5,6,7,8,9)]
    for (j in 1:length(gg[,1])){
      pop[nrow(pop) + 1,] <- c(gg$GEO[j], as.numeric(gg$Population[j]), 2007+i)
    }
  }
  pop <- pop[-c(1),]
  
 # ggplot(data = pop) + 
 #   geom_point(aes(x = Country, y = Population),fill = 'grey') + 
  #  labs(title = "Population vs Country", x = "Country", y = "Population") +
  #  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(sapply(pop, class)) 
  
  # Syntax
  pop$Population = as.numeric(as.character(pop$Population))
  
  
  ggplot(pop, aes(x=Country, y=Population)) + 
    geom_boxplot(
      # custom boxes
      color="blue",
      fill="blue",
      alpha=0.2,
      

      # custom outliers
      outlier.colour="red",
      outlier.fill="red",
      outlier.size=3
    )+
    labs(title = "Population vs Country", x = "Country", y = "Population in Millions")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

  # pop[which(pop$Country == "Greece" | pop$Country == "Belgium"),]
  
  ggplot(pop, aes(x= Year, y = Population, group = Country))+
    geom_point(aes(colour = Country))+
    geom_line (aes(colour = Country)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #Let's make some latex tables
  po <- data.frame(matrix(nrow = length(list_eur_countries), ncol = 17))
  colnames(po) <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,"min","25-quantile","median","75-quantile","max","Std")
  rownames(po) <- list_eur_countries
  for (i in 1:11){
    for(j in 1:length(list_eur_countries)){
      po[j,i]<- pop[which(pop$Year==2007+i & pop$Country==list_eur_countries[j]),]$Population
    }
    po[,i] <- as.numeric(as.character(po[,i]))
  }
  for(i in 1:length(list_eur_countries)){
    po[i,12] <- min(po[i,1:11])
    po[i,13] <- quantile(po[i,1:11], 0.25)
    po[i,14] <- median(as.numeric(po[i,1:11]))
    po[i,15] <- quantile(po[i,1:11], 0.75)
    po[i,16] <- max(po[i,1:11])
    po[i,17] <- sd(po[i,1:11])

  }
  po2 <- po[,-c(1:11)]
  po3 <- po2/1000000
  #<<results=tex>>
  #  xtable(po3)
  #@
}

visualize_Manufacturing  <- function(){
    will_use_log <- FALSE
    will_normalise <- FALSE
  pop <- data.frame(matrix(ncol = 3))
  colnames(pop) <- c("Country", "Manufacturing","Year")
  for(i in (1:11)){
    print(i)
    gg <-read_data(year = 2007+i)[-c(2,3,4,5,6,8,9)]
    for (j in 1:length(gg[,1])){
      pop[nrow(pop) + 1,] <- c(gg$GEO[j], as.numeric(gg$Manufacturing [j]), 2007+i)
    }
  }
  pop <- pop[-c(1),]
  
 # ggplot(data = pop) + 
 #   geom_point(aes(x = Country, y = Population),fill = 'grey') + 
  #  labs(title = "Population vs Country", x = "Country", y = "Population") +
  #  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(sapply(pop, class)) 
  
  # Syntax
  pop$Manufacturing  = as.numeric(as.character(pop$Manufacturing ))
  
  
  #Let's make some latex tables
  po <- data.frame(matrix(nrow = length(list_eur_countries), ncol = 17))
  colnames(po) <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,"min","25-quantile","median","75-quantile","max","Std")
  rownames(po) <- list_eur_countries
  for (i in 1:11){
    for(j in 1:length(list_eur_countries)){
      po[j,i]<- pop[which(pop$Year==2007+i & pop$Country==list_eur_countries[j]),]$Manufacturing 
    }
    po[,i] <- as.numeric(as.character(po[,i]))
  }
  po <- po/1000000000
  for(i in 1:length(list_eur_countries)){
    po[i,12] <- min(po[i,1:11])
    po[i,13] <- quantile(po[i,1:11], 0.25)
    po[i,14] <- median(as.numeric(po[i,1:11]))
    po[i,15] <- quantile(po[i,1:11], 0.75)
    po[i,16] <- max(po[i,1:11])
    po[i,17] <- sd(po[i,1:11])

  }
  po2 <- po[,-c(1:11)]
  #<<results=tex>>
  #  xtable(po2)
  #@

  ggplot(pop, aes(x=Country, y=Manufacturing )) + 
    geom_boxplot(
      # custom boxes
      color="blue",
      fill="blue",
      alpha=0.2,
      
      
      # custom outliers
      outlier.colour="red",
      outlier.fill="red",
      outlier.size=3
    )+
    labs(title = "Manufacturing  vs Country", x = "Country", y = " Manufacturing ")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  # write a function that compares free allocation between 2012 and 2013

compare_2012_2013 <-function(){
  gg <-read_free(year = 2012)
  gg2 <-read_free(year = 2013)
  output <- data.frame(matrix(ncol = 4))
  colnames(output) <- c("Country", "2012","2013","Percentage drop")
  for(i in 1:length(gg[,1])){
    output[nrow(output)+1,] <- c(gg$GEO[i], as.numeric(gg$Free[i]), as.numeric(gg2$Free[i]), as.numeric((as.numeric(gg2$Free[i])-as.numeric(gg$Free[i]))/as.numeric(gg$Free[i])))
  }
  output <- output[-c(1),]
  output$"Percentage drop" <- as.numeric(output$"Percentage drop" )* 100
  output <- output[order(output$"Percentage drop"),]
  output$"Percentage drop" <- round(output$"Percentage drop",2)

  output$"2012" <- as.numeric(output$"2012")/1000000
  output$"2013" <- as.numeric(output$"2013")/1000000

  print(xtable(output), format.args = list(big.mark = ",", decimal.mark = "."))
}

calculate_all_with_given_weights <-function(weights){
    # write a function that calculates all the r^2 values for all the countries and all the years with given weights
 weights <- c(60,20,15,70,410,10,170,850)
dat <- data.frame(matrix(ncol=13, nrow = length(list_eur_countries)))
  colnames(dat) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "max p-value", "max MSE")
  rownames(dat) <- list_eur_countries
  newi = 0
  for (i in 1:length(list_eur_countries)){
    pv <- 0
    mse <- 0
    if(newi !=i){
      newi <- i
      print(paste("Working on: ",list_eur_countries[i]))
    }
    for (j in 1:11){
      gg<- find_slopes_with_one_country_with_weights(country = list_eur_countries[i], year =  2007+j,weights =  weights)$linear
      dat[i,j] <- summary(gg)$r.squared
      if (p_val(gg) > pv){
        pv <- p_val(gg)
      }
      if (MSE(gg) > mse){
        mse <- MSE(gg)  
      }
      dat[i,12] <- pv
      dat[i,13] <- mse
    } 
  }
    
  print(xtable(dat), format.args = list(big.mark = ",", decimal.mark = "."))
}
#calculate_all_with_given_weights(c(60,20,15,70,410,10,170,850))

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

clustering <- function(){
  # Let's cluster the countries based on their features from the read_data() function
  # We will use the k-means algorithm
  # We will use the elbow method, the silhouette method, and the gap statistic to find the optimal number of clusters

  will_normalise <- FALSE
  features <- read_data()
  
  # Normalize based on Germany
  # features$Total_energy_supply <- features$Total_energy_supply / features$Total_energy_supply[9]
  # features$GDPpc <- features$GDPpc / features$GDPpc[9]
  # features$Population <- features$Population / features$Population[9]
  # features$Inflation<- features$Inflation / features$Inflation[9]
  # features$Verified_emissions <- features$Verified_emissions / features$Verified_emissions[9]
  # features$Agriculture <- features$Agriculture / features$Agriculture[9]
  # features$Industry <- features$Industry / features$Industry[9]
  # features$Manufacturing <- features$Manufacturing / features$Manufacturing[9]
  
  # I'll change the values because the clustering produces the error "system is computationally singular: reciprocal condition number = 9.94365e-17"otherwise
  # Values that are closer fixes the problem
  # features$Total_energy_supply <- features$Total_energy_supply / 1000
  # features$GDPpc <- features$GDPpc / 1000 # USD -> thousants USD
  # features$Population <- features$Population / 1000000 # Millions of people
  # features$Inflation<- features$Inflation #Sufficiently small
  # features$Verified_emissions <- features$Verified_emissions / 1000000 # tCO2 equivalent -> ECO2 (exa CO2) equivalent
  # features$Agriculture <- features$Agriculture 
  # features$Industry <- features$Industry
  # features$Manufacturing <- features$Manufacturing 
  
  # Normalize average
  features$Total_energy_supply <- features$Total_energy_supply / mean(features$Total_energy_supply)
  features$GDPpc <- features$GDPpc / mean(features$GDPpc)
  features$Population <- features$Population / mean(features$Population)
  features$Inflation<- features$Inflation / mean(features$Inflation)
  features$Verified_emissions <- features$Verified_emissions / mean( features$Verified_emissions)
  features$Agriculture <- features$Agriculture / mean(features$Agriculture)
  features$Industry <- features$Industry/ mean(features$Industry)
  features$Manufacturing <- features$Manufacturing / mean(features$Manufacturing)
  
  
  
  names_of_df <-names(features)
  for (i in 2:length(names_of_df)){
    features[[i]] <- as.numeric(as.character(features[[i]]))
  }
  gg <- NbClust(features[-c(1)], distance = "euclidean", min.nc = 3, max.nc = 10, method = "kmeans", index = "all")

  #print(xtable(gg$All.index[,14:26]), format.args = list(big.mark = ",", decimal.mark = "."))
  features$partition <- gg$Best.partition
  
  gg <- kmeans(features[-c(1)], 3, 20, 30)
  features$partition <- gg$cluster
  
  features <- features[order(features$partition),]
  #print(xtable(features[-c(2:9)]), format.args = list(big.mark = ",", decimal.mark = "."))
  return(features)
}
clustering_per_capita <-function(){
  will_normalise <- FALSE
  features <- read_data()
  features$Total_energy_supply <- features$Total_energy_supply / features$Population
  features$Verified_emissions <- features$Verified_emissions / features$Population
  features$Agriculture <- features$Agriculture / features$Population
  features$Manufacturing <- features$Manufacturing / features$Population
  features$Industry <- features$Industry / features$Population

  # normalise the data
  
  names_of_df <-names(features)
  for (i in 2:length(names_of_df)){
    features[[i]] <- as.numeric(as.character(features[[i]])) / max(as.numeric(as.character(features[[i]])))
  }
  gg <- NbClust(features[-c(1,4)], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")


  print(xtable(gg$All.index[,14:26]), format.args = list(big.mark = ",", decimal.mark = "."))
  features$partition <- gg$Best.partition
  features <- features[order(features$partition),]
  print(xtable(features[c(1,4,10)]), format.args = list(big.mark = ",", decimal.mark = "."))
}

can_countries_explain_their_own_cluster <-function(){
  # Για να τρέξει πρέπει πρώτα να έχουμε βάλει όλες τις χώρες ξανά.
  will_normalise <- FALSE
  
  features <- clustering()
  feat <-list()
  slop <- list()
  # First cluster
  for (i in 1:3){
    feat[[i]] <- features[features$partition == i,]
    temp <- list_eur_countries
    list_eur_countries <- feat[[i]]$GEO
    slop[[i]] <- find_slopes_with_one_country()
    list_eur_countries <-temp
  }
  # Plot it 
  select <-2
  ggplot(slop[[select]]$data, aes(x = df_distance, y = df_free_distance)) + 
    geom_point(aes(color = GEO)) + 
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Distance in features") + 
    ylab("Distance in free") +
    labs(title = paste("Distances from ", slop[[select]]$country ," at ",as.character(select)," cluster and r^2 = ", as.character(summary(slop[[select]]$linear)$r.squared)))
  
  
  #list_eur_countries <- gg1$GEO
  # Αυτό εδώ μπορεί να μας βρει πολύ γρήγορα ποιο είναι το βέλτιστο σετ.
  #g1 <- find_the_best_combo_with_one()

  # graph the linear regration
}

#Let's find out if we can find a relation between the features of each country and the free allocation it got

features_linear_free <- function(){
  will_normalise <- FALSE
  dat <- read_data()
  free <- read_free()
  dat <- merge(dat, free, by = "GEO")
  select <- 2
  # keep only the countries of the list "clusters"
  dat <- dat[dat$GEO %in% clusters[[select]],]
  #dat <- dat[-which(dat$GEO == "Germany"),]
  linear <- list()
  linear[[select]] <- lm(dat$Free ~    dat$Population)
  summary(linear[[select]])
  summary(linear[[select]])$r.squared
  
  ggplot(dat, aes(x = Population, y = Free)) + 
    geom_point(aes(color = GEO)) + 
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Industry in Billios USD (as calculated in GDP)") + 
    ylab("Free allocation") +
    labs(title = paste("Relationship of the", as.character(select)," cluster and r^2 = ", as.character(summary(linear[[select]])$r.squared)))
}

# Are these correlated?
gg <-function(){
  g<- read_data()
  ff <- lm(g$Agriculture ~ g$GDPpc)
  summary(ff)
  ggplot(g, aes(x = Agriculture, y = Industry)) + 
    geom_point(aes(color = GEO)) + 
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Agriculture") + 
    ylab("GDPpc")
    
}

Fotakis_idea <- function(){
  features <- clustering()
  free <- read_free()
  features <- merge(features, free, by = "GEO")
  for (i in 1:25){
    if (features$partition[i] == 1){
      features$partition[i] <- "Industrialized"
    }
    if (features$partition[i] == 2){
      features$partition[i] <- "USSR"
    }
    if (features$partition[i] == 3){
      features$partition[i] <- "REST"
    }
  }
  

  ggplot(features, aes(x = Verified_emissions, y = Free)) + 
    geom_point(aes(color = partition)) + 
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Verified 2008") + 
    ylab("Free") 
}

