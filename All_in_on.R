library("hash")
library("xlsx")
library("xtable") # Load xtable package
library("stringr")
library("DBI")
library("RMariaDB")
library("RMySQL")
library("ggplot2")
library("factoextra")
library("NbClust")
library("dplyr")
library("lpSolve")
library("svglite")
library("shiny")
library(corrplot)
library(foreach)
library(doParallel)
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
will_normalise <- TRUE
force_fresh_data <- FALSE
use_mean_for_missing_data <- TRUE
will_use_free <- TRUE
will_use_energy_intensity <- TRUE
Manufacturing_Industry_Agriculture_as_percentage <- TRUE
cached_data <- hash()
cached_free <- hash()

data_path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Data/"

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
                  (if(will_use_free) "_free" else ""),
                  (if(will_use_energy_intensity) "_ei" else ""),
                  ".csv", sep = "")
  if(!force_fresh_data & !is.null(cached_data[[text_s]])){
    #print (paste("Using Cached Data from hash for: ", year_for_comparison))
    return(cached_data[[text_s]])
  }
  if (!force_fresh_data & file.exists(paste(data_path,"created_csvs/",text_s, sep = ""))) {
    #print ("Using Cached Data from csv")
    cached_data[[text_s]] <- data.frame(read.csv(file = paste(data_path,"created_csvs/",text_s, sep = ""), header = TRUE)[-c(1)])
    return(data.frame(read.csv(file = paste(data_path,"created_csvs/",text_s, sep = ""),
                               header = TRUE)[-c(1)]))
  }
  
  # import countries from databse
  kanali <- dbConnect(RMySQL::MySQL(),
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

    
    df_ei <- read.csv(file = paste0(data_path,"nrg_ind_ei_linear.csv"),
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
    
    df_emis <- read.csv(file = paste0(data_path,"Historical emissions_data_2.csv"),
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
    
    df_free <- read.csv(file = paste0(data_path,"Historical emissions_data.csv"),
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
    
    headers <- read.csv(file = paste0(data_path,"GDP_per_capita_1960_2021.csv"),
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_GDPpc <- read.csv(file = paste0(data_path,"GDP_per_capita_1960_2021.csv"),
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
    headers <- read.csv(file = paste0(data_path,"Inflation_1960_2021.csv"),
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_inflation <- read.csv(file = paste0(data_path,"Inflation_1960_2021.csv"),
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
    headers <- read.csv(file = paste0(data_path,"API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv"),
                        skip = 4,
                        header = FALSE,
                        nrows = 1,
                        as.is = TRUE)
    df_population <- read.csv(file = paste0(data_path,"API_SP.POP.TOTL_DS2_en_csv_v2_4701113.csv"),
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
    
    d <- read.csv(file = paste0(data_path,"nrg_bal_s__custom_4143365_linear.csv"),
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
    my_data <- read.csv(file = paste0(data_path,"2dbe830a-5afc-4ed9-b478-f5349450364b_Data.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
    
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
    max_Free <- max(abs(df_1$Free))
    max_EI <- max(abs(df_1$Energy_Intensity))
    
    # devide by max value of each column to normalise data unless max value is 0
    if(max_total_energy_supply  != 0) df_1$Total_energy_supply <- df_1$Total_energy_supply / max_total_energy_supply
    if(max_inflation            != 0) df_1$Inflation <- df_1$Inflation / max_inflation
    if(max_GDPpc                != 0) df_1$GDPpc <- df_1$GDPpc / max_GDPpc
    if(max_population           != 0) df_1$Population <- df_1$Population / max_population
    if(max_verified_emissions   != 0) df_1$Verified_emissions <- df_1$Verified_emissions / max_verified_emissions
    if(max_Agriculture          != 0) df_1$Agriculture <- df_1$Agriculture / max_Agriculture
    if(max_Industry             != 0) df_1$Industry <- df_1$Industry / max_Industry
    if(max_Manufacturing        != 0) df_1$Manufacturing <- df_1$Manufacturing / max_Manufacturing
    if(max_Free                 != 0) df_1$Free <- df_1$Free/max_Free
    if(max_EI                   != 0) df_1$Energy_Intensity <- df_1$Energy_Intensity / max_EI
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
                  (if(will_use_free) "_free" else ""),
                  (if(will_use_energy_intensity) "_ei" else ""),
                  ".csv", sep = "")
  write.csv(df_1, file = paste(data_path,"created_csvs/",text_t, sep = "" ), row.names = TRUE)
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
    if (!force_fresh_data & file.exists(paste(data_path,"created_csvs/",text_s, sep = ""))) {
        #print ("Using Cached Data from csv")
        cached_data[[text_s]] <- data.frame(read.csv(file = paste("./Data/created_csvs/",text_s, sep = ""), header = TRUE)[-c(1)])
        return(data.frame(read.csv(file = paste(data_path,"created_csvs/",text_s, sep = ""),
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
  kanali <- dbConnect(RMySQL::MySQL(),
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
      "SELECT CAST(SUM(freeAlloc) AS DOUBLE) FROM `eutl_compliance` WHERE country = '",
      countries$eu_abbr2L[which (countries$name == list_eur_countries[i])], "' AND etos ='",year_for_comparison,"'", sep = "", collapse = NULL)
    #res <- dbSendQuery(kanali, querr) # send query to database
    #free <- dbFetch(res, n = -1) # fetch all data from querry
    # Use dbGetQuery to send and fetch results in one step
    free <- dbGetQuery(kanali, querr)

    #dbClearResult(res) # clear result

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
  df_data <-read_data_2(year_for_comparison)
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

# Function to select 'n' points with closest Euclidean distances and move them to df_below_threshold
select_closest_points <- function(df_1D, n) {
  df_below_threshold <- data.frame() # Initialize empty dataframe for below threshold
  df_remaining <- df_1D              # Copy of the original dataframe to modify
  
  for (i in 1:n) {
    # Calculate Euclidean distances between all points using both x and y values
    distance_matrix <- as.matrix(dist(df_remaining[, c("df_distance", "df_free_distance")]))
    
    # Set the diagonal to a large value to ignore self-distances
    diag(distance_matrix) <- Inf
    
    # Find the row index of the closest pair of points
    min_distance <- which(distance_matrix == min(distance_matrix), arr.ind = TRUE)[1,]
    closest_point_index <- min_distance[1]  # Get index of one of the closest points
    
    # Move the closest point to df_below_threshold
    df_below_threshold <- rbind(df_below_threshold, df_remaining[closest_point_index,])
    
    # Remove the selected point from df_remaining
    df_remaining <- df_remaining[-closest_point_index,]
  }
  
  # The remaining points are assigned to df_above_threshold
  df_above_threshold <- df_remaining
  
  list(df_below_threshold = df_below_threshold, df_above_threshold = df_above_threshold)
}

create_graph <- function (year = 0, type = "png", name = "01_distances_from_features_to_frees_all_countries_", path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/distnaces_all_from_all/", weights = c(1,1,1,1,1,1,1,1)) {
  
  if (year != 0) {
    year_for_comparison <- year
  }
  
  
  # Assuming weights is already passed correctly
  slopes <- find_slopes(year = year_for_comparison, 
                       weight_population = weights[1], weight_GDPpc = weights[2], 
                       weight_inflation = weights[3], weight_agriculture = weights[4], 
                       weight_industry = weights[5], weight_manufacturing = weights[6], 
                       weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])
  df_1D <- slopes$data
  
  lm <- slopes$linear
  
  if (type == "png"){
    # Set up plot file
    png(paste(path, name, year, ".png", sep=""), width = 1000, height = 1000)
  }else if(type == "svg"){
    svg(paste(path, name, year, ".svg", sep=""))
  }

  
  # Base plot with grid lines
  plot(df_1D$"df_distance", df_1D$"df_free_distance", 
       xlab = "Combined calculated distance", 
       ylab = "Free distance", 
       main = paste("Scatterplot of calculated distance and actual distance for the year ", year, sep = ""),
       pch = 20, col = "black")  # Base points in black
  
  grid()  # Add grid lines
  
  # Define countries and colors in a vector for reuse
  countries <- c("Germany", "Greece", "Italy", "Ukraine", "France", "United Kingdom", "Luxembourg")
  colors <- c("red", "blue", "green", "yellow", "orange", "purple")
  
  # Use a loop to color points by country
  for (i in seq_along(countries)) {
    country <- countries[i]
    color <- colors[i]
    
    # Highlight points for each country with the corresponding color
    points(df_1D[str_detect(df_1D$"pair", regex(paste0(".", country))), 1], 
           df_1D[str_detect(df_1D$"pair", regex(paste0(".", country))), 2], 
           col = color, pch = 20)
    points(df_1D[str_detect(df_1D$"pair", regex(paste0(country, "."))), 1], 
           df_1D[str_detect(df_1D$"pair", regex(paste0(country, "."))), 2], 
           col = color, pch = 20)
  }
  
  # Add regression line
  abline(lm, col = "red", lwd = 2)
  
  # Add a legend
  legend("bottomright", legend = countries, col = colors, pch = 20, title = "Countries")
  
  # Close the file device
  dev.off()
}


find_slopes_with_weights <- function(weights){
  return (find_slopes(weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8]))
}

find_slopes_with_one_country <- function(year = 0, weight_population = 1, weight_GDPpc = 1, weight_inflation = 1, weight_agriculture = 1, weight_industry = 1, weight_manufacturing = 1, weight_total_energy_supply = 1, weight_verified_emisions = 1, country = "none"){
  if(year != 0) {year_for_comparison <- year}
  

  #df_data <-read_data( TRUE, 2016,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  df_data <-read_data_2(year_for_comparison)
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
  return (find_slopes_with_one_country(country = country, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8]))
}

create_graph_for_one <- function(year = 0, name = "", type = "png", country = "none", weights = c(1,1,1,1,1,1,1,1), path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/02_distances_from_one/") {
  # Set the comparison year
  if (year != 0) {
    year_for_comparison <- year
  }
  
  # Fetch data and linear model
  buffer <- find_slopes_with_one_country(country = country, year = year_for_comparison,
                                         weight_population = weights[1], weight_GDPpc = weights[2],
                                         weight_inflation = weights[3], weight_agriculture = weights[4],
                                         weight_industry = weights[5], weight_manufacturing = weights[6],
                                         weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])
  
  df_1D <- buffer$data
  lm <- buffer$linear
  country <- buffer$country
  
  # Print country for verification
  print(paste("Generating plot for country:", country))
  
  # Construct the file path and name
  file_name <- paste0(path, name, country, "_", year_for_comparison, ".", type)
  
  # Start device
  if (type == "png") {
    png(file_name, width = 1000, height = 1000)
    # png(file_name)
  } else if (type == "svg") {
    svg(file_name)
  }
  # Define color scheme
  color_below_threshold <- '#E24A33'   # Color for points with x < 2e08
  color_main <- '#348ABD'              # Main color for other points
  color_regression <- 'black'          # Color for regression line
  color_grid <- 'grey'                 # Grid color
  
  
  
  # Define color palette with 30 distinct colors
  color_palette <- c("#E24A33", "#348ABD", "#988ED5", "#777777", "#FBC15E",
                     "#8EBA42", "#FFB5B8", "#FF7F0E", "#1F77B4", "#AEC7E8",
                     "#FF9896", "#C5B0D5", "#C49C94", "#9467BD", "#8C564B",
                     "#D62728", "#BCBD22", "#17BECF", "#9EDAE5", "#2CA02C",
                     "#98DF8A", "#FFBB78", "#F7B6D2", "#C7C7C7", "#DBDB8D",
                     "#B5BD61", "#17BECF", "#C49C94", "#FFBB78", "#9EDAE5")
  
  # Set size of points
  if (type == 'png'){
    point_size <- 4
  } else {
    point_size <- 1
  }

  
  # Define axis limits based on the data range
  xlim_range <- c(0, max(df_1D$df_distance, na.rm = TRUE))
  ylim_range <- c(0, max(df_1D$df_free_distance, na.rm = TRUE))
  
  # Make the n closest be put on the legend.
  result <- select_closest_points(df_1D, n = 14)  # Adjust 'n' as needed
  df_below_threshold <- result$df_below_threshold
  df_above_threshold <- result$df_above_threshold
  
  # Plot main points (x >= 2e08) with defined axis limits
  plot(df_above_threshold$df_distance, df_above_threshold$df_free_distance,
       xlab = "Combined Calculated Distance",
       ylab = "Free Distance",
       main = paste(country, "Year:", year_for_comparison, " (R² = ", round(summary(lm)$r.squared, 3), ")", sep = ""),
       pch = 20, cex = point_size, col = color_main,
       xlim = xlim_range, ylim = ylim_range,  # Set axis limits
       cex.main = 1.8)
  
  # Add labels for points with x >= 2e08 directly after plotting
  text(df_above_threshold$df_distance, df_above_threshold$df_free_distance + 0.02, 
       labels = df_above_threshold$GEO, pos = 3, col = color_main, cex = 1.2)
  
  # Add grid lines for clarity
  grid(col = color_grid)
  
  # Add regression line in black
  abline(lm, col = color_regression, lwd = 2)
  
  # Plot each point with x < 2e08 in unique color from the color palette
  for (i in seq_len(nrow(df_below_threshold))) {
    points(df_below_threshold$df_distance[i], df_below_threshold$df_free_distance[i],
           pch = 20, cex = point_size, col = color_palette[i])
  }
  
  # Add a legend with unique colors for each country with x < 2e08
  legend("bottomright", legend = df_below_threshold$GEO, col = color_palette[seq_len(nrow(df_below_threshold))], 
         pch = 20, title = "Countries")
  
  # Close the device
  dev.off()
}

# Create a table for all the countries with the the r^2 value for each country and each year 2005 to 2018 on the columns and the countries on the rows

create_table_for_all_countries <- function(year = 0, weights = c(1,1,1,1,1,1,1,1)){
  #browser()
  if(year != 0) {year_for_comparison <- year}
  df_free <- read_free()
  l = nrow(df_free)
  df <- data.frame(GEO = rep(NA, l), slope = rep(NA, l), r_squared = rep(NA, l))
  df$GEO <- df_free$GEO
  for (i in seq_along(df$GEO)) {
    country <- df$GEO[i]    
    buffer <- find_slopes_with_one_country(country = country, year = year_for_comparison, weight_population = weights[1], weight_GDPpc = weights[2], weight_inflation = weights[3], weight_agriculture = weights[4], weight_industry = weights[5], weight_manufacturing = weights[6], weight_total_energy_supply = weights[7], weight_verified_emisions = weights[8])
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

find_the_best_combo_with_one <- function(year = 0, country = "none"){
  weights <- rep(1,8)
  r_squared <- summary(find_slopes_with_one_country_with_weights(year = 0, country = "none", weights = weights)$linear)$r.squared
  step <- 10
  low <- 0
  high <- 100
  for (i in 1:96){
    index <- i %% 8 +1
    
    if (i %% 2 == 0 ){
      index <- (i/2) %% 8+1
    } else {
      index <- sample(1:8, 1)
    }
    
    #index <- sample(1:8, 1)
    for (j in 1:10){
      random_val <- sample((low + (j-1)*high/10):( j*high/10), 1)
      weight_with_random_val <- weights
      weight_with_random_val[index] <- random_val
      r_squared_with_random_val <- summary(find_slopes_with_one_country_with_weights(year = 0, country = "none", weights = weight_with_random_val)$linear)$r.squared
      if (r_squared_with_random_val > r_squared){
        r_squared <- r_squared_with_random_val
        weights[index] <- random_val
        shit <- data.frame(type = c(paste("Population: ", weights[1]), paste("GDPpc: ", weights[2]), paste( "Inflation: ", weights[3]), paste("Agriculture: ", weights[4]), paste("Industry: ", weights[5]), paste("Manufacturing: ", weights[6]), paste("Total Energy Supply: ", weights[7]), paste("Verified Emissions: ", weights[8]))
                           , value = weights)
        
        #print(ggplot(shit, aes(x = type, y=value)) +
        #        geom_bar(stat = "identity") + 
        #        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        #        ggtitle(paste("R^2 = ", r_squared)))
      }
    }
    worth_doing_it <- TRUE
    while(worth_doing_it){
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- summary(find_slopes_with_one_country_with_weights(year = 0, country = "none", weights = weights)$linear)$r.squared
        weights[index] <- weights[index] + step
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- summary(find_slopes_with_one_country_with_weights(year = 0, country = "none", weights = weights)$linear)$r.squared
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
      #print(ggplot(shit, aes(x = type, y=value)) +
      #        geom_bar(stat = "identity") + 
      #        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      #        ggtitle(paste("R^2 = ", r_squared)))
    }
  }
  print(weights)
  print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  print(paste("R^2:", r_squared))
  return (weights)
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



# Usage:
# Assuming you have a dataframe 'df' and a list of European countries 'list_eur_countries'
# final_data <- analyze_european_data(df, list_eur_countries, "log_file.txt", "plots/", 2008, 2018)


is_first_linear_regration_better <- function(lm1, lm2){
  return(summary(lm1)$r.squared > summary(lm2)$r.squared && (p_val(lm1) < 0.05 || p_val(lm1) < p_val(lm2)) && MSE(lm1) < MSE(lm2)*1.5)
}

how_much_good <-function(lm){
  return(paste("R^2:", summary(lm)$r.squared, "p-value:", p_val(lm), "MSE:", MSE(lm)))
}

find_the_better_best_combo <- function(year = 0){
  weights <- rep(1,8)

  old <- find_slopes_with_one_country(year = year)$linear
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
        raised <- find_slopes_with_one_country_with_weights(weights = weights)$linear
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
      buffer <- find_slopes_with_one_country(year = i + 2007)
      dat$Country[i] <- buffer$country
      dat$"R^2"[i] <- summary(buffer$linear)$r.squared
      dat$"p-value"[i] <- p_val(buffer$linear)
      dat$MSE[i] <- MSE(buffer$linear)
    }
    #<<results=tex>>
    #  xtable(dat)
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
  #country <-"Hungary"
  weights <- rep(1,8)
  old <- find_slopes_with_one_country_with_weights(country = country, year = year, weights = weights)$linear
  r_squared <- summary(old)$r.squared
  step <- 10
  low <- 0
  high <- 50
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
          r_squared <- summary(old)$r.squared
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
          r_squared <- summary(old)$r.squared
        }
      }
    }
    shit <- data.frame(type = c(paste("Population: ", weights[1]), paste("GDPpc: ", weights[2]), paste( "Inflation: ", weights[3]), paste("Agriculture: ", weights[4]), paste("Industry: ", weights[5]), paste("Manufacturing: ", weights[6]), paste("Total Energy Supply: ", weights[7]), paste("Verified Emissions: ", weights[8]))
                       , value = weights)
    
    print(ggplot(shit, aes(x = type, y=value)) +
            geom_bar(stat = "identity") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            ggtitle(paste("R^2 = ", r_squared)))
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

  gg <- dat[,-c(12,13)]
  matrixgg <- as.matrix(gg)
  heatmap(matrixgg, Rowv = NA, Colv = NA, scale = "none", col = colorRampPalette(c("red","white", "blue"))(100), margins = c(5, 10), trace = "none", xlab = "Year", ylab = "Country", main = "R^2 values for each country and year")
  return(dat)
  }

log_message <- function(message, log_file) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- paste("[", timestamp, "]", message)
  write(entry, file = log_file, append = TRUE)
}


#EWxperiment 1-2
perform_analysis <- function(log_file, plot_path, start_year, end_year) {
  will_normalise <- TRUE
  
  # Define the year range
  years <- start_year:end_year
  # Initialize data frames to store R^2 and model objects
  r_squared_df <- data.frame(matrix(NA, nrow = length(list_eur_countries), ncol = length(years)))
  rownames(r_squared_df) <- list_eur_countries
  colnames(r_squared_df) <- years
  
  linear_models_df <- data.frame(matrix(vector("list", length(list_eur_countries) * length(years)), 
                                        nrow = length(list_eur_countries), ncol = length(years)))
  rownames(linear_models_df) <- list_eur_countries
  colnames(linear_models_df) <- years
  
  # Helper function to log messages with timestamp
  log_message <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    entry <- paste("[", timestamp, "]", message)
    write(entry, file = log_file, append = TRUE)
  }
  # Iterate over each country and year
  for (country in list_eur_countries) {
    for (year in years) {
      
      # Log the start of the iteration
      log_message(paste("Starting analysis for Country:", country, "Year:", year))
      
      tryCatch({
        # Step 1: Find the best combination of weights
        weights <- find_the_best_combo_with_one(year = year, country = country)
        
        # Step 2: Perform linear regression with the optimal weights
        model_data <- find_slopes_with_one_country_with_weights(year = year, country = country, weights = weights)
        linear_model <- model_data$linear
        r_squared <- summary(linear_model)$r.squared
        p_val_model <- p_val(linear_model)
        mse_model <- MSE(linear_model)
        
        # Step 3: Store R^2 and the model in respective data frames
        r_squared_df[country, as.character(year)] <- r_squared
        linear_models_df[[country, as.character(year)]] <- linear_model
        
        # Plot creation wrapped in tryCatch to handle any errors
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "png", 
                               country = country, 
                               weights = weights, 
                               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/")
        }, error = function(e) {
          log_message(paste("Failed to create PNG plot for Country:", country, "Year:", year, "- Error:", e$message))
        })
        
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "svg", 
                               country = country, 
                               weights = weights, 
                               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/")
        }, error = function(e) {
          log_message(paste("Failed to create SVG plot for Country:", country, "Year:", year, "- Error:", e$message))
        })
        
        # Log successful completion of the iteration
        log_message(paste("Completed analysis for Country:", country, "Year:", year, 
                          "- Weights:", toString(weights), 
                          "- R^2:", r_squared, 
                          "- P-Value:", p_val_model, 
                          "- MSE:", mse_model))
        
      }, error = function(e) {
        # Log any error that occurs in the main analysis steps
        log_message(paste("Failed analysis for Country:", country, "Year:", year, "- Error:", e$message))
      })
    }
  }
  
  # Return a list with the r_squared and linear models data frames
  return(list(R_Squared = r_squared_df, Linear_Models = linear_models_df))
}

perform_analysis_parallel <- function(log_file, plot_path, start_year, end_year) {
  will_normalise <- TRUE
  
  # Define the year range
  years <- start_year:end_year
  
  # Initialize data frames to store R^2 and model objects
  r_squared_df <- data.frame(matrix(NA, nrow = length(list_eur_countries), ncol = length(years)))
  rownames(r_squared_df) <- list_eur_countries
  colnames(r_squared_df) <- years
  
  linear_models_df <- data.frame(matrix(vector("list", length(list_eur_countries) * length(years)), 
                                        nrow = length(list_eur_countries), ncol = length(years)))
  rownames(linear_models_df) <- list_eur_countries
  colnames(linear_models_df) <- years
  
  # Initialize parallel backend
  numCores <- detectCores() - 1  # Use one less core than available
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Parallel processing of the inner loop
  results <- foreach(country = list_eur_countries, .combine = list, .packages = c("doParallel", "foreach")) %dopar% {
    foreach(year = years, .combine = list) %do% {
      
      # Create a temporary log file for each parallel task
      temp_log_file <- paste0("temp_log_", country, "_", year, ".txt")
      
      # Helper function to log messages with timestamp
      log_message <- function(message) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        entry <- paste("[", timestamp, "]", message)
        write(entry, file = temp_log_file, append = TRUE)
      }
      
      # Initialize variables to handle potential errors
      r_squared <- NA
      linear_model <- NULL
      
      # Log the start of the iteration
      log_message(paste("Starting analysis for Country:", country, "Year:", year))
      
      tryCatch({
        # Step 1: Find the best combination of weights
        weights <- find_the_best_combo_with_one(year = year, country = country)
        
        # Step 2: Perform linear regression with the optimal weights
        model_data <- find_slopes_with_one_country_with_weights(year = year, country = country, weights = weights)
        linear_model <- model_data$linear
        r_squared <- summary(linear_model)$r.squared
        p_val_model <- p_val(linear_model)
        mse_model <- MSE(linear_model)
        
        # Plot creation wrapped in tryCatch to handle any errors
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "png", 
                               country = country, 
                               weights = weights, 
                               path = plot_path)
        }, error = function(e) {
          log_message(paste("Failed to create PNG plot for Country:", country, "Year:", year, "- Error:", e$message))
        })
        
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "svg", 
                               country = country, 
                               weights = weights, 
                               path = plot_path)
        }, error = function(e) {
          log_message(paste("Failed to create SVG plot for Country:", country, "Year:", year, "- Error:", e$message))
        })
        
        # Log successful completion of the iteration
        log_message(paste("Completed analysis for Country:", country, "Year:", year, 
                          "- Weights:", toString(weights), 
                          "- R^2:", r_squared, 
                          "- P-Value:", p_val_model, 
                          "- MSE:", mse_model))
        
      }, error = function(e) {
        # Log any error that occurs in the main analysis steps
        log_message(paste("Failed analysis for Country:", country, "Year:", year, "- Error:", e$message))
      })
      
      # Return a list of results for this iteration
      list(
        r_squared = r_squared,
        linear_model = linear_model,
        country = country,
        year = year
      )
    }
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Merge temporary logs into main log file
  log_files <- list.files(pattern = "temp_log_")
  for (temp_log in log_files) {
    file.append(log_file, temp_log)
    file.remove(temp_log)
  }
  
  # Update the main data frames with the results
  for (result in unlist(results, recursive = FALSE)) {
    r_squared_df[result$country, as.character(result$year)] <- result$r_squared
    linear_models_df[[result$country, as.character(result$year)]] <- result$linear_model
  }
  
  # Return a list with the r_squared and linear models data frames
  return(list(R_Squared = r_squared_df, Linear_Models = linear_models_df))
}


# Main function to visualize attribute trends and summarize data
visualize_attribute <- function(attribute_name, year_range = 2008:2018) {
  # Initialize an empty data frame to store data over years
  attribute_data <- data.frame(Country = character(), Value = numeric(), Year = integer(), stringsAsFactors = FALSE)
  
  # Load and combine data for each year
  for (year in year_range) {
    yearly_data <- read_data_2(year = year)
    attribute_col_index <- grep(attribute_name, colnames(yearly_data), ignore.case = TRUE)
    yearly_data <- yearly_data[, c(1, attribute_col_index)]
    yearly_data$Year <- year
    colnames(yearly_data) <- c("Country", "Value", "Year")  # Standardize column names
    attribute_data <- rbind(attribute_data, yearly_data)
  }
  
  # Convert columns to numeric types where needed
  attribute_data <- attribute_data %>%
    mutate(Value = as.numeric(as.character(Value)), Year = as.numeric(Year))
  
  # Generate plot objects
  scatter_plot <- plot_scatter(attribute_data, attribute_name)
  box_plot <- plot_box(attribute_data, attribute_name)
  trends_plot <- plot_trends(attribute_data, attribute_name)
  
  # Prepare the summary table
  summary_table <- create_summary_table(attribute_data, year_range, attribute_name)
  
  # Return a list containing the plots and the summary table
  return(list(
    scatter_plot = scatter_plot,
    box_plot = box_plot,
    trends_plot = trends_plot,
    summary_table = summary_table
  ))
}

# Plot functions
plot_scatter <- function(data, attribute_name) {
  ggplot(data) +
    geom_point(aes(x = Country, y = Value), fill = 'grey') +
    labs(title = paste("Country-Wise", attribute_name, "Scatter Plot"), x = "Country", y = attribute_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_box <- function(data, attribute_name) {
  ggplot(data, aes(x = Country, y = Value)) +
    geom_boxplot(color = "blue", fill = "blue", alpha = 0.2, outlier.colour = "red", outlier.size = 3) +
    labs(title = paste(attribute_name, "Distribution by Country"), x = "Country", y = paste(attribute_name, "in Millions")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_trends <- function(data, attribute_name) {
  ggplot(data, aes(x = Year, y = Value, group = Country, colour = Country)) +
    geom_point() +
    geom_line() +
    labs(title = paste(attribute_name, "Trends Over Time by Country"), x = "Year", y = attribute_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Helper function to create a summary table for any attribute
create_summary_table <- function(data, year_range, attribute_name) {
  summary_table <- data.frame(matrix(nrow = length(list_eur_countries), ncol = length(year_range) + 6))
  colnames(summary_table) <- c(year_range, "Min", "Q1", "Median", "Q3", "Max", "StdDev")
  rownames(summary_table) <- list_eur_countries
  
  # Populate yearly data for each country
  for (year in year_range) {
    year_column <- year - min(year_range) + 1  # Adjust column index based on the range start
    for (country in list_eur_countries) {
      yearly_value <- data %>%
        filter(Year == year, Country == country) %>%
        pull(Value)
      summary_table[country, year_column] <- as.numeric(yearly_value)
    }
  }
  
  # Calculate and assign summary statistics for each country
  summary_table[, (length(year_range) + 1):ncol(summary_table)] <- t(apply(summary_table[, 1:length(year_range)], 1, calculate_summary_stats))
  
  return(summary_table)
}

# Helper function to calculate summary statistics for a vector of values
calculate_summary_stats <- function(values) {
  values <- as.numeric(values)  # Ensure values are numeric
  stats <- c(
    Min = min(values, na.rm = TRUE),
    Q1 = quantile(values, 0.25, na.rm = TRUE),
    Median = median(values, na.rm = TRUE),
    Q3 = quantile(values, 0.75, na.rm = TRUE),
    Max = max(values, na.rm = TRUE),
    StdDev = sd(values, na.rm = TRUE)
  )
  return(stats)
}


  # write a function that compares free allocation between 2012 and 2013
compare_free_in_two_years <- function(year1 = 2012, year2 = 2013) {
  # Read data for the specified years
  data_year1 <- read_free(year = year1)
  data_year2 <- read_free(year = year2)
  
  # Ensure that 'Country' and 'Free' columns exist in both datasets
  if (!("GEO" %in% colnames(data_year1)) || !("Free" %in% colnames(data_year1)) ||
      !("GEO" %in% colnames(data_year2)) || !("Free" %in% colnames(data_year2))) {
    stop("Expected columns 'GEO' and 'Free' not found in data.")
  }
  
  # Join data for both years by 'Country' (GEO column)
  output <- data_year1 %>%
    rename(Country = GEO, Free_Year1 = Free) %>%
    inner_join(data_year2 %>% rename(Country = GEO, Free_Year2 = Free), by = "Country") %>%
    mutate(
      Free_Year1 = as.numeric(Free_Year1) / 1e6,  # Convert to millions
      Free_Year2 = as.numeric(Free_Year2) / 1e6,  # Convert to millions
      Percentage_Drop = round(((Free_Year2 - Free_Year1) / Free_Year1) * 100, 2)
    ) %>%
    select(Country, Free_Year1, Free_Year2, Percentage_Drop) %>%
    arrange(Percentage_Drop)  # Order by percentage drop
  
  # Rename columns for clarity
  colnames(output) <- c("Country", as.character(year1), as.character(year2), "Percentage Drop (%)")
  
  # Print as LaTeX table
  print(xtable(output), format.args = list(big.mark = ",", decimal.mark = "."))
  
  return(output)
}

calculate_all_with_given_weights <- function(weights = c(60, 20, 15, 70, 410, 10, 170, 850)) {
  # Initialize the results data frame
  dat <- data.frame(matrix(ncol = 13, nrow = length(list_eur_countries)))
  colnames(dat) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Max P-Value", "Max MSE")
  rownames(dat) <- list_eur_countries
  
  # Loop over countries and years to calculate R^2, p-values, and MSE
  for (i in seq_along(list_eur_countries)) {
    country <- list_eur_countries[i]
    max_pv <- 0
    max_mse <- 0
    
    message(paste("Working on:", country))
    
    # Calculate R^2 values for each year and update max p-value and max MSE
    r_squared_values <- sapply(2008:2018, function(year) {
      model <- find_slopes_with_one_country_with_weights(country = country, year = year, weights = weights)$linear
      r_squared <- summary(model)$r.squared
      
      # Update max p-value and max MSE for the country
      max_pv <<- max(max_pv, p_val(model))
      max_mse <<- max(max_mse, MSE(model))
      
      return(r_squared)
    })
    
    # Assign calculated R^2 values, max p-value, and max MSE to the data frame
    dat[i, 1:11] <- r_squared_values
    dat[i, 12] <- max_pv
    dat[i, 13] <- max_mse
  }
  
  # Display the final table
  print(xtable(dat), format.args = list(big.mark = ",", decimal.mark = "."))
  
  return(dat)
}

#calculate_all_with_given_weights(c(60,20,15,70,410,10,170,850))

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Helper function for normalization
normalize_data <- function(data, method) {
  data <- data %>% mutate(across(-GEO, ~ as.numeric(as.character(.))))
  if (method == "Mean") {
    data %>% mutate(across(-GEO, ~ . / mean(., na.rm = TRUE)))
  } else if (method == "MinMax") {
    data %>% mutate(across(-GEO, minMax))
  } else if (method == "Germany") {
    data %>% mutate(across(-GEO, ~ . / .[GEO == "Germany"]))
  } else if (method == "max") {
    data %>% mutate(across(-GEO, ~ . / max(., na.rm = TRUE)))
  } else if (method == 'none'){
    message("Not normalizing")
    return (data)
  } else {
    stop("Invalid normalization method. Choose from 'Mean', 'MinMax', 'Germany', or 'max'.")
  }
}

clustering <- function(normalize = "Mean", minNc = 3, maxNc = 10) {
  # Read and prepare data
  features <- read_data_2()
  # Remove the "Free" column if it exists
  if ("Free" %in% colnames(features)) {
    features <- features %>% select(-Free)
  }
  
  features <- normalize_data(features, normalize)
  
  # Perform clustering using multiple estimators
  clustering_results <- NbClust(features %>% select(-GEO), 
                                distance = "euclidean", 
                                min.nc = minNc, 
                                max.nc = maxNc, 
                                method = "kmeans", 
                                index = "all")
  
  # Print all values from different estimators
  estimators_table <- clustering_results$All.index
  print(xtable(estimators_table, 
               caption = "Estimator Values for Best Number of Clusters", 
               format.args = list(big.mark = ",", decimal.mark = ".")))
  
  # Assign clusters and arrange data by cluster
  features$partition <- clustering_results$Best.partition
  features <- features %>% arrange(partition)
  
  return(features)
}

clustering_per_capita <- function(normalize_method = "max", minNc = 2, maxNc = 10) {
  # Load data
  features <- read_data_2()
  
  # Remove the "Free" column if it exists
  if ("Free" %in% colnames(features)) {
    features <- features %>% select(-Free)
  }
  
  # Convert columns to numeric and calculate per capita values
  features <- features %>%
    mutate(across(c(Total_energy_supply, Verified_emissions, Agriculture, Manufacturing, Industry),
                  ~ as.numeric(as.character(.)) / Population))
  
  # Normalize the data using the specified method
  features <- features %>% rename(GEO = GEO)  # Rename for consistency if needed
  features <- normalize_data(features, method = normalize_method)
  
  # Perform clustering (excluding GEO and Population columns)
  clustering_results <- NbClust(features %>% select(-GEO, -Population), 
                                distance = "euclidean", min.nc = minNc, max.nc = maxNc, 
                                method = "kmeans", index = "all")
  
  # Assign clusters and order data by partition
  features$partition <- clustering_results$Best.partition
  features <- features %>% arrange(partition)
  
  # Print clustering metrics and final features table
  print(xtable(clustering_results$All.index[, 14:26], 
               caption = "Clustering Metrics", 
               format.args = list(big.mark = ",", decimal.mark = ".")))
  print(xtable(features %>% select(GEO, partition, Population), 
               caption = "Per Capita Clustering Results", 
               format.args = list(big.mark = ",", decimal.mark = ".")))
  
  return(features)
}

can_countries_explain_their_own_cluster_2 <-function(){
  # ÎÎ¹Î± Î½Î± ÏÏÎ­Î¾ÎµÎ¹ ÏÏÎ­ÏÎµÎ¹ ÏÏÏÏÎ± Î½Î± Î­ÏÎ¿ÏÎ¼Îµ Î²Î¬Î»ÎµÎ¹ ÏÎ»ÎµÏ ÏÎ¹Ï ÏÏÏÎµÏ Î¾Î±Î½Î¬.
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
  # ÎÏÏÏ ÎµÎ´Ï Î¼ÏÎ¿ÏÎµÎ¯ Î½Î± Î¼Î±Ï Î²ÏÎµÎ¹ ÏÎ¿Î»Ï Î³ÏÎ®Î³Î¿ÏÎ± ÏÎ¿Î¹Î¿ ÎµÎ¯Î½Î±Î¹ ÏÎ¿ Î²Î­Î»ÏÎ¹ÏÏÎ¿ ÏÎµÏ.
  #g1 <- find_the_best_combo_with_one()

  # graph the linear regration
}

can_countries_explain_their_own_cluster <- function(cluster_to_plot = 2) {
  # Perform clustering
  features <- clustering()
  
  # Initialize lists to store features and slopes by cluster
  cluster_features <- list()
  cluster_slopes <- list()
  
  # Loop through clusters and analyze each
  for (cluster in unique(features$partition)) {
    # Subset features for the current cluster
    cluster_features[[cluster]] <- features %>% filter(partition == cluster)
    
    # Temporarily set the list of countries to those in the current cluster
    temp_countries <- list_eur_countries
    list_eur_countries <- cluster_features[[cluster]]$GEO  # Update global country list temporarily
    
    # Find slopes for the current cluster and store results
    cluster_slopes[[cluster]] <- find_slopes_with_one_country()
    
    # Restore the original list of countries
    list_eur_countries <- temp_countries
  }
  
  # Plot results for the selected cluster
  if (cluster_to_plot %in% names(cluster_slopes)) {
    slope_data <- cluster_slopes[[cluster_to_plot]]$data
    plot_title <- paste("Distances from", cluster_slopes[[cluster_to_plot]]$country,
                        "in Cluster", cluster_to_plot, 
                        "| R² =", round(summary(cluster_slopes[[cluster_to_plot]]$linear)$r.squared, 2))
    
   print( ggplot(slope_data, aes(x = df_distance, y = df_free_distance)) + 
      geom_point(aes(color = GEO)) + 
      geom_smooth(method = "lm", se = FALSE) +
      xlab("Distance in Features") + 
      ylab("Distance in Free") +
      labs(title = plot_title))
  } else {
    message("Invalid cluster selected for plotting.")
  }
  
  # Return results for further analysis if needed
  return(list(features = cluster_features, slopes = cluster_slopes))
}


visualize_clustering_results <- function(normalize = "max", minNc = 3, maxNc = 10, use_pca = TRUE) {
  # Run clustering with specified normalization and cluster range
  if (use_pca) {
    # Use PCA-reduced features for clustering
    clustered_data <- clustering_with_pca(normalize = normalize, minNc = minNc, maxNc = maxNc, n_components = 2)
    
    # Perform PCA again for visualization
    pca_results <- prcomp(clustered_data %>% select(-GEO, -partition), scale. = TRUE)
    pca_data <- as.data.frame(pca_results$x[, 1:2])  # Select the first two principal components
  } else {
    # Use original features for clustering
    clustered_data <- clustering(normalize = normalize, minNc = minNc, maxNc = maxNc)
    
    # Perform PCA for visualization only
    pca_results <- prcomp(clustered_data %>% select(-GEO, -partition), scale. = TRUE)
    pca_data <- as.data.frame(pca_results$x[, 1:2])
  }
  
  # Add cluster assignment and country names to the PCA data for plotting
  pca_data$Cluster <- as.factor(clustered_data$partition)  # Cluster assignments
  pca_data$Country <- clustered_data$GEO  # Country names for labeling
  
  # Create scatter plot of the first two principal components colored by cluster
  plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 3) +
    geom_text(aes(label = Country), hjust = 0.5, vjust = 1.5, size = 2, check_overlap = TRUE) +
    labs(title = ifelse(use_pca, "Clustering with PCA Visualization", "Clustering without PCA Visualization"),
         x = "Principal Component 1 (PC1)",
         y = "Principal Component 2 (PC2)",
         color = "Cluster") +
    theme_minimal()
  
  # Print the plot
  print(plot)
  
  # Return the clustered data with cluster assignments
  return(clustered_data)
}

clustering_with_pca <- function(normalize = "max", minNc = 3, maxNc = 10, n_components = 2) {
  # Step 1: Read and normalize the data
  features <- read_data_2()
  
  # Remove the "Free" column if it exists
  if ("Free" %in% colnames(features)) {
    features <- features %>% select(-Free)
  }
  
  features <- normalize_data(features, normalize)
  
  # Step 2: Apply PCA for dimensionality reduction
  pca_results <- prcomp(features %>% select(-GEO), scale. = TRUE)
  
  # Step 3: Select the top `n_components` principal components
  pca_data <- as.data.frame(pca_results$x[, 1:n_components])
  pca_data$GEO <- features$GEO  # Keep GEO (country names) for reference
  
  # Step 4: Perform clustering on the principal components
  clustering_results <- NbClust(pca_data %>% select(-GEO), 
                                distance = "euclidean", 
                                min.nc = minNc, 
                                max.nc = maxNc, 
                                method = "kmeans", 
                                index = "all")
  
  # Step 5: Assign clusters to the original data based on PCA clustering
  features$partition <- clustering_results$Best.partition
  
  # Print the recommended number of clusters by various indices
  estimator_table <- clustering_results$All.index
  print(xtable(estimator_table, 
               caption = "Estimator Values for Best Number of Clusters (Using PCA)", 
               format.args = list(big.mark = ",", decimal.mark = ".")))
  
  # Return the original data with cluster assignments
  return(features)
}

compare_clustering_methods <- function(normalize = "max", minNc = 3, maxNc = 10, n_components = 2) {
  # Perform clustering on the original features
  original_clusters <- clustering(normalize = normalize, minNc = minNc, maxNc = maxNc)
  
  # Perform clustering on the PCA-reduced features
  pca_clusters <- clustering_with_pca(normalize = normalize, minNc = minNc, maxNc = maxNc, n_components = n_components)
  
  # Combine results into a single data frame for comparison
  comparison_df <- original_clusters %>%
    select(GEO, partition) %>%
    rename(Original_Cluster = partition) %>%
    left_join(pca_clusters %>% select(GEO, partition) %>% rename(PCA_Cluster = partition), by = "GEO")
  
  # Plot the differences in clustering between the two methods
  plot <- ggplot(comparison_df, aes(x = Original_Cluster, y = PCA_Cluster, label = GEO)) +
    geom_jitter(width = 0.3, height = 0.3, size = 3, aes(color = Original_Cluster != PCA_Cluster)) +
    geom_text(vjust = -0.5, check_overlap = TRUE) +
    scale_color_manual(values = c("grey", "red"), labels = c("Same", "Different"), name = "Cluster Match") +
    labs(title = "Comparison of Clustering Assignments: Original vs PCA",
         x = "Cluster Assignment (Original Features)",
         y = "Cluster Assignment (PCA-Reduced Features)") +
    theme_minimal()
  
  # Print the plot
  print(plot)
  
  # Return the comparison data frame
  return(comparison_df)
}


features_linear_free <- function(select = 2, attribute = "Population") {
  # Load data (including "Free" and other attributes)
  dat <- read_data_2()
  
  # Ensure the attribute exists in the data
  if (!(attribute %in% colnames(dat))) {
    stop("Specified attribute does not exist in the data.")
  }
  
  # Filter data by the selected cluster
  dat <- dat %>% filter(GEO %in% clusters[[select]])
  
  # Fit linear model to find relationship between the chosen attribute and Free allocation
  formula <- as.formula(paste("Free ~", attribute))
  linear_model <- lm(formula, data = dat)
  r_squared <- summary(linear_model)$r.squared
  
  # Print R-squared value for reference
  print(paste("R-squared for cluster", select, ":", round(r_squared, 4)))
  
  # Plot the relationship with abline
  plot <- ggplot(dat, aes_string(x = attribute, y = "Free", color = "GEO")) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "blue", linetype = "dashed") +
    xlab(attribute) + 
    ylab("Free Allocation") +
    labs(title = paste("Relationship in Cluster", select, "- R² =", round(r_squared, 4))) +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  # Return the linear model object
  return(linear_model)
}

# Function to create and visualize the correlation matrix
plot_correlation_matrix <- function(year) {
  # Read the data
  dat <- read_data_2(year)
  
  # Select numeric columns, excluding the first column (GEO)
  cor.mat <- dat %>%
    dplyr::select(-GEO) %>%
    dplyr::select(where(is.numeric)) %>%
    cor(use = "pairwise.complete.obs")  # Calculates correlations for non-missing pairs
  
  # Plot the correlation matrix
  corrplot(
    cor.mat, type = 'lower', order = 'hclust', 
    tl.col = 'black', tl.srt = 90
  )
}


Fotakis_s <- function() {
  # Perform clustering and load free allocation data
  year_for_comparison <- 2008
  features <- clustering(minNc = 2, maxNc = 4, normalize = "max")
  free <- read_free()
  features <- merge(features, free, by = "GEO")
  
  # Assign descriptive names to clusters
  features <- features %>%
    mutate(partition = case_when(
      partition == 1 ~ "Industrialized",
      partition == 2 ~ "USSR",
      partition == 3 ~ "REST",
      TRUE ~ as.character(partition)
    ))
  
  # Fit linear model to add R² and p-value in title
  linear_model <- lm(Free ~ Verified_emissions, data = features)
  r_squared <- summary(linear_model)$r.squared
  p_value <- summary(linear_model)$coefficients[2, 4]
  
  # Plot with cluster-based colors and simplified legend
  ggplot(features, aes(x = Verified_emissions, y = Free, color = partition)) + 
    geom_point(size = 3) + 
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("Verified Emissions (2008)") + 
    ylab("Free Allocation") +
    labs(
      title = paste("Linear Regression of Verified Emissions vs Free Allocation",
                    "\nR² =", round(r_squared, 4), "| p-value =", round(p_value, 4)),
      color = "Cluster"
    ) +
    scale_color_manual(
      values = c("Industrialized" = "blue", "USSR" = "red", "REST" = "green"),
      breaks = c("Industrialized", "USSR", "REST"),
      labels = c("Industrialized", "USSR", "REST")
    ) +
    theme_minimal() +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 9))
}
#-------------------------------------------------------------------------------------------------------------
Peirama_1 <-function(){
  temp <- read_data_2(year = 2004)
  temp$Phase <- "0"
  temp$year <- 2004
  temp2 <- read_data_2(year = 2005)
  temp2$Phase <- "Phase I"
  temp2$year <- 2005
  temp <- rbind(temp, temp2)
  for (i in 2006:2007){
    temp2 <- read_data_2(year = i)
    temp2$Phase <- "Phase I"
    temp2$year <- i
    temp <- rbind(temp, temp2)
  }
  for (i in 2008:2012){
    temp2 <- read_data_2(year = i)
    temp2$Phase <- "Phase II"
    temp2$year <- i
    temp <- rbind(temp, temp2)
  }
  for (i in 2013:2020){
    temp2 <- read_data_2(year = i)
    temp2$Phase <- "Phase III"
    temp2$year <- i
    temp <- rbind(temp, temp2)
  }
  
  # Find which cluster they belong to from the list "clusters"
  for (i in 1:nrow(temp)){
    for (j in 1:6){
      if (temp$GEO[i] == clusters[[1]][j]){
        temp$partition[i] <- "First"
      }
    }
    for (j in 1:6){
      if (temp$GEO[i] == clusters[[2]][j]){
        temp$partition[i] <- "Second"
      }
    }    
    for (j in 1:13){
      if (temp$GEO[i] == clusters[[3]][j]){
        temp$partition[i] <- "Third"
      }
    }
  }


  temp$actual_agri <- temp$Agriculture*temp$GDPpc*temp$Population
  temp$actual_ind <- temp$Industry*temp$GDPpc*temp$Population
  temp$actual_manu <- temp$Manufacturing*temp$GDPpc*temp$Population
  temp$tot_and_EI <- temp$Total_energy_supply*temp$Energy_Intensity

temp$last_verified <- 0
for (i in 1:nrow(temp)){
  a <- temp[i,]
  for (j in 1:nrow(temp)){
    b <- temp[j,]
    if (a$GEO == b$GEO & a$year == b$year + 1){
      temp$last_verified[i] <- b$Verified_emissions
    }
  }
}
temp[which(temp$GEO == "Greece"),][c(1,2,3,13,19)]

  best <- 0
  ind <- 1
  tem <- temp[which(temp$Phase == "Phase III" & temp$partition =="First" ),]
  


  for (i in 3:ncol(tem)){
    if(i == 12 | i == 13 | i == 14){ 
      next
    }
    jjj<-summary(lm(tem$Free ~ tem[,i]))$r.squared
    if (jjj> best){
      best <- jjj
      ind <- i
    }
    print(names(tem)[i])
    print(jjj)
    rrr[i,4] <- jjj
  }


  
  
  
  summary(lm(tem$Free ~ tem$Verified))
  ggplot(data=tem, aes( y = Free, x = Total_energy_supply))+
    geom_point(aes(color = GEO, alpha = year)) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year))+
    geom_smooth(method = "lm", se = FALSE)
  
  
  #Linear for Paper
  temp$Total_ener_times_EI <- temp$Total_energy_supply*temp$Energy_Intensity
  temp_phaseIII <- temp[which(temp$Phase=="Phase III"),]
  Lin <- lm(temp_phaseIII$Free ~ temp_phaseIII$Population + temp_phaseIII$GDPpc + temp_phaseIII$Total_ener_times_EI)
  summary(Lin)
  theme_set(theme_minimal())
  
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase I")] ~ temp$Verified_emissions[which(temp$Phase=="Phase I")])
  summary(ff)
  ggplot(temp[which(temp$Phase=="Phase I"),], aes(x = Verified_emissions, y = Free))+
    geom_point(aes(color = partition)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("log Verified emissions in  t CO2 equivalent ") + 
    ylab("log Free Allocation in t CO2 equivalent") +
    scale_x_log10()+
    scale_y_log10() +
    labs(title = "1,Phase I", color = "Cluster")
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase II")] ~ temp$Verified_emissions[which(temp$Phase=="Phase II")])
  summary(ff)
  ggplot(temp[which(temp$Phase=="Phase II"),], aes(x = Verified_emissions, y = Free))+
    geom_point(aes(color = partition)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("Verified emissions in  t CO2 equivalent ") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "2,Phase II", color = "Cluster")
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase III")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III")])
  summary(ff)
  ggplot(temp[which(temp$Phase=="Phase III"),], aes(x = Verified_emissions, y = Free))+
    geom_point(aes(color = partition)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("Verified emissions in  t CO2 equivalent ") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "3,Phase III", color = "Cluster")
  
  
    ff <- lm(temp$Free[which(temp$Phase=="Phase III" & temp$partition == "First")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III"& temp$partition == "First")])
  summary(ff)
  f2 <- lm(temp$Free[which(temp$Phase=="Phase III" & temp$partition == "Second")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III"& temp$partition == "Second")])
  summary(f2)
  f3 <- lm(temp$Free[which(temp$Phase=="Phase III" & temp$partition == "Third")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III"& temp$partition == "Third")])
  summary(f3)
  ggplot(temp[which(temp$Phase=="Phase III" & temp$partition=="First"),])+
    geom_point( aes(x = Verified_emissions, y = Free, alpha = year, color = GEO))+ 
    geom_smooth(aes(x = Verified_emissions, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Verified_emissions, y = Free, color = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Verified emissions in  t CO2 equivalent ") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "4,Phase III First Cluster", color = "Country")
  
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = Population, y = Free, color = partition))+ 
    geom_smooth(aes(x = Population, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
  labs(title = "5,year = 2019", color = "Cluster")
  
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$Population[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$Population[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$Population[which(temp$Phase == "Phase III" & temp$partition=="Third")]))
  
  image6 <- ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    xlab("Population") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "6, Phase III", color = "Cluster") 
  
  ggsave(file="6.svg", plot=image6, path = "./Paper4pages/graphs", width = 6)
  
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Population, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Population") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "7, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Population, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Population") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",8 All", color = "Cluster")
  
  
  
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = GDPpc, y = Free, color = partition))+ 
    geom_smooth(aes(x = GDPpc, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "9,year = 2019", color = "Cluster")
  
  
  
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$GDPpc[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$GDPpc[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$GDPpc[which(temp$Phase == "Phase III" & temp$partition=="Third")]))
  
  image10 <-ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = GDPpc, y = Free, color = partition, alpha = year))+ 
        geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year))+
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), 
                linetype="dashed", size=1)+
    xlab("GDPpc") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "10, Phase III", color = "Cluster") 
  ggsave(file="10.svg", plot=image10, path = "./Paper4pages/graphs", width = 6)
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = GDPpc, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = GDPpc, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of GDPpc") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "11, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = GDPpc, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = GDPpc, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of GDPpc") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",12 All", color = "Cluster")
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition))+ 
    geom_smooth(aes(x = Total_energy_supply, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "13,year = 2019", color = "Cluster")
  
  
  
  
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$Total_energy_supply[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$Total_energy_supply[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$Total_energy_supply[which(temp$Phase == "Phase III" & temp$partition=="Third")]))
  
  image14 <- ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year))+
    xlab("Total_energy_supply") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "14, Phase III", color = "Cluster") 
  ggsave(file="14.svg", plot=image14, path = "./Paper4pages/graphs", width = 6)

  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "15, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",16 All", color = "Cluster")
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition))+ 
    geom_smooth(aes(x = Total_energy_supply*Energy_Intensity, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "17,year = 2019", color = "Cluster")
  
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="Third")]))

  
  image18 <- ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), linetype="dashed", size=1)+
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    xlab("Total_energy_supply Energy_Intensity") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "18, Phase III", color = "Cluster") 
  ggsave(file="18.svg", plot=image18, path = "./Paper4pages/graphs", width = 6)
  
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply*Energy_Intensity, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply*Energy_Intensity") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "19, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply*Energy_Intensity, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply*Energy_Intensity") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",20 All", color = "Cluster")
  
  
  tep <- temp[-which(temp$Free == 0),]
  tep <- tep[-which(tep$last_verified == 0),]  
  tep$year <- as.integer(tep$year)
  lm1 <- lm(tep$Free[which(tep$Phase == "Phase I")] ~tep$last_verified[which(tep$Phase == "Phase I")])
  lm2 <- lm(tep$Free[which(tep$Phase == "Phase II")] ~tep$last_verified[which(tep$Phase == "Phase II")])
  lm3 <- lm(tep$Free[which(tep$Phase == "Phase III" & tep$GEO != "Malta")] ~tep$last_verified[which(tep$Phase == "Phase III" & tep$GEO != "Malta")])
  
  summary(lm3)
  my_plot <- ggplot(tep[which(tep$Phase == "Phase III" & tep$GEO != "Malta"),])+
  labs(title = "Phase III of EU ETS")+
    scale_x_log10()+
    scale_y_log10() +
  geom_point( aes(x = last_verified, y = Free, alpha = factor(year)))+
  geom_smooth(aes(x = last_verified, y = Free),method = "lm", se = FALSE, color = "black", size =0.5)+
  geom_line(aes(x = last_verified, y = Free, group = GEO, alpha = factor(year)))+
    scale_alpha_discrete(name = "Year")+
    annotate(
      "text",
      x = max(tep$last_verified),  # Set x to the maximum x-value
      y = min(tep$Free),  # Set y to the minimum y-value
      label = "Multiple R-squared:  0.9058
      Adjusted R-squared:  0.9053
      p-value: < 2.2e-16",
      vjust = 0,  # Adjust vertical alignment
      hjust = 1   # Adjust horizontal alignment
    )+
    xlab("Last year's verified emissions (log)") + 
    ylab("Free Allocation in t CO2 equivalent (log)") +
    theme(panel.background = element_rect(fill = rgb(220/255, 220/255, 220/255)),
          axis.title.x = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"),
          axis.title.y = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"),
          title = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"))
  ggsave(filename = "./4σέλιδο/Phase_III.svg", plot = my_plot, device = "svg")
  
  
   
  ggplot(temp[which(temp$Phase == "Phase I"),])+
    labs(title = "Presentation 2", color = "Cluster")+
    geom_point( aes(x = Verified_emissions, y = Free, color = partition, alpha = year))+
    geom_smooth(aes(x = Verified_emissions, y = Free),method = "lm", se = FALSE, color = "black", size =0.5)+
    geom_line(aes(x = Verified_emissions, y = Free, color = partition, group = GEO, alpha = year))
  
}

Kosta_eisai_vlakas_grapse_to_lp <- function(){
  #create an LP to solve the following problem:
  # max \sum v_{ij}   (GDP_{ij} / verified emissions)_{ij} )  (GDP_{i} correction to PPS currency unit) 
  # s.t. \sum v_{ij} = 1
  #a_1 free(t-1) \leq \sum v_i \leq a_2 free(t-1)
  #a_1 free(t-1) \leq \sum v_j \leq a_2 free(t-1)
  #v_i = \sum_j v_{ij}
  #v_i \approx p_i / Pop_total
  #v_{ij} / v_i \approx GDP_{ij} / GDP_i 
  #Nash welfare \max \product_i v_i
  #v_{ij} = fraction of total free cap of country i in sector j 
  #v_i = fraction of gree in country j
  #v_j = fraction of free in sector j
  #v_{ij} / v_i \approx (sector correction factor) * GDP_{ij} / GDP_i 

  
  # #example LP

  # # Set coefficients of the objective function (max 5x_1 + 7x_2)
  # f.obj <- c(5, 7)
  
  # # Set matrix corresponding to coefficients of constraints by rows
  # # Do not consider the non-negative constraint; it is automatically assumed
  # f.con <- matrix(c(1, 0,
  #                   2, 3,
  #                   1, 1), nrow = 3, byrow = TRUE)
  
  # # Set unequality signs
  # f.dir <- c("<=",
  #            "<=",
  #            "<=")
  
  # # Set right hand side coefficients
  # f.rhs <- c(16,
  #            19,
  #            8)
  
  # # Final value (z)
  # lp("max", f.obj, f.con, f.dir, f.rhs)
  
  # # Variables final values
  # lp("max", f.obj, f.con, f.dir, f.rhs)$solution
  
  # # Sensitivities
  # lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
  # lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to
  
  # # Dual Values (first dual of the constraints and then dual of the variables)
  # # Duals of the constraints and variables are mixed
  # lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals
  
  # # Duals lower and upper limits
  # lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
  # lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to
  will_use_a1 <- TRUE
  will_use_a2 <- TRUE
  will_use_pop <- TRUE
  
  a_1 <- 0.5 # Συντελεστής κάτω ορίου ως προς τα περσινά free
  a_2 <- 2 # Συντελεστής ανω ορίου ως προς τα περσινά free
  a_3 <- 0.5 # Συντελεστής κάτω ορίου ως προς το συντελεστή πληθυσμού
  a_4 <- 2 # Συντελεστής κάτω ορίου ως προς το συντελεστή πληθυσμού
  # free / total free -> 10%
  # > a_1* 10% -> 9%
  # < a_2* 10% -> 11%
  # pop / total pop -> 14%
  # >7%
  # <28%
  


  df_year <- read_data_2(year = year_for_comparison)
   df_next_year<- read_data_2(year = year_for_comparison+1)
   df_year <-df_year[-c(17),] #Remove Malta
   df_next_year <-df_next_year[-c(17),]
  df_year$Free <- df_year$Free / sum(df_year$Free)
  df_next_year$Free <- df_next_year$Free / sum(df_next_year$Free)
  df_year$Pop_norm <- df_year$Population / sum(df_year$Population)
  df_next_year$Pop_norm <- df_next_year$Population / sum(df_next_year$Population)
  # Let's read the data from GDP per capita PPS 
  GDPpps <- read.csv(paste0(data_path,"/tec00114_linear.csv"), header = TRUE, sep = ",")
  GDPpps <- GDPpps[-c(1,2,3,4,5,9)]
  # convert eu 2letter abbriviation to country name
  eu_2l_name <- data.frame(eu_2l = c("AL", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"), 
                           eu_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom"))

  GDPpps <- GDPpps[-which(GDPpps$geo %in% c("BA", "CH", "EA19", "EA20", "EU27_2007","EU27_2020","EU28", "IS", "JP", "ME","MK","NO","RS","TR","US" )),]
  for (i in 1:nrow(GDPpps)){
    GDPpps$geo[i] <- eu_2l_name$eu_name[which(eu_2l_name$eu_2l == GDPpps$geo[i])]
  }
  GDPpps <- GDPpps[which(GDPpps$TIME_PERIOD == year_for_comparison),]
  GDPpps <- GDPpps[-c(2)]
  names(GDPpps) <- c("GEO", "GDPpps")
  df_year <- merge(df_year, GDPpps, by = "GEO")

  f.obj <- c(df_year$GDPpps*df_year$Population/df_year$Verified_emissions*df_year$Industry/100)

  # repeat 1 for each row
  f.con <- matrix( rep(1, nrow(df_year)), nrow = 1, byrow = TRUE)
  f.dir <- c("=")
  f.rhs <- c(1)
  sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution

  if (will_use_a1){
    f.con <- rbind(f.con, diag(nrow(df_year)))
    f.dir <- c(f.dir, rep(">=", nrow(df_year)))
    f.rhs <- c(f.rhs, a_1*df_year$Free)
  }
  if (will_use_a2){
    f.con <- rbind(f.con, diag(nrow(df_year)))
    f.dir <- c(f.dir, rep("<=", nrow(df_year)))
    f.rhs <- c(f.rhs, a_2*df_year$Free)
  }
  if (will_use_pop){
    f.con <- rbind(f.con, diag(nrow(df_year)))
    f.dir <- c(f.dir, rep(">=", nrow(df_year)))
    f.rhs <- c(f.rhs, a_3*df_year$Pop_norm)
    f.con <- rbind(f.con, diag(nrow(df_year)))
    f.dir <- c(f.dir, rep("<=", nrow(df_year)))
    f.rhs <- c(f.rhs, a_4*df_year$Pop_norm)
  }






  sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution

  gg <- data.frame(Country =  df_year$GEO[1], efficiency =  (df_year$GDPpps[1]*df_year$Population[1]/df_year$Verified_emissions[1]*df_year$Industry[1]/100), last_year = df_year$Free[1], low_free = a_1*df_year$Free[1] ,up_free = a_2*df_year$Free[1], pop = df_year$Pop_norm[1], min = df_year$Pop_norm[1]*a_3, max = df_year$Pop_norm[1]*a_4, forecasted =  sol[1], change = paste((sol[1]-df_year$Free[1])/df_year$Free[1]*100, "%"))
  for (i in 2:nrow(df_year)){
    gg <- rbind(gg, data.frame(Country =  df_year$GEO[i], efficiency =  (df_year$GDPpps[i]*df_year$Population[i]/df_year$Verified_emissions[i]*df_year$Industry[i]/100), last_year = df_year$Free[i], low_free = a_1*df_year$Free[i] ,up_free = a_2*df_year$Free[i], pop = df_year$Pop_norm[i], min = df_year$Pop_norm[i]*a_3, max = df_year$Pop_norm[i]*a_4, forecasted =  sol[i], change = paste((sol[i]-df_year$Free[i])/df_year$Free[i]*100, "%")))
  }
  gg <-gg[order(gg$efficiency, decreasing = TRUE),]
  xtable(gg, caption = "GDP per capita PPS", label = "tab:GDPpps", digits = 4, include.rownames = FALSE, booktabs = TRUE, floating = TRUE, file = "GDPpps.tex")

  # LEt's create the same thing with slitghly different contraints
  df_year$b <- df_year$GDPpps/mean(df_year$GDPpps)
  e <- 0.5
   #Now we will use the b parameter to change the constraints
  f.con <- matrix( rep(1, nrow(df_year)), nrow = 1, byrow = TRUE)
  f.dir <- c("=")
  f.rhs <- c(1)


  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  for (i in 1:nrow(df_year)){
    f.rhs <- c(f.rhs, min(1-e, 1/df_year$b[i])*df_year$Free[i])
  }
    
  
  a_3 <- 0.5 # Συντελεστής κάτω ορίου ως προς το συντελεστή πληθυσμού
  a_4 <- 2
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  for (i in 1:nrow(df_year)){
    f.rhs <- c(f.rhs, max(1+e, 1/df_year$b[i])*df_year$Free[i])
  }
  if (will_use_pop){
    f.con <- rbind(f.con, diag(nrow(df_year)))
    f.dir <- c(f.dir, rep(">=", nrow(df_year)))
    f.rhs <- c(f.rhs, a_3*df_year$Pop_norm)
    f.con <- rbind(f.con, diag(nrow(df_year)))
    f.dir <- c(f.dir, rep("<=", nrow(df_year)))
    f.rhs <- c(f.rhs, a_4*df_year$Pop_norm)
  }
  sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution
  gg2 <- data.frame(Country =  df_year$GEO[1], efficiency =  (df_year$GDPpps[1]*df_year$Population[1]/df_year$Verified_emissions[1]*df_year$Industry[1]/100), last_year = df_year$Free[1], low_free = a_1*df_year$Free[1] ,up_free = a_2*df_year$Free[1], pop = df_year$Pop_norm[1], min = df_year$Pop_norm[1]*a_3, max = df_year$Pop_norm[1]*a_4, forecasted =  sol[1], change = paste((sol[1]-df_year$Free[1])/df_year$Free[1]*100, "%"))
  for (i in 2:nrow(df_year)){
    gg2 <- rbind(gg2, data.frame(Country =  df_year$GEO[i], efficiency =  (df_year$GDPpps[i]*df_year$Population[i]/df_year$Verified_emissions[i]*df_year$Industry[i]/100), last_year = df_year$Free[i], low_free = a_1*df_year$Free[i] ,up_free = a_2*df_year$Free[i], pop = df_year$Pop_norm[i], min = df_year$Pop_norm[i]*a_3, max = df_year$Pop_norm[i]*a_4, forecasted =  sol[i], change = paste((sol[i]-df_year$Free[i])/df_year$Free[i]*100, "%")))

    
    }
  gg2 <-gg2[order(gg2$efficiency, decreasing = TRUE),]
  xtable(gg, caption = "GDP per capita PPS", label = "tab:GDPpps", digits = 4, include.rownames = FALSE, booktabs = TRUE, floating = TRUE, file = "GDPpps.tex")


}


new_distances <- function(year_for_comparison, country = "Hungary"){
  country <- "Hungary"
  data <- read_data_2(year = year_for_comparison)
  for (i in 2:ncol(data)){
    data[,i] <- minMax(data[,i])
  }
  midle <- data[which(data$GEO == country),]
  data <- data[-which(data$GEO == country),]
  for (i in 1:nrow(data)){
    data$actual_distance[i] <- sqrt((data$Verified_emissions[i] - midle$Verified_emissions)^2 + (data$Population[i] - midle$Population)^2 + (data$Total_energy_supply[i] - midle$Total_energy_supply)^2 +(data$Agriculture[i] - midle$Agriculture)^2 + (data$Industry[i] - midle$Industry)^2 + (data$Manufacturing[i] - midle$Manufacturing)^2 + (data$Energy_Intensity[i] - midle$Energy_Intensi)^2 + (data$Inflation[i] - midle$Inflation)^2 )
    data$distance_Free[i] <- abs(data$Free[i] - midle$Free)
  }
  gg <- lm(data$actual_distance ~ data$distance_Free)
  #png("distance.png", width = 10, height = 10, units = "in", res = 300)
  ggplot(data, aes(x = actual_distance, y = distance_Free)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("Distance between countries") + 
  xlab("Distance between countries in 2018") + ylab("Distance between countries in 2019")
  ggsave("gg.png")
  summary(gg)
}

check_the_proxy_energy_intensity <- function(){
  # Load Energy Balance data from csv file
  # Path: Data
  # File: nrg_ind_ren_linear.csv
  # Source: Eurostat
  # Data tree :  Detailed datasets -> Energy (nrg) -> Energy statistics - quantities (nrg_quant) -> Energy statistics - quantities, annual data (nrg_quanta) -> Share of energy from renewable sources (nrg_ind_share) -> Share of energy from renewable sources (nrg_ind_ren)	 
  # Data name on Eurostat : Share of energy from renewable sources 
  # Country: All countries
  # Year: 2004 - 2020
  # Unit: Percentage
  # nrg_ind_ren codes:
  # Renewable energy sources    		                  -> REN
  # Renewable energy sources in transport           	-> REN_TRA
  # Renewable energy sources in electricity           -> REN_ELC
  # Renewable energy sources in heating and cooling   -> REN_HEAT_CL
  # Total energy supply             -> NRGSUP
  # Available for final consumption -> AFC
  green_percent <- read.csv(paste0(data_path,"nrg_ind_ren_linear.csv"))
  green_percent <- green_percent[which(green_percent$nrg_bal=="REN"),]
  green_percent <- green_percent[which (green_percent$TIME_PERIOD == year_for_comparison),]
  green_percent <- green_percent[-c(1,2,3,4,5,7,9)]
  
  other_data <- read_data_2()
  eu_2l_name <- data.frame(eu_2l = c("AL", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"), 
                           eu_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom"))

  green_percent <- green_percent[ green_percent$geo %in% eu_2l_name$eu_2l,] # vgale ta perierga
  for (i in 1:nrow(green_percent)){
    green_percent$geo[i] <- eu_2l_name$eu_name[which(eu_2l_name$eu_2l == green_percent$geo[i])]
  }
  colnames(green_percent) <- c("GEO", "green_per" )
  dat <- merge(other_data, green_percent, by = "GEO")
  dat$Calculated_EI <- dat$Total_energy_supply / dat$GDPpc / dat$Population
  dat$Calculated_rev_EI <- dat$Verified_emissions / (100-dat$green_per) / dat$GDPpc / dat$Population
  for (i in 1:nrow(dat)){
    print(dat$GEO[i])
    print (format(dat$GDPpc[i]*dat$Population[i]/1000, big.mark = ","))
  }
  
  
  ggplot(data = dat, aes(x = Energy_Intensity, y = Calculated_EI)) +
    geom_point(aes(color = GEO))+
    geom_smooth(method = "lm", se = FALSE)
  gg <- lm( dat$Calculated_EI ~ dat$Energy_Intensity)
  summary(gg)
}

library(shiny)

filter_dataframe <- function(data, column) {
  # Launch Shiny app
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "selected_values", label = "Select values:", choices = unique(data[[column]]), multiple = TRUE),
          actionButton(inputId = "submit_button", label = "Submit")
        ),
        mainPanel(
          tableOutput(outputId = "filtered_data_table")
        )
      )
    ),
    server = function(input, output) {
      # Filter dataframe based on selected values
      filtered_data <- reactive({
        data[data[[column]] %in% input$selected_values, ]
      })
      
      # Render filtered data as a table
      output$filtered_data_table <- renderTable({
        filtered_data()
      })
    }
  )
}



create_second_proxy_for_energy_intensity <- function(){
  # First let's get the surrendered values from the ETS, seperated by installation type. 
  # Load Energy Balance data from csv file
  # Path: Data
  # File: ETS_Database_v50_Apr23.csv
  # Source: Eurostat
  # Data link :  https://www.eea.europa.eu/data-and-maps/data/european-union-emissions-trading-scheme-17  # Country: All countries
  # Year: 2005 - 2023
  # Unit: tonne of CO2 equ.


# Set the file path to your downloaded CSV file
file_path <- paste0(data_path,"ETS_Database_v50_Apr23.csv")
text <- read.csv(file_path, sep = "\t", header = TRUE) # I hate \t seperated shit.

#filter_dataframe(text, "ETS.information")
print(unique(text[["ETS.information"]]))
text <- text[which(text$ETS.information == "4. Total surrendered units"),]
print(unique(text[["main.activity.sector.name"]]))
text <- text[which(text$main.activity.sector.name == "21-99 All industrial installations (excl. combustion)"),]
text <- text[which(text$year == year_for_comparison),]
View(text)

# Read GDP per capita pps
#eurostat dataset 
gdpps <- read.csv(paste0(data_path,"sdg_10_10__custom_6312863_linear.csv"))[-c(1,2,3,4,5,6,10)]
gdpps <- gdpps[which(gdpps$TIME_PERIOD == year_for_comparison),][-c(2)]
eu_2l_name <- data.frame(eu_2l = c("AL", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"), 
                         eu_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom"))
gdpps <- gdpps[ gdpps$geo %in% eu_2l_name$eu_2l,]
for (i in 1:nrow(green_percent)){
  gdpps$geo[i] <- eu_2l_name$eu_name[which(eu_2l_name$eu_2l == gdpps$geo[i])]
}
colnames(gdpps) <- c("GEO", "GDPpps")


# Read green Percentage
green_percent <- read.csv(paste0(data_path,"nrg_ind_ren_linear.csv"))
green_percent <- green_percent[which(green_percent$nrg_bal=="REN"),]
green_percent <- green_percent[which (green_percent$TIME_PERIOD == year_for_comparison),]
green_percent <- green_percent[-c(1,2,3,4,5,7,9)]

other_data <- read_data_2()
eu_2l_name <- data.frame(eu_2l = c("AL", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"), 
                         eu_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom"))

green_percent <- green_percent[ green_percent$geo %in% eu_2l_name$eu_2l,] # vgale ta perierga
text <- text[text$country %in% eu_2l_name$eu_name,]
text <- text[-c(2,3,4,5,7)]

for (i in 1:nrow(green_percent)){
  green_percent$geo[i] <- eu_2l_name$eu_name[which(eu_2l_name$eu_2l == green_percent$geo[i])]
}
colnames(green_percent) <- c("GEO", "green_per" )
colnames(text) <- c("GEO", "Exl.Comb")
dat <- merge(other_data, green_percent, by = "GEO")
dat <- merge(dat, text, by = "GEO")
dat <- merge(dat, gdpps, by = "GEO")
dat <- dat[- which(dat$GEO %in% c("Finland", "Lithuania", "Austria") ),]
dat$Calculated_EI <- dat$Exl.Comb / dat$GDPpc / dat$Population
dat$Calculated_rev_EI <- dat$Exl.Comb / (100-dat$green_per) / dat$GDPpps / dat$Population



ggplot(data = dat, aes(x = Energy_Intensity, y = Calculated_rev_EI)) +
  geom_point(aes(color = GEO))+
  geom_smooth(method = "lm", se = FALSE)
gg <- lm( dat$Calculated_rev_EI ~ dat$Energy_Intensity)
summary(gg)
}

# SELECT SUM(trnew.NbOfUnits), concat(mat.kwdikos , ' ' , mat.onoma) as ActivityType
# FROM transactions_new AS trnew
# JOIN eutl_accountholders AS ah ON trnew.TransferringAccountHolder = ah.holderName
# JOIN eutl_accounts AS a ON ah.rawCode = a.rawCode
# JOIN eutl_installations_orair AS io ON io.account = a.rawCode
# JOIN mainactivitytype AS mat ON io.mainActivity = mat.kwdikos
# WHERE trnew.TransactionType LIKE '%%-2' AND YEAR(trnew.TransactionDate) = 2016
# GROUP BY mat.onoma
# ORDER BY mat.kwdikos

dummy_function_for_graph_for_presentation <- function(){
  dat <- read.csv(file = paste0(data_path,"sdg_13_10_page_linear_for_small_graph.csv"),
                    header = TRUE,
                    as.is = TRUE)
  dat <- dat[-c(1,2,3,4,5,6,7,10)]
  dat <- dat[which(dat[1] > 2003),]
  dat$OBS_VALUE <- dat$OBS_VALUE/dat$OBS_VALUE[1]*100
  my_plot <- ggplot(dat, aes(x = TIME_PERIOD, y = OBS_VALUE))+
    geom_rect(
      xmin = -Inf, xmax = 2005, ymin = -Inf, ymax = Inf,
      fill = "white", alpha = 0.09)+
    geom_rect(
      xmin = 2005, xmax = 2008, ymin = -Inf, ymax = Inf,
      fill = rgb(1,0.8,0.8), alpha = 0.09)+
    geom_rect(
      xmin = 2008, xmax = 2013, ymin = -Inf, ymax = Inf,
      fill = rgb(0.8,1,0.8), alpha = 0.09)+
    geom_rect(
      xmin = 2013, xmax = 2021, ymin = -Inf, ymax = Inf,
      fill = rgb(0.8,0.8,1), alpha = 0.09)+
    geom_point(color = rgb(50/255, 50/255, 50/255)) +
       xlab("Year") + 
    ylab(bquote("Net greenhouse gas emissions"[(source: EEA)])) +
     labs(title = "GHG emissions on EU", color = "Cluster")+ 
    theme(panel.background = element_rect(fill = rgb(220/255, 220/255, 220/255)),
          axis.title.x = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"),
          axis.title.y = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"),
          title = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"))+
    geom_vline(xintercept = 2005, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 2008, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 2013, linetype="dotted", size = 0.3) +
    geom_text(aes(x=2008, label="\nPHASE II", y=75), colour="black", angle=90) +
    geom_text(aes(x=2013, label="\nPHASE III", y=75), colour="black", angle=90) +
    geom_text(aes(x=2005, label="\nPHASE I", y=75), colour="black", angle=90)
  ggsave(filename = "./4σέλιδο/total GHG Emissions.svg", plot = my_plot, device = "svg")
}
