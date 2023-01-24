library("xlsx")
library("xtable") # Load xtable package
library("stringr")
library("DBI")
library("RMySQL")

read_data <- function(year = 0) {
  source("config.R")
  if(year != 0) {year_for_comparison <- year}
  information_text <- list()
    

    
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

    if (!force_fresh_data & file.exists(paste("./Data/created_csvs/",text_s, sep = ""))) {
        print ("Using Cached Data")
        return(read.csv(file = paste("./Data/created_csvs/",text_s, sep = ""),
                        header = TRUE))
    }


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
        for (i in 1:nrow(df_1)){
          GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
          df_1$Agriculture[i] <- as.numeric(buffer_Agriculture[buffer_Agriculture$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100

        }
    }
    if (will_use_industry){
        for (i in 1:nrow(df_1)){
          GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
          df_1$Industry[i] <- as.numeric(buffer_Industry[buffer_Industry$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100
        }
    }
    if (will_use_manufacturing){
        for (i in 1:nrow(df_1)){
          if (df_1$GEO[i] == "Bulgaria"){
            # Estimating Bulgaria's manufacturing value added from the rest of the GDP components
            df_1$Manufacturing[i] <- df_1$GDP[i] - (as.numeric(buffer_Agriculture[buffer_Agriculture$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100) - (as.numeric(buffer_Industry[buffer_Industry$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100) - (as.numeric(buffer_Services[buffer_Services$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100)
            next
          }
          GDP_multiplier <- as.numeric(buffer_GDP[buffer_GDP$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")])
          df_1$Manufacturing[i] <- as.numeric(buffer_Manufacturing[buffer_Manufacturing$"Country.Name" == df_1$GEO[i],paste("X",year_for_comparison, "..YR", year_for_comparison,".", sep = "")]) * GDP_multiplier / 100
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
    
    #save csv file of df_1
    # text_t <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
                        # (if(will_normalise) "_norm" else ""),
                        # (if(will_use_total_energy_supply) "_tes" else ""),
                        # (if(will_use_verified_emisions) "_ve" else ""),
                        # (if(will_use_GDPpc) "_gdp" else ""),
                        # (if(will_use_population) "_pop" else ""),
                        # (if(will_use_inflation) "_inf" else ""),
                        # (if(will_use_agriculture) "_agr" else ""),
                        # (if(will_use_industry) "_ind" else ""),
                        # (if(will_use_manufacturing) "_man" else ""),
                        # ".csv", sep = "")
    # write.csv(df_1, file = paste("./Data/created_csvs/",text_t, sep = "" ), row.names = TRUE)
    return(df_1)
}


read_free <- function(year = 0){
  if(year != 0) {year_for_comparison <- year}
  source("config.R")
  library("xlsx")
  library("xtable") # Load xtable package
  library("stringr")
  library("DBI")
  library("RMySQL")

  
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


  
  return (df_free)
}

run_test <- function(j,l){
  for (i in j:l){
    print(i)
    x <- read_data()
    err = FALSE
    if (dim(x)[1] != 25 & dim(x)[2] != 9){
      print("ERROR, wrong dimensions")
      err = TRUE
    }
    if (sum(is.na(x)) != 0){
      print("ERROR, missing data")
      err = TRUE
    }
    if (err == FALSE){
      print("OK")
    }    
  }
}

# run_test(2002,2018)
