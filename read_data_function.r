will_use_log <- TRUE
year_for_comparison <- 2012
will_use_total_energy_supply <- TRUE
will_use_inflation <- TRUE
will_use_GDPpc <- TRUE
will_use_population <- TRUE
will_use_verified_emisions <- TRUE
will_use_agriculture <- TRUE
will_use_industry <- TRUE
will_use_manufacturing <- TRUE
will_normalise <- TRUE
force_fresh_data <- FALSE
# read_data <- function() {
    # Try to find the file in the folder "Data/created_csvs"
    # If it is not found, create it
    text <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
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

    if (!force_fresh_data & file.exists(paste("./Data/created_csvs/",text, sep = ""))) {
        print ("Using Cached Data")
        return(read.csv(file = paste("./Data/created_csvs/",text, sep = ""),
                        header = TRUE))
    }

    library("xlsx")
    library("xtable") # Load xtable package
    library("stringr")
    library("DBI")
    library("RMySQL")
    df_1 <- data.frame()
    list_eur_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovenia", "Spain", "Sweden","United Kingdom")
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

    if (will_use_total_energy_supply) {
        # Load Energy Balance data from csv file
        # Path: Data
        # File: nrg_bal_s_1_Data.csv
        # Source: http://wdi.worldbank.org/table/4.2
        # Data: Energy balance
        # Country: All countries
        # Year: 2011 - 2020
        # Unit: Thousand tonnes of oil equivalent

        d <- read.csv(file = "./Data/nrg_bal_s_1_Data.csv",
                            header = TRUE)
        d <- d[-c(4, 5, 7)]
        df_total_energy_supply <- subset(d, d$NRG_BAL == "Total energy supply")
        df_total_energy_supply$GEO[which(df_total_energy_supply$GEO == "Germany (until 1990 former territory of the FRG)")] <- "Germany"
        for (i in 1:nrow(df_1)) {
            print(df_1$"GEO"[i])
            df_1$"Total_energy_supply"[i] <- df_total_energy_supply$"Value"[which(df_total_energy_supply$TIME == year_for_comparison & df_total_energy_supply$GEO == df_1$"GEO"[i])]
        }
        df_1$Total_energy_supply <- as.numeric(gsub(",", "", df_1$Total_energy_supply))

    }

    if (will_use_population){
        # Open population_2011_2021.tsv file and read it
        # Path: Data
        # File: population_2011_2021.tsv
        # Source: https://data.worldbank.org/indicator/SP.POP.TOTL
        # Data: Population, total
        # Country: All countries
        # Year: 2011 - 2021
        # Unit: Persons
        # Headers are on the 1st row
        df_population <- read.csv(file = "./Data/population_2011_2021.tsv",
                            sep = "\t",
                            header = TRUE,
                            as.is = TRUE)

        #delete first 6 character from the first column
        df_population[,1] <- gsub(" ", "", df_population[,1])
        df_population[,1] <- substr(df_population[,1], 7, nchar(df_population[,1]))
        df_population <- df_population[,c(1,which(colnames(df_population) == paste("X", year_for_comparison, sep="" )))]
        # edo isos egine akoma pio ilithia to diavasma


        colnames(df_population) <- c("GEO_2LABBR", year_for_comparison)
        # Find the number in each entry of column X2015 and remove the rest
        df_population[,2] <- gsub("[^0-9]", "", df_population[,2])
        #Omit NA values
        df_population <- df_population[!is.na(df_population[,2]),]
        df_population[,2] <- as.numeric(df_population[,2])
        # Replace 2 letter country code with country name
        df_population$"GEO_2LABBR" <- gsub("AL", "Albania", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("AT", "Austria", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("BA", "Bosnia and Herzegovina", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("BE", "Belgium", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("BG", "Bulgaria", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("CH", "Switzerland", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("CY", "Cyprus", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("CZ", "Czechia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("DE", "Germany", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("DK", "Denmark", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("EE", "Estonia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("ES", "Spain", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("FI", "Finland", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("FR", "France", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("GE", "Georgia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("GB", "United Kingdom", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("EL", "Greece", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("HR", "Croatia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("HU", "Hungary", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("IE", "Ireland", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("IS", "Iceland", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("IT", "Italy", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("LT", "Lithuania", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("LU", "Luxembourg", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("LV", "Latvia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("ME", "Montenegro", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("MK", "North Macedonia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("MT", "Malta", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("NL", "Netherlands", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("NO", "Norway", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("PL", "Poland", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("PT", "Portugal", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("RO", "Romania", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("RS", "Serbia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("SE", "Sweden", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("SI", "Slovenia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("SK", "Slovakia", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("TR", "Turkey", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("UA", "Ukraine", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("UK", "United Kingdom", df_population$"GEO_2LABBR")
        df_population$"GEO_2LABBR" <- gsub("XK", "Kosovo", df_population$"GEO_2LABBR")
        # Omit non common "SM" (San Marino), "MD" (MaryLand), "MC" (Monaco), "VA" (Vatican City), "LI" (Liechtenstein), "BY", "EA18", "EA19"
        df_population <- df_population[df_population$"GEO_2LABBR" != "SM",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "MD",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "MC",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "VA",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "LI",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "BY",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "EA18",]
        df_population <- df_population[df_population$"GEO_2LABBR" != "EA19",]
        colnames(df_population) <- c("GEO", "Population")

        for (i in 1:nrow(df_1)){
            df_1$Population[i] <- as.integer(df_population$Population[which(df_population$GEO == df_1$GEO[i])])
        }
    }

    if (will_use_verified_emisions){
        # Taking data drom the database as well

        # DEFINITIONS FOR DATABASE
        db <- "eu_ets"           # name of database
        use <- "root"           # user name
        passwor <- ""     # password
        hos <- "localhost"       # host name


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

        ##################################
        # include verified emissions data
        ##################################

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
        colnames(df_verified_emisions) <- c("GEO", "verified_emisions")


        # Merge the two dataframes
        for (i in 1:nrow(df_1)){
            df_1$Verified_emissions[i] <- as.integer(df_verified_emisions$verified_emisions[which(df_verified_emisions$GEO == df_1$GEO[i])])
        }
    }

    if (will_use_agriculture | will_use_industry | will_use_manufacturing){
        # import data from World Bank
        # Path: Data
        # File: 4.2_Structure_of_value_added.csv
        # Source: http://wdi.worldbank.org/table/4.2
        # Data: GDP, Argicalture, Industry, Manufacturing, Services
        # Country: All countries
        # Year: 2010 - 2020
        # Unit: Billions USD and percentage
        # Opened teh excel file on microsoft excel and coverted it into csv file AND CALCULATED BY HAND BULAGRIA MANUFACTURING 2020
        # We will only use the 2020 data

        my_data <- read.csv(file = "./Data/4.2_Structure_of_value_added.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)
        my_data <- my_data[5:230,] # Omit usesless info at the bottom
        my_data <- my_data[, -c(2,4,6,8,10 )] #Omit 2010
        colnames(my_data) <- c("GEO","GDP", "Agriculture", "Industry",  "Manufacturing", "Services")

        # Delete all rows from my_data whose GEO is not in df_1
        my_data <- my_data[my_data$"GEO" %in% df_1$"GEO",]
        my_data$GDP <- gsub(",", "", my_data$GDP)

        my_data$GDP <- as.numeric(my_data$GDP)
        my_data$Agriculture <- as.numeric(my_data$Agriculture)
        my_data$Industry <- as.numeric(my_data$Industry)
        my_data$Manufacturing <- as.numeric(my_data$Manufacturing)
        # Percentage to actual number in billiosn USD
        for (i in 1:nrow(my_data)){
            my_data[i,3] <- my_data[i,2] * my_data[i,3] / 100
            my_data[i,4] <- my_data[i,2] * my_data[i,4] / 100
            my_data[i,5] <- my_data[i,2] * my_data[i,5] / 100
        }
        # We don't care about the GDP anymore nor the services
        my_data <- my_data[, -c(2,6,7)]
    }
    if (will_use_agriculture){
        for (i in 1:nrow(df_1)){
            df_1$Agriculture[i] <- as.numeric(my_data$Agriculture[which(my_data$GEO == df_1$GEO[i])])
        }
    }
    if (will_use_industry){
        for (i in 1:nrow(df_1)){
            df_1$Industry[i] <- as.numeric(my_data$Industry[which(my_data$GEO == df_1$GEO[i])])
        }
    }
    if (will_use_manufacturing){
        for (i in 1:nrow(df_1)){
            df_1$Manufacturing[i] <- as.numeric(my_data$Manufacturing[which(my_data$GEO == df_1$GEO[i])])
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


        df_1$Total_energy_supply <- df_1$Total_energy_supply / max_total_energy_supply
        df_1$Inflation <- df_1$Inflation / max_inflation
        df_1$GDPpc <- df_1$GDPpc / max_GDPpc
        df_1$Population <- df_1$Population / max_population
        df_1$Verified_emissions <- df_1$Verified_emissions / max_verified_emissions
        df_1$Agriculture <- df_1$Agriculture / max_Agriculture
        df_1$Industry <- df_1$Industry / max_Industry
        df_1$Manufacturing <- df_1$Manufacturing / max_Manufacturing
    }

    
    #save csv file of df_1
    # text <- paste("df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),
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
    # write.csv(df_1, file = paste("./Data/created_csvs/",text, sep = "" ), row.names = TRUE)
    # return(df_1)
# }
# read_data()
