will_use_log <- TRUE
year_for_comparison <- 2015
will_use_total_energy_supply <- TRUE
will_use_inflation <- TRUE
will_use_GDPpc <- TRUE
will_use_population <- TRUE
will_use_verified_emisions <- TRUE
will_use_Agriculture <- TRUE
will_use_Industry <- TRUE
will_use_Manufacturing <- TRUE
will_normalise <- TRUE

read_data <- function() {
    library("xlsx")
    library("xtable") # Load xtable package
    library("stringr")
    library("DBI")
    library("RMySQL")
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
    df_Inflation <- read.csv(file = "./Data/Inflation_1960_2021.csv",
                        skip = 4, header = FALSE)
    colnames(df_Inflation) <- headers

    # Create new dataframe, where colnames are the country names
    # and the rows are the years
    df_Inflation <- subset(df_Inflation, select = -c(2, 3, 4))
    df_Inflation <- t(df_Inflation)
    colnames(df_Inflation) <- df_Inflation[1, ]
    df_Inflation <- df_Inflation[-1, ]

    # Load Energy Balance data from csv file
    # Path: Data
    # File: nrg_bal_s_1_Data.csv
    # Source: https://ec.europa.eu/eurostat/databrowser/view/nrg_bal_s_1/default/table?lang=en
    # Data: Energy balance
    # Country: All countries
    # Year: 2011 - 2020
    # Unit: Thousand tonnes of oil equivalent

    d <- read.csv(file = "./Data/nrg_bal_s_1_Data.csv",
                        header = TRUE)
    d <- d[-c(4, 5, 7)]
    df_total_energy_supply <- subset(d, d$NRG_BAL == "Total energy supply")
    df_1 <- df_total_energy_supply[which(df_total_energy_supply$TIME == year_for_comparison),]
    # Omit Time and NRG_BAL columns
    df_1 <- df_1[,-c(1, 3)]
    colnames(df_1) = c("GEO", "Total_energy_supply")
    for (i in 1:nrow(df_1)){ # nolint
        value <- df_Inflation[year_for_comparison-1959, which(colnames(df_Inflation) == df_1$GEO[i])]
        if (length(value) == 0) {
            df_1$Inflation[i] <- 0
        } else {
            df_1$Inflation[i] <- value
        }
    }
    # Omit "European Union - 27 countries (from 2020)", "Euro area - 19 countries  (from 2015)", "Germany (until 1990 former territory of the FRG)","Kosovo (under United Nations Security Council Resolution 1244/99)", "T\xfcrkiye" rows
    df_1 <- df_1[-c(1, 2, 27, 37, 39),]
    #change the name of the country "Germany (until 1990 former territory of the FRG)" to "Germany"
    df_1$GEO[which(df_1$GEO == "Germany (until 1990 former territory of the FRG)")] <- "Germany"
    #Change df_1$Inflation from character to numeric
    df_1$Inflation <- as.numeric(df_1$Inflation)
    # Add a new column with GDPpc data
    for (i in 1:nrow(df_1)){ # nolint
        value <- df_GDPpc[year_for_comparison-1959, which(colnames(df_GDPpc) == df_1$GEO[i])] # nolint
        if (length(value) == 0) {
            df_1$GDPpc[i] <- 0
        } else {
            df_1$GDPpc[i] <- value
        }
    }
    df_1$GDPpc <- as.numeric(df_1$GDPpc)
    # remove commas from df_1$Total_energy_supply
    df_1$Total_energy_supply <- gsub(",", "", df_1$Total_energy_supply)
    df_1$Total_energy_supply <- as.numeric(df_1$Total_energy_supply)

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

    # Merge the two dataframes
    df_all <- merge(df_1, df_population, by = c("GEO"), all.x = TRUE)

    #Omit Moldova
    df_all <- df_all[df_all$"GEO" != "Moldova",]

    colnames(df_all) <- c("GEO", "Total_energy_supply", "Inflation", "GDPpc", "Population")

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
    df_all <- merge(df_all, df_verified_emisions, by = c("GEO"), all.x = FALSE)
    df_all$verified_emisions <- as.numeric(df_all$verified_emisions)
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

    # Delete all rows from my_data whose GEO is not in df_all
    my_data <- my_data[my_data$"GEO" %in% df_all$"GEO",]
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

    df_all <- merge(df_all, my_data, by = c("GEO"), all.x = FALSE)


    if (will_use_log){
        df_all[,2] <- log(df_all[,2])
        df_all[,4:9] <- log(df_all[,4:9])

    }

    max_total_energy_supply <- max(df_all$"Total_energy_supply")
    max_inflation <- max(df_all$"Inflation")
    max_GDPpc <- max(df_all$"GDPpc")
    max_population <- max(df_all$"Population")
    max_verified_emisions <- max(df_all$"verified_emisions")
    max_Agriculture <- max(df_all$"Agriculture")
    max_Industry <- max(df_all$"Industry")
    max_Manufacturing <- max(df_all$"Manufacturing")


    df_all$"Total_energy_supply" <- df_all$"Total_energy_supply" / max_total_energy_supply
    df_all$"Inflation" <- df_all$"Inflation" / max_inflation
    df_all$"GDPpc" <- df_all$"GDPpc" / max_GDPpc
    df_all$"Population" <- df_all$"Population" / max_population
    df_all$"verified_emisions" <- df_all$"verified_emisions" / max_verified_emisions
    df_all$"Agriculture" <- df_all$"Agriculture" / max_Agriculture
    df_all$"Industry" <- df_all$"Industry" / max_Industry
    df_all$"Manufacturing" <- df_all$"Manufacturing" / max_Manufacturing
    
    #save csv file of df_all
    write.csv(df_all, file = paste("./Data/created_csvs/df_all_",year_for_comparison,(if (will_use_log) "_log" else ""),".csv", sep = "" ), row.names = TRUE)
    return(df_all)
}
read_data()
