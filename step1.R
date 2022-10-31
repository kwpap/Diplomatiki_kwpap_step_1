# install.packages("ggplot2")
# install.packages("DBI")
# install.packages("RMariaDB")
# install.packages("RMySQL")

library("DBI")
library("RMariaDB")
library("RMySQL")
library("ggplot2")


# Load Data from csv file
# Path: Data
# File: GDP_per_capita_1960_2021.csv
# Source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
# Data: GDP per capita (current US$)
# Country: All countries
# Year: 1960 - 2021
# Unit: US$

# Headers are on the 4rth row
headers <- read.csv(file = "./Data/GDP_per_capita_1960_2021.csv",
                    skip = 4,
                    header = FALSE,
                    nrows = 1,
                    as.is = TRUE)
df_GDPpc <- read.csv(file = "./Data/GDP_per_capita_1960_2021.csv",
                    skip = 4, header = FALSE)
colnames(df_GDPpc) <- headers

# Create new dataframe, where colnames are the country names
# and the rows are the years
df_GDPpc <- subset(df_GDPpc, select = -c(2, 3, 4))
df_GDPpc <- t(df_GDPpc)
colnames(df_GDPpc) <- df_GDPpc[1, ]
df_GDPpc <- df_GDPpc[-1, ]

# To select precific country and year use the following syntax
# df_GDPpc[year-1959, which(colnames(df_GDPpc) == "country_name")]

# Example plot
png("Greece.png")
plot(df_GDPpc[1:62, which(colnames(df_GDPpc) == "Greece")], type = "l", col = "red", xlab = "Year", ylab = "GDP per capita (current US$)")
dev.off()

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

# To select precific country and year use the following syntax
# df_Inflation[year-1959, which(colnames(df_Inflation) == "country_name")]

# Example plot
png("Greece_Inflation.png")
plot(df_Inflation[1:62, which(colnames(df_Inflation) == "Greece")],
    type = "b", col = "red", xlab = "Year", ylab = "Inflation,
    consumer prices (annual %)")
dev.off()

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



######################################################################################

#Iterate the data frame and create several new ones
#with the data for each NRG_BAL
#Replace " " with "_" in NRG_BAL column
# d$NRG_BAL <- gsub(" ", "_", d$NRG_BAL)
# for (i in 1:length(unique(d$NRG_BAL))) {
#   assign(paste0("df_", unique(d$NRG_BAL)[i]), subset(d, d$NRG_BAL == unique(d$NRG_BAL)[i]))
# }
# So there are 8 new data frames
# df_Energy_supply
# df_Energy_use
# df_Energy_imports
# df_Energy_exports
# df_Energy_production
# df_Energy_consumption
# df_Energy_final_consumption
# df_Energy_final_consumption_of_households
#######################################################################################

# Subtrack only data for Energy consumption from d dataframe
df_total_energy_supply <- subset(d, d$NRG_BAL == "Total energy supply")

year_for_comparison = 2015
# Create a new dataframe with data for the year_of_comparison for each Country with colnames ("Country", "Energy_supply","Inflation","GDPpc")
df_1 <- df_total_energy_supply[which(df_total_energy_supply$TIME == year_for_comparison),]
# Omit Time and NRG_BAL columns
df_1 <- df_1[,-c(1, 3)]
colnames(df_1) = c("GEO", "Total_energy_supply")

# Add a new column with Inflation data
for (i in 1:nrow(df_1)){
    value <- df_Inflation[year_for_comparison-1959, which(colnames(df_Inflation) == df_1$GEO[i])]
    if (length(value) == 0) {
        df_1$Inflation[i] <- 0
    } else {
        df_1$Inflation[i] <- value
    }
}
# Omit "European Union - 27 countries (from 2020)", "Euro area - 19 countries  (from 2015)", "Germany (until 1990 former territory of the FRG)","Kosovo (under United Nations Security Council Resolution 1244/99)", "T\xfcrkiye" rows
df_1 <- df_1[-c(1, 2, 7, 27, 37, 39),]

#Change df_1$Inflation from character to numeric
df_1$Inflation <- as.numeric(df_1$Inflation)

# Add a new column with GDPpc data
for (i in 1:nrow(df_1)){
    value <- df_GDPpc[year_for_comparison-1959, which(colnames(df_GDPpc) == df_1$GEO[i])]
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
df_population <- df_population[,-c(2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13)]
colnames(df_population) <- c("GEO_2LABBR", "2015")
# Find the number in each entry of column X2015 and remove the rest
df_population$"2015" <- gsub("[^0-9]", "", df_population$"2015")
df_population$"2015" <- as.numeric(df_population$"2015")
#Omit NA values
df_population <- df_population[!is.na(df_population$"2015"),]
df_population$"2015" <- as.numeric(df_population$"2015")
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


colnames(df_population) <- c("GEO", "Year")

# Merge the two dataframes
df <- merge(df_1, df_population, by = c("GEO"), all.x = TRUE)

#Omit Moldova
df <- df[df$"GEO" != "Moldova",]

colnames(df) <- c("GEO", "Total_energy_supply", "Inflation", "GDPpc", "Population")

# Mormalize all the values to 0 to 1 of the df
max_total_energy_supply <- max(df$"Total_energy_supply")
max_inflation <- max(df$"Inflation")
max_GDPpc <- max(df$"GDPpc")
max_population <- max(df$"Population")

df$"Total_energy_supply" <- df$"Total_energy_supply" / max_total_energy_supply
df$"Inflation" <- df$"Inflation" / max_inflation
df$"GDPpc" <- df$"GDPpc" / max_GDPpc
df$"Population" <- df$"Population" / max_population





# Create a 2 D dataframe with countries as rows and countries as columns and the value of the Eukleidian Distance between all the values the two countries
df_distance <- data.frame(matrix(NA, nrow = nrow(df), ncol = nrow(df)))
rownames(df_distance) <- df$"GEO"
colnames(df_distance) <- df$"GEO"
for (i in 1 : nrow(df_distance)) { #nolint
  for (j in 1 : ncol(df_distance)) { # nolint 
    df_distance[i, j] <- sqrt((df[i, 2] - df[j, 2])^2 + (df[i, 3] - df[j, 3])^2 + (df[i, 4] - df[j, 4])^2 + (df[i, 5] - df[j, 5])^2)
  }
}
# Create png file of Heatmap of the dataframe df_distance with the countries as rows and columns



png("heatmap_distances.png", width = 2000, height = 2000)
heatmap(as.matrix.data.frame(df_distance), Rowv = NA, Colv = NA, symm = TRUE, margins = c(10, 10), col = colorRampPalette(c("#3863ff8c","white", "red"))(100))
dev.off()


# Chapter 2: Calculating the distance of those countries from the database

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




for (i in 1:length(country_names)) {
    df_temp <- data.frame()
    rv <- vector()
    querr <- paste(
    "SELECT SUM(verified) FROM `eutl_compliance` WHERE country = '",
    country_eu_abbr2L[i], "' AND etos ='2015'", sep = "", collapse = NULL)
    res <- dbSendQuery(kanali, querr) # send query to database
    verified <- dbFetch(res, n = -1) # fetch all data from querry
    dbClearResult(res) # clear result
    rv <- c(rv, verified[1,1])
    print(rv)
}
df_temp <- data.frame(rv)
colnames(df_temp) <- c(country_names[i])

print ("vvv")
