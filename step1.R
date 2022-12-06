# install.packages("ggplot2")
# install.packages("DBI")
# install.packages("RMariaDB")
# install.packages("RMySQL")
# install.packages("stringr")     # Install & load stringr package
# install.packages("xtable") # Install & load xtable package
# install.packages("xlsx")
library("xlsx")
library("xtable") # Load xtable package
library("stringr")
library("DBI")
# library("RMariaDB")
library("RMySQL")
# library("ggplot2")


will_use_log <- TRUE
year_for_comparison <- 2015

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


# Create a new dataframe with data for the year_of_comparison for each Country with colnames ("Country", "Energy_supply","Inflation","GDPpc")
df_1 <- df_total_energy_supply[which(df_total_energy_supply$TIME == year_for_comparison),]
# Omit Time and NRG_BAL columns
df_1 <- df_1[,-c(1, 3)]
colnames(df_1) = c("GEO", "Total_energy_supply")

# Add a new column with Inflation data
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
df <- merge(df_1, df_population, by = c("GEO"), all.x = TRUE)

#Omit Moldova
df <- df[df$"GEO" != "Moldova",]

colnames(df) <- c("GEO", "Total_energy_supply", "Inflation", "GDPpc", "Population")



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
df <- merge(df, df_verified_emisions, by = c("GEO"), all.x = FALSE)
df$verified_emisions <- as.numeric(df$verified_emisions)

################################## import new data from World Bank ##################################
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

# Delete all rows from my_data whose GEO is not in df
my_data <- my_data[my_data$"GEO" %in% df$"GEO",]
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

df <- merge(df, my_data, by = c("GEO"), all.x = FALSE)


if (will_use_log){
    df[,2] <- log(df[,2])
    df[,4:9] <- log(df[,4:9])

}

max_total_energy_supply <- max(df$"Total_energy_supply")
max_inflation <- max(df$"Inflation")
max_GDPpc <- max(df$"GDPpc")
max_population <- max(df$"Population")
max_verified_emisions <- max(df$"verified_emisions")
max_Agriculture <- max(df$"Agriculture")
max_Industry <- max(df$"Industry")
max_Manufacturing <- max(df$"Manufacturing")


df$"Total_energy_supply" <- df$"Total_energy_supply" / max_total_energy_supply
df$"Inflation" <- df$"Inflation" / max_inflation
df$"GDPpc" <- df$"GDPpc" / max_GDPpc
df$"Population" <- df$"Population" / max_population
df$"verified_emisions" <- df$"verified_emisions" / max_verified_emisions
df$"Agriculture" <- df$"Agriculture" / max_Agriculture
df$"Industry" <- df$"Industry" / max_Industry
df$"Manufacturing" <- df$"Manufacturing" / max_Manufacturing







# Create a 2 D dataframe with countries as rows and countries as columns and the value of the Eukleidian Distance between all the values the two countries
df_distance <- data.frame(matrix(NA, nrow = nrow(df), ncol = nrow(df)))
rownames(df_distance) <- df$"GEO"
colnames(df_distance) <- df$"GEO"
for (i in 1 : nrow(df_distance)) { #nolint
  for (j in 1 : ncol(df_distance)) { # nolint 
    df_distance[i, j] <- sqrt((df[i, 2] - df[j, 2])^2 + (df[i, 3] - df[j, 3])^2 + (df[i, 4] - df[j, 4])^2 + (df[i, 5] - df[j, 5])^2 + (df[i, 6] - df[j, 6])^2 + (df[i, 7] - df[j, 7])^2 + (df[i, 8] - df[j, 8])^2 + (df[i, 9] - df[j, 9])^2)
  }
}

# save a file "distance_2015.tex" with the content of the dataframe df_distance
#<<results=tex>>
 #   xtable(df_distance[,21:26], caption = "Distance between countries in 2015", label = "tab:distance_2015")
#@ 


# Create png file of Heatmap of the dataframe df_distance with the countries as rows and columns



png(paste("heatmap_distances_of_", year_for_comparison,"with_full_data_and_log=", will_use_log, ".png", sep=""), width = 2000, height = 2000)
heatmap(as.matrix.data.frame(df_distance), Rowv = NA, Colv = NA, symm = TRUE, margins = c(10, 10), col = colorRampPalette(c("#3863ff8c","white", "red"))(100))
dev.off()


df_free_allocation <- data.frame(nrow = 50,ncol = 2)
colnames(df_free_allocation) <- c("GEO", "verified_emisions")
for (i in 1:length(country_names)) { # nolint
    querr <- paste(
    "SELECT SUM(freeAlloc) FROM `eutl_compliance` WHERE country = '",
    country_eu_abbr2L[i], "' AND etos ='",year_for_comparison,"'", sep = "", collapse = NULL)
    res <- dbSendQuery(kanali, querr) # send query to database
    verified <- dbFetch(res, n = -1) # fetch all data from querry
    dbClearResult(res) # clear result
    df_free_allocation <- rbind(df_free_allocation, data.frame("GEO" = country_names[i], "verified_emisions" = verified[1,1]))
}
colnames(df_free_allocation) <- c("GEO", "verified_emisions")
if (will_use_log){
    df_free_allocation$"verified_emisions" <- log(df_free_allocation$"verified_emisions")
}

# Calculate Eukleidian distance of the verified_emisions from df_free_allocation and store them in df_actual_distance
df_actual_distance <- data.frame(matrix(NA, nrow = nrow(df_free_allocation), ncol = nrow(df_free_allocation)))
rownames(df_actual_distance) <- df_free_allocation$"GEO"
colnames(df_actual_distance) <- df_free_allocation$"GEO"
for (i in 1 : nrow(df_actual_distance)) { #nolint
  for (j in 1 : ncol(df_actual_distance)) { # nolint 
    df_actual_distance[i, j] <- sqrt((df_free_allocation[i, 2] - df_free_allocation[j, 2])^2 )
  }
}

#Normalize values of df_actual_distance to 0 to 1
max_actual_distance <- max(df_actual_distance)
df_actual_distance <- df_actual_distance / max_actual_distance

# Create png file of Heatmap of the dataframe df_actual_distance with the countries as rows and columns
png("heatmap_actual_distances_with_all_data", width = 2000, height = 2000)
heatmap(as.matrix.data.frame(df_actual_distance), Rowv = NA, Colv = NA, symm = TRUE, margins = c(10, 10), col = colorRampPalette(c("#3863ff8c","white", "red"))(100))
dev.off()

# Delete the non comon_countries

non_common_countries <- colnames(df_distance)
non_common_countries <- non_common_countries[!(non_common_countries %in% rownames(df_actual_distance))]

#Omit non common countries from df_distance
df_distance <- df_distance[!(rownames(df_distance) %in% non_common_countries),]
df_distance <- df_distance[,!(colnames(df_distance) %in% non_common_countries)]


#Sort alphabetically the columns and rows of df_distance 
df_distance <- df_distance[order(rownames(df_distance)), order(colnames(df_distance))]
df_actual_distance <- df_actual_distance[order(rownames(df_actual_distance)), order(colnames(df_actual_distance))]


#Omit from actual distance rows and columns "Slovakia" and "Czech Republic"
df_actual_distance <- df_actual_distance[!(rownames(df_actual_distance) %in% c("Slovakia", "Czech Republic", "50")),]
df_actual_distance <- df_actual_distance[,!(colnames(df_actual_distance) %in% c("Slovakia", "Czech Republic", "50"))]

colnames(df_distance) == rownames(df_actual_distance)


# Create a 1D dataframe containing a tuple with 1 value from df_distance and 1 value from df_actual_distance
df_1D <- data.frame(matrix(NA, nrow = nrow(df_distance) * (ncol(df_distance)-1)/2, ncol = 3))
colnames(df_1D) <- c("df_distance", "df_actual_distance", "pair")
for (i in 1 : nrow(df_distance)) { #nolint
  for (j in i : ncol(df_distance)) { # nolint 
    df_1D[(i-1)*ncol(df_distance) + j, 1] <- df_distance[i, j]
    df_1D[(i-1)*ncol(df_distance) + j, 2] <- df_actual_distance[i, j]
    df_1D[(i-1)*ncol(df_distance) + j, 3] <- paste(rownames(df_distance)[i], colnames(df_distance)[j], sep = " - ")
  }
}


# Remove rows with NA and 0
df_1D <- df_1D[!is.na(df_1D$"df_distance"),]
df_1D <- df_1D[!is.na(df_1D$"df_actual_distance"),]
df_1D <- df_1D[df_1D$"df_distance" != 0,]
df_1D <- df_1D[df_1D$"df_actual_distance" != 0,]

lm <- lm(df_1D$"df_actual_distance" ~ df_1D$"df_distance")
summary(lm)

#write that summary to a file
sink(paste("linear_regration_summary_for", year_for_comparison, "_with_all_data_and_log=", will_use_log, ".txt"))
summary(lm)
sink()



# Create png with the regression line
png(paste("scatterplot_with_regression_line_",year_for_comparison,"_with_all_data_and_log=", will_use_log, ".png") , width = 1000, height = 1000)
plot(df_1D$"df_distance", df_1D$"df_actual_distance", xlab = "calculated", ylab = "Actual", main = paste( "Scatterplot of calculated distance and actual distance for the year ", year_for_comparison, sep = ""))
# Color red the points of the scatterpolit where df_1D[3,] contains "Germany"
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

# Create a legend with the colors of the points
legend("topright", legend = c("Germany", "Greece", "Italy", "France", "United Kingdom", "Luxembourg"), col = c("red", "blue", "green", "yellow", "orange", "purple"), pch = 20)

abline(lm, col = "red")
dev.off()
  