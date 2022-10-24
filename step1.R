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
plot(df_Inflation[1:62, which(colnames(df_Inflation) == "Greece")], type = "b", col = "red", xlab = "Year", ylab = "Inflation, consumer prices (annual %)")
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

#Replace " " with "_" in NRG_BAL column
d$NRG_BAL <- gsub(" ", "_", d$NRG_BAL)

######################################################################################

#Iterate the data frame and create several new ones
#with the data for each NRG_BAL
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


