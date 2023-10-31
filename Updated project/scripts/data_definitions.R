
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
