source("read_data.R")
test_read_data <-read_data()
print(test_read_data)
test_free <- read_free()
print(test_free)


find_slopes <- function(year = 0){
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
      df_distance[i, j] <- sqrt((df_data[i, 2] - df_data[j, 2])^2 + (df_data[i, 3] - df_data[j, 3])^2 + (df_data[i, 4] - df_data[j, 4])^2 + (df_data[i, 5] - df_data[j, 5])^2 + (df_data[i, 6] - df_data[j, 6])^2 + (df_data[i, 7] - df_data[j, 7])^2 + (df_data[i, 8] - df_data[j, 8])^2 + (df_data[i, 9] - df_data[j, 9])^2)
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

print(find_slopes(year_for_comparison = 2014))
X2014 <- find_slopes(year_for_comparison = 2014)
print(X2014$linear)
print(X2014$data)

#####################################




  # Create png with the regression line
#   #png(paste("Newscatterplot_with_regression_line_",year_for_comparison,"_with_all_data_and_log=", will_use_log, ".png") , width = 1000, height = 1000)
#   png("test2.png", width = 1000, height = 1000)
#   #plot(df_1D$"df_distance", df_1D$"d f_free_distance", xlab = "Combined calculated distance", ylab = "Free distance", main = paste( "Scatterplot of calculated distance and actual distance for the year ", year_for_comparison, sep = ""))  # Color red the points of the scatterpolit where df_1D[3,] contains "Germany"
#   plot(df_1D$"df_distance", df_1D$"df_free_distance", xlab = "Combined calculated distance", ylab = "Free distance")
#   points(df_1D[ str_detect(df_1D$"pair", regex(".Germany")), 1], df_1D[str_detect(df_1D$"pair", regex(".Germany")), 2], col = "red")
#   points(df_1D[ str_detect(df_1D$"pair", regex("Germany.")), 1], df_1D[str_detect(df_1D$"pair", regex("Germany.")), 2], col = "red")
# #  
# #  # Color blue the points of the scatterpolit where df_1D[3,] contains "Greece"
#   points(df_1D[ str_detect(df_1D$"pair", regex(".Greece")), 1], df_1D[str_detect(df_1D$"pair", regex(".Greece")), 2], col = "blue")
#   points(df_1D[ str_detect(df_1D$"pair", regex("Greece.")), 1], df_1D[str_detect(df_1D$"pair", regex("Greece.")), 2], col = "blue")
# #  
# #  # Color green the points of the scatterpolit where df_1D[3,] contains "Italy"
#   points(df_1D[ str_detect(df_1D$"pair", regex(".Italy")), 1], df_1D[str_detect(df_1D$"pair", regex(".Italy")), 2], col = "green")
#   points(df_1D[ str_detect(df_1D$"pair", regex("Italy.")), 1], df_1D[str_detect(df_1D$"pair", regex("Italy.")), 2], col = "green")
# #  
# #  # Color yellow the points of the scatterpolit where df_1D[3,] contains "Ukraine"
#   points(df_1D[ str_detect(df_1D$"pair", regex(".Ukraine")), 1], df_1D[str_detect(df_1D$"pair", regex(".Ukraine")), 2], col = "yellow")
#   points(df_1D[ str_detect(df_1D$"pair", regex("Ukraine.")), 1], df_1D[str_detect(df_1D$"pair", regex("Ukraine.")), 2], col = "yellow")
# #  
# #  # Color Yellow the points of the scatterpolit where df_1D[3,] contains "France"
#   points(df_1D[ str_detect(df_1D$"pair", regex(".France")), 1], df_1D[str_detect(df_1D$"pair", regex(".France")), 2], col = "yellow")
#   points(df_1D[ str_detect(df_1D$"pair", regex("France.")), 1], df_1D[str_detect(df_1D$"pair", regex("France.")), 2], col = "yellow")
# #  
# #  # Color orange the points of the scatterpolit where df_1D[3,] contains "United Kingdom"
#   points(df_1D[ str_detect(df_1D$"pair", regex(".United Kingdom")), 1], df_1D[str_detect(df_1D$"pair", regex(".United Kingdom")), 2], col = "orange")
#   points(df_1D[ str_detect(df_1D$"pair", regex("United Kingdom.")), 1], df_1D[str_detect(df_1D$"pair", regex("United Kingdom.")), 2], col = "orange")
# #  
# #  # Color purple the points of the scatterpolit where df_1D[3,] contains "Luxembourg"
#   points(df_1D[ str_detect(df_1D$"pair", regex(".Luxembourg")), 1], df_1D[str_detect(df_1D$"pair", regex(".Luxembourg")), 2], col = "purple")
#   points(df_1D[ str_detect(df_1D$"pair", regex("Luxembourg.")), 1], df_1D[str_detect(df_1D$"pair", regex("Luxembourg.")), 2], col = "purple")

#  #Create a legend with the colors of the points
#  legend("topright", legend = c("Germany", "Greece", "Italy", "France", "United Kingdom", "Luxembourg"), col = c("red", "blue", "green", "yellow", "orange", "purple"), pch = 20)

#  abline(lm, col = "red")
#  dev.off()

run_test_find_slopes <- function(j, l){
  for (i in j:l){
    X <- find_slopes(year_for_comparison = i)
    print(i)
    if (length(X$linear) < 2){
      print("No data for this year")
    } else {
      print("Slope:")
      print(X$linear$coefficients[2])
      print("Intercept:")
      print(X$linear$coefficients[1])
      print("R squared:")
      print(X$linear$r.squared)
    }
  } 
}

create_graph <- function (year = 0, name = "default"){
    if(year != 0) {year_for_comparison <- year}
  df_1D <- find_slopes(year_for_comparison)$data
  lm <- find_slopes(year_for_comparison)$linear
  print(df_1D)
  # Create png with the regression line
  #png(paste("Newscatterplot_with_regression_line_",year_for_comparison,"_with_all_data_and_log=", will_use_log, ".png") , width = 1000, height = 1000)
  png(paste(name,".png",sep=""), width = 1000, height = 1000)
  #plot(df_1D$"df_distance", df_1D$"d f_free_distance", xlab = "Combined calculated distance", ylab = "Free distance", main = paste( "Scatterplot of calculated distance and actual distance for the year ", year_for_comparison, sep = ""))  # Color red the points of the scatterpolit where df_1D[3,] contains "Germany"
  plot(df_1D$"df_distance", df_1D$"df_free_distance", xlab = "Combined calculated distance", ylab = "Free distance")
  points(df_1D[ str_detect(df_1D$"pair", regex(".Germany")), 1], df_1D[str_detect(df_1D$"pair", regex(".Germany")), 2], col = "red")
  points(df_1D[ str_detect(df_1D$"pair", regex("Germany.")), 1], df_1D[str_detect(df_1D$"pair", regex("Germany.")), 2], col = "red")
#  
#  # Color blue the points of the scatterpolit where df_1D[3,] contains "Greece"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Greece")), 1], df_1D[str_detect(df_1D$"pair", regex(".Greece")), 2], col = "blue")
  points(df_1D[ str_detect(df_1D$"pair", regex("Greece.")), 1], df_1D[str_detect(df_1D$"pair", regex("Greece.")), 2], col = "blue")
#  
#  # Color green the points of the scatterpolit where df_1D[3,] contains "Italy"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Italy")), 1], df_1D[str_detect(df_1D$"pair", regex(".Italy")), 2], col = "green")
  points(df_1D[ str_detect(df_1D$"pair", regex("Italy.")), 1], df_1D[str_detect(df_1D$"pair", regex("Italy.")), 2], col = "green")
#  
#  # Color yellow the points of the scatterpolit where df_1D[3,] contains "Ukraine"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Ukraine")), 1], df_1D[str_detect(df_1D$"pair", regex(".Ukraine")), 2], col = "yellow")
  points(df_1D[ str_detect(df_1D$"pair", regex("Ukraine.")), 1], df_1D[str_detect(df_1D$"pair", regex("Ukraine.")), 2], col = "yellow")
#  
#  # Color Yellow the points of the scatterpolit where df_1D[3,] contains "France"
  points(df_1D[ str_detect(df_1D$"pair", regex(".France")), 1], df_1D[str_detect(df_1D$"pair", regex(".France")), 2], col = "yellow")
  points(df_1D[ str_detect(df_1D$"pair", regex("France.")), 1], df_1D[str_detect(df_1D$"pair", regex("France.")), 2], col = "yellow")
#  
#  # Color orange the points of the scatterpolit where df_1D[3,] contains "United Kingdom"
  points(df_1D[ str_detect(df_1D$"pair", regex(".United Kingdom")), 1], df_1D[str_detect(df_1D$"pair", regex(".United Kingdom")), 2], col = "orange")
  points(df_1D[ str_detect(df_1D$"pair", regex("United Kingdom.")), 1], df_1D[str_detect(df_1D$"pair", regex("United Kingdom.")), 2], col = "orange")
#  
#  # Color purple the points of the scatterpolit where df_1D[3,] contains "Luxembourg"
  points(df_1D[ str_detect(df_1D$"pair", regex(".Luxembourg")), 1], df_1D[str_detect(df_1D$"pair", regex(".Luxembourg")), 2], col = "purple")
  points(df_1D[ str_detect(df_1D$"pair", regex("Luxembourg.")), 1], df_1D[str_detect(df_1D$"pair", regex("Luxembourg.")), 2], col = "purple")

 #Create a legend with the colors of the points
  legend("topright", legend = c("Germany", "Greece", "Italy", "France", "United Kingdom", "Luxembourg"), col = c("red", "blue", "green", "yellow", "orange", "purple"), pch = 20)

  abline(lm, col = "red")
  dev.off()
}

# run_test_find_slopes(2012,2018)
