source("scripts/config.R")
source("scripts/data_definitions.R")
source("scripts/read_data.R")


find_slopes <- function(
  year = 0,
  weight_population = 1,
  weight_GDPpc = 1,
  weight_inflation = 1,
  weight_agriculture = 1,
  weight_industry = 1,
  weight_manufacturing = 1,
  weight_total_energy_supply = 1,
  weight_verified_emissions = 1
) {
  if (year != 0) year_for_comparison <- year
  
  # Load data
  df_data <- read_data(year_for_comparison)
  df_free <- read_free(year_for_comparison)
  
  # Define a function to calculate Euclidean distance
  euclidean_distance <- function(vec1, vec2, weights) {
    sum((vec1 - vec2)^2 * weights)
  }
  
  # Calculate Euclidean distance between data and free allocations
  df_distance <- dist(df_data[, 2:10], method = "euclidean", 
                       diag = TRUE, upper = TRUE)
  df_free_distance <- dist(df_free[, 2], method = "euclidean", 
                           diag = TRUE, upper = TRUE)
  
  # Create a 1D dataframe
  df_1D <- data.frame(
    df_distance = as.vector(df_distance),
    df_free_distance = as.vector(df_free_distance),
    pair = as.vector(df_distance)
  )
  
  # Remove rows with NA and 0
  df_1D <- df_1D[!is.na(df_1D$df_distance) & df_1D$df_distance != 0, ]
  df_1D <- df_1D[!is.na(df_1D$df_free_distance) & df_1D$df_free_distance != 0, ]
  
  # Linear regression
  lm_model <- lm(df_1D$df_free_distance ~ df_1D$df_distance)
  lm_summary <- summary(lm_model)
  
  # Save summary to a file
  text_s <- generate_filename(year_for_comparison)
  summary_file <- paste("linear_regression_summary_for", text_s, ".txt")
  sink(summary_file)
  print(lm_summary)
  sink()
  
  # Plot the regression line
  plot(
    df_1D$df_distance,
    df_1D$df_free_distance,
    xlab = "Distance between countries in 2015",
    ylab = "Distance between countries in 2015 with free allocation",
    main = paste("Distance between countries in 2015 and in 2015 with free allocation", year_for_comparison)
  )
  abline(lm_model, col = "red")
  
  return(list(data = df_1D, linear = lm_model))
}