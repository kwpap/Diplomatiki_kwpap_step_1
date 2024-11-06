# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/All_in_on.R")

# Create plots for all the distances from all countries to all the others
for (year in c(2005:2020)) {
  # Try to create the SVG graph, log any error that occurs
  tryCatch({
    create_graph(
      year = year, 
      type = "svg",
      name = "01_distances_from_features_to_frees_all_countries_", 
      path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/01_distnaces_all_from_all/", 
      weights = c(1,1,1,1,1,1,1,1)
    )
  }, error = function(e) {
    message(paste("Error creating SVG graph for year:", year, "-", e$message))
  })
  
  # Try to create the PNG graph, log any error that occurs
  tryCatch({
    create_graph(
      year = year, 
      type = "png",
      name = "01_distances_from_features_to_frees_all_countries_", 
      path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/01_distnaces_all_from_all/", 
      weights = c(1,1,1,1,1,1,1,1)
    )
  }, error = function(e) {
    message(paste("Error creating PNG graph for year:", year, "-", e$message))
  })
  
}

# Create plots for all the distances from the middle one. 
for (year in c(2008:2020)) {
  create_graph_for_one(year = year, 
                       name = "02_distances_from_one_",
                       type = "png", 
                       country = "none", 
                       weights = c(1,1,1,1,1,1,1,1), 
                       path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/02_distances_from_one/")
  create_graph_for_one(year = year, 
                       name = "02_distances_from_one_",
                       type = "svg", 
                       country = "none", 
                       weights = c(1,1,1,1,1,1,1,1), 
                       path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/02_distances_from_one/")
}
create_graph_for_one(year = 2018, 
                     name = "02_distances_from_one_",
                     type = "png", 
                     country = "Germany", 
                     weights = c(1,1,1,1,1,1,1,1), 
                     path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/02_distances_from_one/")


# Calculate the best combo of weights for atributes to maximize the R^2
find_the_best_combo_with_one(year = 2015, country = "Germany")

find_the_better_best_combo(year = 2015) 
df_ctfac <- create_table_for_all_countries() # How well does each country explain the rest in a table

# Load the xtable library
library(xtable)

# Convert data frame to LaTeX with customized options
latex_code <- print(
  xtable(df_ctfac, caption = "All distances", label = "tab:sample_table", align = c("l", "r", "r", "r")),
  type = "latex",
  include.rownames = FALSE,
  floating = TRUE,
  tabular.environment = "tabular",
  booktabs = TRUE,
  sanitize.text.function = identity
)

find_the_best_combo_with_one() # obv

dat <- All_the_countries_throught_the_years_with_best_combo()

###########################################################################################################
############################ This helps visualize and present the data ####################################
#result <- visualize_attribute("Population")
#result <- visualize_attribute("Manufacturing")
result <- visualize_attribute("Total_energy_supply")
# Possible attributes: Free Verified_emissions Total_energy_supply GDPpc Population Inflation Agriculture Industry Manufacturing Energy_Intensity

for (item in result) {
  print(item)
}
# Access the individual components
scatter_plot <- result$scatter_plot
box_plot <- result$box_plot
trends_plot <- result$trends_plot
summary_table <- result$summary_table

############################################################################################################
compare_free_in_two_years(2010, 2015)

calculate_all_with_given_weights(c(60,20,15,70,410,10,170,850))
#Normalize takes values: Mean, MinMax, Germany, max
clustering(normalize = "max",  minNc = 3, maxNc = 10)

clustering_per_capita("max", minNc = 3, maxNc = 10)

can_countries_explain_their_own_cluster()

can_countries_explain_their_own_cluster(cluster_to_plot = 2) # OLD SHIT

clustered_data <- clustering_with_pca(normalize = "max", minNc = 2, maxNc = 7, n_components = 2)

# Example usage to visualize clustering with PCA
visualized_data_pca <- visualize_clustering_results(normalize = "max", minNc = 2, maxNc = 7, use_pca = TRUE)

# Example usage to visualize clustering without PCA
visualized_data_no_pca <- visualize_clustering_results(normalize = "max", minNc = 2, maxNc = 7, use_pca = FALSE)

comparison_results <- compare_clustering_methods(normalize = "max", minNc = 2, maxNc = 7, n_components = 2)

# Choose from: Free Verified_emissions Total_energy_supply      GDPpc Population Inflation Agriculture Industry Manufacturing Energy_Intensity
model <- features_linear_free(select = 2, attribute = "Industry")

plot_correlation_matrix(2010)  # Replace 2020 with the desired 


##################################### EXP 3 ####################################################################
# Define file paths and year range
log_file <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/log_file.txt"
plot_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/"
RDS_03_file <-"C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/exp_03_normalized.rds"
start_year <- 2005
end_year <- 2018

# Call the perform_analysis function
results <- perform_analysis(log_file = log_file, plot_path = plot_path, start_year = start_year, end_year = end_year)
#results <- perform_analysis_parallel(log_file = log_file, plot_path = plot_path, start_year = start_year, end_year = end_year)
saveRDS(results, file = RDS_03_file)
#results <- readRDS(RDS_03_file)
# Access the results
r_squared_data <- results$R_Squared
linear_models_data <- results$Linear_Models

# Print or inspect the data frames
print(r_squared_data)       # View R^2 values for each country-year
# Assuming linear_models_df contains linear regression model objects
p_value_df <- linear_models_data  # Create a copy if you want to keep the original

# Loop through each cell in the data frame
for (i in seq_len(nrow(linear_models_data))) {
  for (j in seq_len(ncol(linear_models_data))) {
    # Check if the entry is a model object and has an R-squared value
    model <- linear_models_data[[i, j]]
    if (!is.null(model) && inherits(model, "lm")) {  # Check if it’s a valid lm object
      p_value_df[i, j] <- p_val(model)  # Extract the R-squared value
    } else {
      p_value_df[i, j] <- NA  # Set to NA if there's no valid model
    }
  }
}
latex_code <- print(
  xtable(r_squared_data, caption = "All distances", label = "tab:sample_table"),
  type = "latex",
  include.rownames = FALSE,
  floating = TRUE,
  tabular.environment = "tabular",
  booktabs = TRUE,
  sanitize.text.function = identity
)


################################################################################################################

