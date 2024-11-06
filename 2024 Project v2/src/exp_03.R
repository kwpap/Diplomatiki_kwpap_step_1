# Find the best weights
library(xtable)
library(kableExtra)
# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/All_in_on.R")
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/linear_tester.R")

optimize_weights_with_constraints_with_one <- function(year = 0, country = "none") {
  initial_weights <- rep(1, 8)
  
  objective_function <- function(weights) {
    # Penalize negative weights
    if (any(weights < 0)) return(Inf)
    -summary(
      find_slopes_with_one_country_with_weights(
        year = year, country = country, weights = weights
      )$linear
    )$r.squared
  }
  
  result <- optim(
    par = initial_weights,
    fn = objective_function,
    method = "L-BFGS-B",
    lower = rep(0, 8),
    upper = rep(100, 8)
  )
  
  best_weights <- result$par
  best_r_squared <- -result$value
  
  print(best_weights)
  print(paste("Best R^2:", best_r_squared))
  return(best_weights)
}

perform_analysis <- function(log_file, plot_path, start_year, end_year) {
  will_normalise <- TRUE
  
  # Define the year range
  years <- start_year:end_year
  # Initialize data frames to store R^2 and model objects
  r_squared_df <- data.frame(matrix(NA, nrow = length(list_eur_countries), ncol = length(years)))
  rownames(r_squared_df) <- list_eur_countries
  colnames(r_squared_df) <- years
  
  linear_models_df <- data.frame(matrix(vector("list", length(list_eur_countries) * length(years)), 
                                        nrow = length(list_eur_countries), ncol = length(years)))
  rownames(linear_models_df) <- list_eur_countries
  colnames(linear_models_df) <- years
  

  # Iterate over each country and year
  for (country in list_eur_countries) {
    for (year in years) {
      
      # Log the start of the iteration
      log_message(paste("Starting analysis for Country:", country, "Year:", year))
      
      tryCatch({
        # Step 1: Find the best combination of weights
        weights <- optimize_weights_with_constraints_with_one(year = year, country = country)
        
        # Step 2: Perform linear regression with the optimal weights
        model_data <- find_slopes_with_one_country_with_weights(year = year, country = country, weights = weights)
        linear_model <- model_data$linear
        r_squared <- summary(linear_model)$r.squared
        p_val_model <- p_val(linear_model)
        mse_model <- MSE(linear_model)
        
        # Step 3: Store R^2 and the model in respective data frames
        r_squared_df[country, as.character(year)] <- r_squared
        linear_models_df[[country, as.character(year)]] <- linear_model
        
        # Plot creation wrapped in tryCatch to handle any errors
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "png", 
                               country = country, 
                               weights = weights, 
                               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/")
        }, error = function(e) {
          log_message(paste("Failed to create PNG plot for Country:", country, "Year:", year, "- Error:", e$message))
        })
        
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "svg", 
                               country = country, 
                               weights = weights, 
                               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/")
        }, error = function(e) {
          log_message(paste("Failed to create SVG plot for Country:", country, "Year:", year, "- Error:", e$message))
        })
        
        # Log successful completion of the iteration
        log_message(paste("Completed analysis for Country:", country, "Year:", year, 
                          "- Weights:", toString(weights), 
                          "- R^2:", r_squared, 
                          "- P-Value:", p_val_model, 
                          "- MSE:", mse_model))
        
      }, error = function(e) {
        # Log any error that occurs in the main analysis steps
        log_message(paste("Failed analysis for Country:", country, "Year:", year, "- Error:", e$message))
      })
    }
  }
  
  # Return a list with the r_squared and linear models data frames
  return(list(R_Squared = r_squared_df, Linear_Models = linear_models_df))
}


##################################### EXP 3 ####################################################################
# Define file paths and year range
log_file <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/log_file.txt"
plot_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/"
RDS_03_file <-"C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/exp_03_normalized.rds"
start_year <- 2005
end_year <- 2020

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

# Function to parse the log file and extract completion lines
parse_log_file <- function(log_file_path) {
  # Read the log file
  log_lines <- readLines(log_file_path)
  
  # Initialize an empty list to store the extracted data
  completion_data <- list()
  
  # Regular expression to match completion lines
  completion_pattern <- "\\[.*\\] Completed analysis for Country: (.+) Year: (\\d+) - Weights: (.+) - R\\^2: (.+) - P-Value: (.+) - MSE: (.+)"
  
  # Loop through each line and extract data
  for (line in log_lines) {
    if (grepl("Completed analysis for Country:", line)) {
      match <- regexec(completion_pattern, line)
      matched_groups <- regmatches(line, match)
      
      if (length(matched_groups[[1]]) > 1) {
        country <- matched_groups[[1]][2]
        year <- as.integer(matched_groups[[1]][3])
        weights_str <- matched_groups[[1]][4]
        r_squared <- as.numeric(matched_groups[[1]][5])
        p_value <- as.numeric(matched_groups[[1]][6])
        mse <- as.numeric(matched_groups[[1]][7])
        
        # Parse the weights string into a numeric vector
        weights <- as.numeric(unlist(strsplit(weights_str, ",\\s*")))
        
        # Append to the list
        completion_data[[length(completion_data) + 1]] <- list(
          country = country,
          year = year,
          weights = weights,
          r_squared = r_squared,
          p_value = p_value,
          mse = mse
        )
      }
    }
  }
  
  return(completion_data)
}

# Function to process each completion entry
process_completion_entry <- function(entry) {
  # Extract the parameters
  country <- entry$country
  year <- entry$year
  weights <- entry$weights
  
  # Wrap the processing in a tryCatch block to handle errors
  result_entry <- tryCatch({
    # Call find_slopes_with_one_country() with the extracted parameters
    regression_result <- find_slopes_with_one_country(
      year = year,
      weight_population = weights[1],
      weight_GDPpc = weights[2],
      weight_inflation = weights[3],
      weight_agriculture = weights[4],
      weight_industry = weights[5],
      weight_manufacturing = weights[6],
      weight_total_energy_supply = weights[7],
      weight_verified_emisions = weights[8],
      country = country
    )
    
    # Extract data for linearity checks
    df_distance <- regression_result$data
    # Assuming df_distance has columns 'df_distance' and 'df_free_distance'
    
    # Check if df_distance has enough data points
    if (nrow(df_distance) < 2 || all(is.na(df_distance$df_distance)) || all(is.na(df_distance$df_free_distance))) {
      message(paste("Insufficient data for regression analysis for", country, "in", year))
      return(NULL)  # Exclude this entry
    }
    
    df_for_checks <- data.frame(
      x = df_distance$df_distance,
      y = df_distance$df_free_distance
    )
    
    # Perform linearity checks and get the metrics
    metrics <- linearity_checks(df_for_checks)
    
    # If linearity_checks returned NULL, exclude this entry
    if (is.null(metrics)) {
      message(paste("Linearity checks failed for", country, "in", year))
      return(NULL)  # Exclude this entry
    }
    
    # Extract required metrics from the results
    result_entry <- list(
      country = country,
      year = year,
      pearson_correlation = metrics$pearson_correlation,
      spearman_correlation = metrics$spearman_correlation,
      kendall_tau = metrics$kendall_tau,
      linear_p_value = metrics$linear_model_summary$coefficients[2, "Pr(>|t|)"],
      linear_r_squared = metrics$linear_model_summary$r.squared,
      mse = metrics$mse,
      rmse = metrics$rmse,
      mae = metrics$mae,
      shapiro_p_value = metrics$shapiro_test$p.value,
      breusch_pagan_p_value = metrics$breusch_pagan_test$p.value,
      reset_test_p_value = metrics$reset_test$p.value,
      quadratic_p_value = metrics$quadratic_model_summary$coefficients["I(x^2)", "Pr(>|t|)"],
      quadratic_coeff = metrics$quadratic_model_summary$coefficients["I(x^2)", "Estimate"],
      aic_linear = metrics$aic_values["Linear_Model"],
      bic_linear = metrics$bic_values["Linear_Model"],
      aic_quadratic = metrics$aic_values["Quadratic_Model"],
      bic_quadratic = metrics$bic_values["Quadratic_Model"],
      anova_p_value = metrics$anova_models$`Pr(>F)`[2]
    )
    
    return(result_entry)
    
  }, error = function(e) {
    # Handle the error by printing a message and excluding the entry
    message(paste("Error processing", country, "in", year, ":", e$message))
    return(NULL)  # Exclude this entry
  })
  
  return(result_entry)
}

# Main function to process the log file and save the results
process_log_and_save_metrics <- function(log_file_path, output_rds_path) {
  # Parse the log file
  completion_data <- parse_log_file(log_file_path)
  
  # Initialize a list to store all results
  all_results <- list()
  
  # Loop through each completion entry
  for (entry in completion_data) {
    # Process the entry and get the metrics
    result_entry <- process_completion_entry(entry)
    
    # If result_entry is not NULL, append to the results list
    if (!is.null(result_entry)) {
      all_results[[length(all_results) + 1]] <- result_entry
    } else {
      # Entry was excluded due to errors
      next
    }
  }
  
  # Check if there are any successful results
  if (length(all_results) == 0) {
    message("No successful analyses to save.")
    return(NULL)
  }
  
  # Convert the list of results to a dataframe
  results_df <- do.call(rbind, lapply(all_results, as.data.frame))
  
  # Save the dataframe using saveRDS
  saveRDS(results_df, output_rds_path)
  
  cat("Metrics saved to", output_rds_path, "\n")
}

# Example usage:
# Replace 'path/to/log_file.txt' with the actual path to your log file
# Replace 'path/to/output_metrics.rds' with the desired output path
log_file_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/log_file.txt"
output_rds_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/exp_03_linear_metrics.rds"

# Run the processing function
process_log_and_save_metrics(log_file_path, output_rds_path)
linear_results <- readRDS(output_rds_path)
