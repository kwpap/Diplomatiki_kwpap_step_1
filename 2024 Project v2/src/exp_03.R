# Find the best weights
library(xtable)
library(kableExtra)
# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/All_in_on.R")
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/linear_tester.R")

# Let's try again. 
r_sq_finder <- function(year , country,weights){

  df_data <-read_data_2(year)
  df_free <- read_free(year)
  country_index <- which(df_data$GEO == country)
  country_free_index <- which(df_free$GEO == country)
  #print(paste("now working with:", country,"for:", year, "with verified:", df_data[df_data$GEO == country,]$Verified_emissions, "and free:", df_free[df_free$GEO==country,]$Free))
  
  df_distance <- data.frame(matrix(NA, nrow = nrow(df_data) - 1, ncol = 3))
  colnames(df_distance) <- c("GEO", "df_distance", "df_free_distance")
  for (i in 1 : nrow(df_distance)) { #nolint
    df_distance[i, 1] <- df_data[i, 1]
    df_distance[i, 2] <- sqrt(
                            (df_data[i, "Total_energy_supply"] - df_data[country_index, "Total_energy_supply"])^2 * weights$Total_energy_supply +
                            (df_data[i, "GDPpc"] - df_data[country_index, "GDPpc"])^2 * weights$GDPpc +
                            (df_data[i, "Population"] - df_data[country_index, "Population"])^2 * weights$Population +
                            (df_data[i, "Inflation"] - df_data[country_index, "Inflation"])^2 * weights$Inflation +
                            (df_data[i, "Agriculture"] - df_data[country_index, "Agriculture"])^2 * weights$Agriculture +
                            (df_data[i, "Industry"] - df_data[country_index, "Industry"])^2 * weights$Industry +
                            (df_data[i, "Manufacturing"] - df_data[country_index, "Manufacturing"])^2 * weights$Manufacturing +
                            (df_data[i, "Energy_Intensity"] - df_data[country_index, "Energy_Intensity"])^2 * weights$Energy_Intensity
                              )
    df_distance[i,3] <- abs(df_free$Free[which(df_data[i,1] == df_free$GEO)] - df_free$Free[country_free_index])
  }
  r_2 <-  summary(lm(df_distance$"df_free_distance" ~ df_distance$"df_distance"))$r.squared
  return (r_2)
}

# Define expected column names for weights
weight_names <- c("Total_energy_supply", "GDPpc", "Population", "Inflation", 
                  "Agriculture", "Industry", "Manufacturing", "Energy_Intensity")

# Conversion function from vector to named list
vector_to_named_weights <- function(weight_vector) {
  setNames(as.list(weight_vector), weight_names)
}

# The optimization function
optimize_weights_with_constraints_with_one <- function(year = 0, country = "none") {
  initial_weights <- rep(50, 8)
  
  objective_function <- function(weight_vector) {
    # Convert vector to named list for readability within the function
    named_weights <- vector_to_named_weights(weight_vector)
    
    # Penalize negative weights
    if (any(weight_vector < 0)) return(Inf)
    
    # Pass the named list to r_sq_finder
    -r_sq_finder(year = year, country = country, weights = named_weights)
  }
  
  result <- optim(
    par = initial_weights,
    fn = objective_function,
    method = "L-BFGS-B",
    lower = rep(0, 8),
    upper = rep(100, 8)
  )
  
  # Retrieve the best weights and convert to named list
  best_weights <- vector_to_named_weights(result$par)
  best_r_squared <- -result$value
  
  print(best_weights)
  print(paste("Best R^2:", best_r_squared))
  return(best_weights)
}



perform_analysis <- function(log_file, plot_path, start_year, end_year) {
  will_normalise <- TRUE
  list_of_useful_countries <- setdiff(list_eur_countries, c("Romania", "Malta", "Cyprus"))
  # Define the year range
  years <- start_year:end_year
  # Initialize data frames to store R^2 and model objects
  r_squared_df <- data.frame(matrix(NA, nrow = length(list_of_useful_countries), ncol = length(years)))
  rownames(r_squared_df) <- list_of_useful_countries
  colnames(r_squared_df) <- years
  
  linear_models_df <- data.frame(matrix(vector("list", length(list_of_useful_countries) * length(years)), 
                                        nrow = length(list_of_useful_countries), ncol = length(years)))
  rownames(linear_models_df) <- list_of_useful_countries
  colnames(linear_models_df) <- years
  
  # Iterate over each country and year
  for (country in list_of_useful_countries) {
    for (year in years) {
      
      # Log the start of the iteration
      log_message(paste("Starting analysis for Country:", country, "Year:", year), log_file = log_file)
      
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
                               path = plot_path)
        }, error = function(e) {
          log_message(paste("Failed to create PNG plot for Country:", country, "Year:", year, "- Error:", e$message), log_file = log_file)
        })
        
        tryCatch({
          create_graph_for_one(year = year, 
                               name = "03_best_weights",
                               type = "svg", 
                               country = country, 
                               weights = weights, 
                               path = plot_path)
        }, error = function(e) {
          log_message(paste("Failed to create SVG plot for Country:", country, "Year:", year, "- Error:", e$message), log_file = log_file)
        })
        
        # Log successful completion of the iteration
        log_message(paste("Completed analysis for Country:", country, "Year:", year, 
                          "- Weights:", toString(weights), 
                          "- R^2:", r_squared, 
                          "- P-Value:", p_val_model, 
                          "- MSE:", mse_model), log_file = log_file)
        
      }, error = function(e) {
        # Log any error that occurs in the main analysis steps
        log_message(paste("Failed analysis for Country:", country, "Year:", year, "- Error:", e$message), log_file = log_file)
      })
    }
  }
  
  # Return a list with the r_squared and linear models data frames
  return(list(R_Squared = r_squared_df, Linear_Models = linear_models_df))
}


##################################### EXP 3 ####################################################################
# Define file paths and year range
log_file <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/log_file_v2.txt"
plot_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/v2/"
RDS_03_file <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/exp_03_normalized_v2.rds"
start_year <- 2005
end_year <- 2020

# Call the perform_analysis function############################################################################################
results <- perform_analysis(log_file = log_file, plot_path = plot_path, start_year = start_year, end_year = end_year)
saveRDS(results, file = RDS_03_file)
results <- readRDS(RDS_03_file)
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

# Or using the new one:
r_squared_data_2_decimal <- r_squared_data %>% mutate(across(where(is.numeric), round, 2))
latex_table <- kable(r_squared_data_2_decimal, format = "latex", longtable = TRUE, booktabs = TRUE, row.names = TRUE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  column_spec(2:ncol(r_squared_data_2_decimal), width = "0.45cm") # Adjust 2cm as needed to fit the page

# Save to .tex file
cat(latex_table, file = "exp3_3_r^2.tex")


################################################################################################################
remove_columns <- function(dataframe, columns_to_remove) {
  dataframe <- dataframe[, !colnames(dataframe) %in% columns_to_remove]
  return(dataframe)
}
# Function to parse the log file and extract completion lines
parse_log_file <- function(log_file_path) {
  lines <- readLines(log_file_path)
  completion_data <- list()
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Check for completed analysis
    if (grepl("Completed analysis for Country:", line)) {
      matches <- regmatches(line, gregexpr("Country: ([^ ]+) Year: ([0-9]+) - Weights: ([^ ]+) - R\\^2: ([^ ]+) - P-Value: ([^ ]+) - MSE: ([^ ]+)", line))
      if (length(matches[[1]]) > 0) {
        entry <- list(
          type = "completed",
          country = sub(".*Country: ([^ ]+) .*", "\\1", line),
          year = as.integer(sub(".*Year: ([0-9]+) .*", "\\1", line)),
          weights = as.numeric(strsplit(sub(".*Weights: ([^ ]+) - R\\^2.*", "\\1", line), ",")[[1]]),
          r_squared = as.numeric(sub(".*R\\^2: ([^ ]+) .*", "\\1", line)),
          p_value = as.numeric(sub(".*P-Value: ([^ ]+) .*", "\\1", line)),
          mse = as.numeric(sub(".*MSE: ([^ ]+)", "\\1", line))
        )
        completion_data[[length(completion_data) + 1]] <- entry
      }
      
      # Check for failed analysis
    } else if (grepl("Failed analysis for Country:", line)) {
      entry <- list(
        type = "failed",
        country = sub(".*Country: ([^ ]+) Year: ([0-9]+).*", "\\1", line),
        year = as.integer(sub(".*Year: ([0-9]+).*", "\\1", line)),
        error = sub(".*Error: (.+)", "\\1", line)
      )
      completion_data[[length(completion_data) + 1]] <- entry
    }
  }
  
  return(completion_data)
}

# Function to modify column names for LaTeX \makecell with line breaks
format_colnames_for_latex <- function(dataframe) {
  colnames(dataframe) <- sapply(colnames(dataframe), function(name) {
    # Replace underscores with line breaks (\\n in LaTeX syntax)
    modified_name <- gsub("_", " ", name)
    # Wrap in \makecell for LaTeX
    paste0(modified_name, "")
  })
  return(dataframe)
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
      weights = weights,  # Pass the entire weights list directly
      country = country
    )
    
    # Extract data for linearity checks
    df_distance <- regression_result$data
    
    # Verify that df_distance has the required columns and data
    if (!all(c("df_distance", "df_free_distance") %in% names(df_distance)) || 
        nrow(df_distance) < 2 || 
        all(is.na(df_distance$df_distance)) || 
        all(is.na(df_distance$df_free_distance))) {
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
    
    # Extract required metrics from the results, with safety checks for each field
    result_entry <- list(
      country = country,
      year = year,
      pearson_correlation = metrics$pearson_correlation,
      spearman_correlation = metrics$spearman_correlation,
      kendall_tau = metrics$kendall_tau,
      linear_p_value = if (!is.null(metrics$linear_model_summary)) metrics$linear_model_summary$coefficients[2, "Pr(>|t|)"] else NA,
      linear_r_squared = if (!is.null(metrics$linear_model_summary)) metrics$linear_model_summary$r.squared else NA,
      mse = metrics$mse,
      rmse = metrics$rmse,
      mae = metrics$mae,
      shapiro_p_value = if (!is.null(metrics$shapiro_test)) metrics$shapiro_test$p.value else NA,
      breusch_pagan_p_value = if (!is.null(metrics$breusch_pagan_test)) metrics$breusch_pagan_test$p.value else NA,
      reset_test_p_value = if (!is.null(metrics$reset_test)) metrics$reset_test$p.value else NA,
      quadratic_p_value = if (!is.null(metrics$quadratic_model_summary)) metrics$quadratic_model_summary$coefficients["I(x^2)", "Pr(>|t|)"] else NA,
      quadratic_coeff = if (!is.null(metrics$quadratic_model_summary)) metrics$quadratic_model_summary$coefficients["I(x^2)", "Estimate"] else NA,
      aic_linear = if (!is.null(metrics$aic_values)) metrics$aic_values["Linear_Model"] else NA,
      bic_linear = if (!is.null(metrics$bic_values)) metrics$bic_values["Linear_Model"] else NA,
      aic_quadratic = if (!is.null(metrics$aic_values)) metrics$aic_values["Quadratic_Model"] else NA,
      bic_quadratic = if (!is.null(metrics$bic_values)) metrics$bic_values["Quadratic_Model"] else NA,
      anova_p_value = if (!is.null(metrics$anova_models)) metrics$anova_models$`Pr(>F)`[2] else NA
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
  results_df <- do.call(rbind, lapply(all_results, function(x) if (!is.data.frame(x)) as.data.frame(x) else x))
  
  # Save the dataframe using saveRDS
  saveRDS(results_df, output_rds_path)
  
  cat("Metrics saved to", output_rds_path, "\n")
}

# Example usage:
# Replace 'path/to/log_file.txt' with the actual path to your log file
# Replace 'path/to/output_metrics.rds' with the desired output path
log_file_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/log_file_v2.txt"
output_rds_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/exp_03_linear_metrics.rds"

############################################################# Run the processing function ############################################
#(log_file_path, output_rds_path)
process_log_and_save_metrics(log_file_path = log_file_path, output_rds_path = output_rds_path)
linear_results <- readRDS(output_rds_path)

linear_results_2_decimal <- linear_results %>% mutate(across(where(is.numeric), round, 2))
columns_to_remove <- c("shapiro_p_value", "breusch_pagan_p_value","reset_test_p_value","aic_linear","bic_linear","aic_quadratic","bic_quadratic","anova_p_value" ) # Specify the column names you want to remove
linear_results_2_decimal <- remove_columns(linear_results_2_decimal, columns_to_remove)#linear_results_2_decimal <- format_colnames_for_latex(linear_results_2_decimal)
latex_table <- kable(linear_results_2_decimal, format = "latex", longtable = TRUE, booktabs = TRUE, row.names = FALSE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  column_spec(2:ncol(linear_results_2_decimal), width = "0.8cm") # Adjust 2cm as needed to fit the page

# Save to .tex file
cat(latex_table, file = "exp3_3.tex")
