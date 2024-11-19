# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/All_in_on.R")
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/linear_tester.R")
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
# Define a function to perform linearity tests on given data and extract metrics
process_linearity_tests <- function(df_for_checks, year) {
  # Perform linearity checks and get the metrics
  metrics <- linearity_checks(df_for_checks)
  
  # If linearity_checks returned NULL, exclude this entry
  if (is.null(metrics)) {
    message(paste("Linearity checks failed for", year))
    return(NULL)
  }
  
  # Extract required metrics from the results
  result_entry <- list(
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
}

# Generate a dataframe with required columns for linearity checks for a given year
prepare_data_for_year <- function(year) {
  find_slopes(year = year)$data[, 1:2]
}

# Process and compile results for a specified range of years
compile_results <- function(start_year, end_year) {
  all_results <- list()
  
  for (year in start_year:end_year) {
    df_for_checks <- prepare_data_for_year(year)
    result_entry <- tryCatch(
      process_linearity_tests(df_for_checks, year),
      error = function(e) {
        message(paste("Error processing year", year, ":", e$message))
        return(NULL)
      }
    )
    if (!is.null(result_entry)) {
      all_results <- append(all_results, list(result_entry))
    }
  }
  
  # Convert the list of results into a dataframe
  results_df <- do.call(rbind, lapply(all_results, as.data.frame))
  return(results_df)
}

# Round numeric results and remove specific columns for LaTeX output
prepare_for_latex <- function(results_df) {
  linear_results_2_decimal <- results_df %>% mutate(across(where(is.numeric), round, 2))
  
  # Specify columns to exclude from the LaTeX table
  columns_to_remove <- c("shapiro_p_value", "breusch_pagan_p_value", "reset_test_p_value", 
                         "aic_linear", "bic_linear", "aic_quadratic", "bic_quadratic", 
                         "anova_p_value")
  linear_results_2_decimal <- select(linear_results_2_decimal, -all_of(columns_to_remove))
  
  # Create LaTeX table with specified styling
  latex_table <- kable(linear_results_2_decimal, format = "latex", longtable = TRUE, booktabs = TRUE, row.names = FALSE) %>%
    kable_styling(latex_options = c("repeat_header")) %>%
    column_spec(2:ncol(linear_results_2_decimal), width = "1cm") # Adjust 2cm as needed to fit the page
  
  return(latex_table)
}

# Save the LaTeX table to a .tex file
save_latex_table <- function(latex_table, filename = "results_output.tex") {
  cat(latex_table, file = filename)
  message("LaTeX table saved to", filename)
}

# Example of usage
start_year <- 2005
end_year <- 2020
results_df <- compile_results(start_year, end_year)
latex_table <- prepare_for_latex(results_df)
save_latex_table(latex_table, "exp3_1.tex")
