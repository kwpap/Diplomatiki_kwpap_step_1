# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/All_in_on.R")
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/linear_tester.R")
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/exp_01.R")

for (year in c(2005:2020)) {
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

prepare_data_for_year_exp2 <- function (year){
  find_slopes_with_one_country(year = year)$data[,1:2]
}

compile_results_exp2 <- function(start_year, end_year) {
  all_results <- list()
  
  for (year in start_year:end_year) {
    df_for_checks <- prepare_data_for_year_exp2(year)
    result_entry <- tryCatch(
      process_linearity_tests(df_for_checks, year),
      error = function(e) {
        message(paste("Error processing year", year, ":", e$message))
        return(NULL)
      }
    )
    if (!is.null(result_entry)) {
      all_results <- append(all_results, list(find_slopes_with_one_country(year = year)$country, result_entry))
    }
  }
  
  # Convert the list of results into a dataframe
  results_df <- do.call(rbind, lapply(all_results, as.data.frame))
  return(results_df)
}


# Example of usage
start_year <- 2005
end_year <- 2020
results_df <- compile_results_exp2(start_year, end_year)
latex_table <- prepare_for_latex(results_df)
save_latex_table(latex_table, "exp3_2.tex")
