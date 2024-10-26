# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/All_in_on.R")

# Create plots for all the distances from all countries to all the others
for (year in c(2005:2020)) {
  create_graph(year = year, 
               type = "svg",
               name = "01_distances_from_features_to_frees_all_countries_", 
               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/01_distnaces_all_from_all/", 
               weights = c(1,1,1,1,1,1,1,1))
  create_graph(year = year, 
               type = "png",
               name = "01_distances_from_features_to_frees_all_countries_", 
               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/01_distnaces_all_from_all/", 
               weights = c(1,1,1,1,1,1,1,1))
  
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

# Calculate the best combo of weights for atributes to maximize the R^2
find_the_best_combo_with_one()

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

find_the_best_combo_with_one()

