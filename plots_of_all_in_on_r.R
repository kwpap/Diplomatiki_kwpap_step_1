# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/All_in_on.R")

for (year in c(2005:2020)) {
  create_graph(year = year, 
               type = "svg",
               name = "01_distances_from_features_to_frees_all_countries_", 
               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/distnaces_all_from_all/", 
               weights = c(1,1,1,1,1,1,1,1))
  create_graph(year = year, 
               type = "png",
               name = "01_distances_from_features_to_frees_all_countries_", 
               path = "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/distnaces_all_from_all/", 
               weights = c(1,1,1,1,1,1,1,1))
  
}

