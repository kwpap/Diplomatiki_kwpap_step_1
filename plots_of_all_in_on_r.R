# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/All_in_on.R")

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

for (year in c(2015:2020)) {
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
