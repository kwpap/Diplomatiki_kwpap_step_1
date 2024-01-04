# The main script that orchestrates the running of your project, calling other scripts as needed.

#source("Allocating_project_v2/R/data_generation.R")
source("Allocating_project_v2/R/model_setup.R")
source("Allocating_project_v2/R/optimization_solver.R")
source("Allocating_project_v2/R/analysis.R")
source("Allocating_project_v2/R/AllowanceDistributor.R")

# Run the model
firms <- model_setup()
optimized_firms <- optimize_firms(firms, price_of_product = 10, permit_price = 2)
analyze_results(optimized_firms)
