# The main script that orchestrates the running of your project, calling other scripts as needed.

source("data_generation.R")
source("model_setup.R")
source("optimization_solver.R")
source("analysis.R")

# Run the model
firms <- model_setup()
optimized_firms <- optimize_firms(firms)
analyze_results(optimized_firms)
