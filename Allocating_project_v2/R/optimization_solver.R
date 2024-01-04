# Optimization solver script
# Implement the logic to optimize the firms' production, pricing, etc.

library(CVXR)

solve_optimization_problem <- function(company, price_of_product, permit_price) {
    # Decision Variables
    new_x_i <- Variable(1)
    new_q_i <- Variable(1)

    # Objective Function
    revenue <- new_q_i * price_of_product
    permit_revenue <- permit_price * (company$free_allocation - new_x_i)
    prod_cost <- company$prod_cost_func(new_q_i)
    abate_cost <- company$abate_cost_func((company$bau_emissions/company$bau_production - new_x_i - new_q_i) / company$bau_emissions)

    objective <- Maximize(revenue - permit_revenue - prod_cost - abate_cost)

    # Constraints
    constraints <- list(new_x_i >= 0, new_q_i >= 0)  # Add more constraints as necessary

    # Problem Definition
    problem <- Problem(objective, constraints)

    # Solve the Problem
    result <- solve(problem)

    # Update Company Object
    company$new_emissions <- result$getValue(new_x_i)
    company$new_production <- result$getValue(new_q_i)
}

## Example: Running the optimization for each company
#price_of_product <- 10  # Example price
#permit_price <- 2  # Price per permit

#for (company in company_list) {
#    solve_optimization_problem(company, price_of_product, permit_price)
#}


# Create a function that manipulates the company list with the new values
optimize_firms <- function(company_list, price_of_product, permit_price) {
    no_change <- TRUE
    old_new_emissions <- sapply(company_list, function(company) company$new_emissions)
    old_new_production <- sapply(company_list, function(company) company$new_production)
    while (no_change) {
        for (company in company_list) {
            solve_optimization_problem(company, price_of_product, permit_price)
        }
        new_new_emissions <- sapply(company_list, function(company) company$new_emissions)
        new_new_production <- sapply(company_list, function(company) company$new_production)

        # Replace NAs with 0
        print(paste0("new_emissions: ", new_new_emissions))
        print(paste0("new_production: ", new_new_production))
        new_new_emissions[is.na(new_new_emissions)] <- 0
        new_new_production[is.na(new_new_production)] <- 0

        if (all(abs(old_new_emissions - new_new_emissions) < 0.5 ) && all(abs(old_new_production - new_new_production) < 0.5)) {
            no_change <- FALSE
        } else {
            old_new_emissions <- new_new_emissions
            old_new_production <- new_new_production
        }
    }
    return(company_list)
}
