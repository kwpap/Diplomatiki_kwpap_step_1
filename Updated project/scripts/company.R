# Define the Company class
Company <- setClass("Company",
                    slots = list(name = "character",
                                 sector = "character",
                                 country = "character",
                                 bau_emissions = "numeric",
                                 free_allocation = "numeric",
                                 prod_cost_func = "function",
                                 abate_cost_func = "function",
                                 abate_cost_func_str = "character",
                                 bau_production = "numeric",
                                 new_production = "numeric",
                                 pollution_func = "function"))

# Create a constructor function for the Company class
newCompany <- function(name, sector, country, bau_emissions, free_allocation, prod_cost_func, abate_cost_func, abate_cost_func_str, bau_production, new_production, pollution_func) {
    # Create a new instance of the Company class
    company <- Company()
    
    # Set the attributes of the new Company instance
    company@name <- name
    company@sector <- sector
    company@country <- country
    company@bau_emissions <- bau_emissions
    company@free_allocation <- free_allocation
    company@prod_cost_func <- prod_cost_func
    company@abate_cost_func <- abate_cost_func
    company@abate_cost_func_str <- abate_cost_func_str
    company@bau_production <- bau_production
    company@new_production <- new_production
    company@pollution_func <- pollution_func
    
    # Return the new Company instance
    return(company)
}
# Define the quadratic function
quadratic_func <- function() {
  a <- runif(1, 0, 10)
  b <- runif(1, 0, 10)
  c <- runif(1, 0, 10)
  func <- function(x) a * x^2 + b * x + c
  func_str <- paste(a, "* x^2 + ", b, "* x + ", c)
  return(list(func = func, func_str = func_str))
}

# Example of company creation 
company_list <- list()
sectors <- c("A", "B", "C", "D")
countries <- c("A", "B", "C", "D")
for (i in 1:20) {
    temp <- quadratic_func()  # Abatement cost function of the company
    company <- newCompany(paste0("Company ", i),  # Name of the company
                          sector =  sample(sectors, 1),  # Random sector of the company
                          country = sample(countries, 1),  # Country of the company
                          bau_emissions = sample(80:100,1),  # BAU emissions of the company
                          free_allocation = -1,  # Free allocation of the company
                          prod_cost_func = function(x) 0,  # Production cost function of the company
                          abate_cost_func = temp$func ,  # Random quadratic abatement cost function of the company
                          abate_cost_func_str = temp$func_str,  # String representation of the abatement cost function
                          bau_production = sample(1000:5000, 1),  # BAU production of the company
                          new_production = -1,  # New production of the company
                          pollution_func = function(x) x * runif(1,0.9,1.3))  # Pollution function of the company
    company_list[[i]] <- company
}

setMethod("show", "Company", function(object) {
  cat("Company Name:", object@name, "\n")
  cat("Sector:", object@sector, "\n")
  cat("Country:", object@country, "\n")
  cat("Free Allocation:", object@free_allocation, "\n")
  cat("BAU Emissions:", object@bau_emissions, "\n")
  cat("Production Cost Function:", deparse(object@prod_cost_func), "\n")
  cat("Abatement Cost Function:", object@abate_cost_func_str, "\n")
  cat("Production:", object@bau_production, "\n")
  cat("New Production:", object@new_production, "\n")
  cat("Pollution Function:", deparse(object@pollution_func), "\n")
  cat("Pollution Function:", object@pollution_func(10)/10, " * x", "\n")
})

show(company_list)

total_cap <- 10000  # Total cap of the system
# Allocate the free allowances to the companies with proportion to their BAU production
    # Calculate the total BAU production
    total_bau_production <- 0
    for (company in company_list) {
        total_bau_production <- total_bau_production + company@bau_production
    }
    
    # Allocate the free allowances to the companies with proportion to their BAU production
    for (i in seq_along(company_list)) {
        company_list[[i]]@free_allocation <- company_list[[i]]@bau_production / total_bau_production * total_cap
    }

# Solve the convex optimization problem for each company to find the optimal abatement and production level
