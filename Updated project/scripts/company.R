
# Define the Company class
Company <- setClass("Company",
                                        slots = list(name = "character",
                                                                 sector = "character",
                                                                 country = "character",
                                                                 bau_emissions = "numeric",
                                                                 prod_cost_func = "function",
                                                                 abate_cost_func = "function"))

# Create a constructor function for the Company class
newCompany <- function(name, sector, country, bau_emissions, prod_cost_func, abate_cost_func) {
    # Create a new instance of the Company class
    company <- Company()
    
    # Set the attributes of the new Company instance
    company@name <- name
    company@sector <- sector
    company@country <- country
    company@bau_emissions <- bau_emissions
    company@prod_cost_func <- prod_cost_func
    company@abate_cost_func <- abate_cost_func
    
    # Return the new Company instance
    return(company)
}


# Example of company creation 
company_list <- list()
for (i in 1:4) {
    company <- newCompany(paste0("Company ", i),  # Name of the company
                                                sector = "B",  # Sector of the company
                                                country = "A",  # Country of the company
                                                bau_emissions = 100,  # BAU emissions of the company
                                                prod_cost_func = function(x) 10*x,  # Production cost function of the company
                                                abate_cost_func = function(x) 5*x)  # Abatement cost function of the company
    company_list[[i]] <- company
}
