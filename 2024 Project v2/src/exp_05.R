
# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/All_in_on.R")

#create an LP to solve the following problem:
# max \sum v_{ij}   (GDP_{ij} / verified emissions)_{ij} )  (GDP_{i} correction to PPS currency unit) 
# s.t. \sum v_{ij} = 1
#a_1 free(t-1) \leq \sum v_i \leq a_2 free(t-1)
#a_1 free(t-1) \leq \sum v_j \leq a_2 free(t-1)
#v_i = \sum_j v_{ij}
#v_i \approx p_i / Pop_total
#v_{ij} / v_i \approx GDP_{ij} / GDP_i 
#Nash welfare \max \product_i v_i
#v_{ij} = fraction of total free cap of country i in sector j 
#v_i = fraction of gree in country j
#v_j = fraction of free in sector j
#v_{ij} / v_i \approx (sector correction factor) * GDP_{ij} / GDP_i 


# #example LP

# # Set coefficients of the objective function (max 5x_1 + 7x_2)
# f.obj <- c(5, 7)

# # Set matrix corresponding to coefficients of constraints by rows
# # Do not consider the non-negative constraint; it is automatically assumed
# f.con <- matrix(c(1, 0,
#                   2, 3,
#                   1, 1), nrow = 3, byrow = TRUE)

# # Set unequality signs
# f.dir <- c("<=",
#            "<=",
#            "<=")

# # Set right hand side coefficients
# f.rhs <- c(16,
#            19,
#            8)

# # Final value (z)
# lp("max", f.obj, f.con, f.dir, f.rhs)

# # Variables final values
# lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# # Sensitivities
# lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
# lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to

# # Dual Values (first dual of the constraints and then dual of the variables)
# # Duals of the constraints and variables are mixed
# lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals

# # Duals lower and upper limits
# lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
# lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to
will_use_a1 <- TRUE
will_use_a2 <- TRUE
will_use_pop <- FALSE

# Define bounds for free allocation and population factors
bounds <- list(
  free_lower = 0.8,   # Lower bound coefficient for last year's free allocation
  free_upper = 1.2,     # Upper bound coefficient for last year's free allocation
  pop_lower = 0.5,    # Lower bound coefficient for population ratio
  pop_upper = 2       # Upper bound coefficient for population ratio
)

# Load data for specified year and next year, removing Malta
load_data <- function(year) {
  df <- read_data_2(year = year)
  df <- df[-c(17), ] # Exclude Malta
  df$Free <- df$Free / sum(df$Free)
  df$Pop_norm <- df$Population / sum(df$Population)
  return(df)
}

df_year <- load_data(year_for_comparison)
df_next_year <- load_data(year_for_comparison + 1)

# Load GDP per capita PPS data and clean
load_gdp_pps <- function(data_path, year) {
  GDPpps <- read.csv(paste0(data_path, "/tec00114_linear.csv"), header = TRUE, sep = ",")
  GDPpps <- GDPpps[-c(1:5, 9)]  # Drop unnecessary columns
  
  # EU country code to name mapping
  eu_2l_name <- data.frame(
    eu_2l = c("AL", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"), 
    eu_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom")
  )
  
  # Filter out non-EU or unwanted regions
  exclude_regions <- c("BA", "CH", "EA19", "EA20", "EU27_2007", "EU27_2020", "EU28", "IS", "JP", "ME", "MK", "NO", "RS", "TR", "US")
  GDPpps <- GDPpps[!GDPpps$geo %in% exclude_regions, ]
  
  # Map EU codes to names
  GDPpps$geo <- sapply(GDPpps$geo, function(code) {
    match <- eu_2l_name$eu_name[eu_2l_name$eu_2l == code]
    if (length(match) == 0) return(NA) else return(match)
  })
  
  # Filter for the specified year and drop unnecessary columns
  GDPpps <- GDPpps[GDPpps$TIME_PERIOD == year, c("geo", "OBS_VALUE")]
  names(GDPpps) <- c("GEO", "GDPpps")
  return(GDPpps)
}

GDPpps <- load_gdp_pps(data_path, year_for_comparison)

# Merge GDP PPS data with the main dataset
df_year <- merge(df_year, GDPpps, by = "GEO")

# Compute objective function coefficients
compute_objective <- function(df) {
  with(df, GDPpps * Population / Verified_emissions * Industry / 100)
}

f.obj <- compute_objective(df_year)

# Create constraint matrix, direction, and right-hand side
f.con <- matrix(1, nrow = 1, ncol = nrow(df_year)) # x1+ x2 x3+ x4 +x5...
f.dir <- "=" # =
f.rhs <- 1 # 1
#Ara olo mazi x1+x2+x3+... =1

# Solve the LP problem
sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution

if (will_use_a1){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$free_lower*df_year$Free)
}
if (will_use_a2){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$free_upper*df_year$Free)
}
if (will_use_pop){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  f.rhs <- c(f.rhs, a_3*df_year$Pop_norm)
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  f.rhs <- c(f.rhs, a_4*df_year$Pop_norm)
}

# Run the optimization
sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# Create data frame 'gg' by calculating columns vector-wise
################################################################### 
# gg <- data.frame(
#   Country = df_year$GEO,
#   efficiency = with(df_year, GDPpps * Population / Verified_emissions * Industry / 100),
#   last_year = df_year$Free,
#   low_free = a_1 * df_year$Free,
#   up_free = a_2 * df_year$Free,
#   pop = df_year$Pop_norm,
#   min = df_year$Pop_norm * a_3,
#   max = df_year$Pop_norm * a_4,
#   forecasted = sol,
#   change = sprintf("%.2f %%", (sol - df_next_year$Free) / df_next_year$Free * 100) # Format percentage
# )
gg <- data.frame(
  Country = df_year$GEO,
  Calculated_Efficiency = with(df_year, GDPpps * Population / Verified_emissions * Industry / 100),
  This_year_Allocation = sprintf("%.2f %%", (df_year$Free * 100)),
  next_year_Allocation = sprintf("%.2f %%", (df_next_year$Free * 100)),
  forecasted = sprintf("%.2f %%", (sol * 100)),
  change = sprintf("%.2f %%", (sol - df_next_year$Free) / df_next_year$Free * 100) # Format percentage
)
# Sort by efficiency in decreasing order
gg <- gg[order(gg$Calculated_Efficiency, decreasing = TRUE), ]

# Output LaTeX table
xtable(
  gg, 
  caption = "GDP per capita PPS", 
  label = "tab:GDPpps", 
  digits = 4, 
  include.rownames = FALSE, 
  booktabs = TRUE, 
  floating = TRUE, 
  file = "GDPpps.tex"
)
###################################################################################################
########################################################## Case 2 of Paper #######################
# Set constraint flags
will_use_a1 <- TRUE
will_use_a2 <- TRUE
will_use_pop <- TRUE  # Set to TRUE to include population-based constraints

# Define bounds for free allocation and population factors
bounds <- list(
  free_lower = 0.8,   # Lower bound coefficient for last year's free allocation (alpha_1)
  free_upper = 1.2,     # Upper bound coefficient for last year's free allocation (alpha_2)
  pop_lower = 0.5,    # Lower bound coefficient for population ratio (alpha_3)
  pop_upper = 2       # Upper bound coefficient for population ratio (alpha_4)
)

df_year <- load_data(year_for_comparison)
df_next_year <- load_data(year_for_comparison + 1)
GDPpps <- load_gdp_pps(data_path, year_for_comparison)
# Merge GDP PPS data with the main dataset
df_year <- merge(df_year, GDPpps, by = "GEO")
# Compute objective function coefficients
compute_objective <- function(df) {
  with(df, GDPpps * Population / Verified_emissions * Industry / 100)
}
f.obj <- compute_objective(df_year)

# Create constraint matrix, direction, and right-hand side
f.con <- matrix(1, nrow = 1, ncol = nrow(df_year)) # x1 + x2 + x3 + ... = 1
f.dir <- "="
f.rhs <- 1

# Add lower bound constraints based on last year's free allocation (alpha_1)
if (will_use_a1){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$free_lower * df_year$Free)
}

# Add upper bound constraints based on last year's free allocation (alpha_2)
if (will_use_a2){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$free_upper * df_year$Free)
}

# Add population-based constraints (alpha_3 and alpha_4)
if (will_use_pop){
  # Lower bound based on population share
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$pop_lower * df_year$Pop_norm)
  
  # Upper bound based on population share
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$pop_upper * df_year$Pop_norm)
}

# Solve the LP problem with all constraints
sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# Create data frame 'gg' by calculating columns vector-wise
gg <- data.frame(
  Country = df_year$GEO,
  Calculated_Efficiency = with(df_year, GDPpps * Population / Verified_emissions * Industry / 100),
  This_year_Allocation = sprintf("%.2f %%", (df_year$Free * 100)),
  next_year_Allocation = sprintf("%.2f %%", (df_next_year$Free * 100)),
  forecasted = sprintf("%.2f %%", (sol * 100)),
  change = sprintf("%.2f %%", (sol - df_next_year$Free) / df_next_year$Free * 100) # Format percentage
)

# Sort by efficiency in decreasing order
gg <- gg[order(gg$Calculated_Efficiency, decreasing = TRUE), ]

xtable_gg <- xtable(
  gg, 
  caption = "Forecasted Allocation of 2018, case 2", 
  label = "tab:Forecasted_allocation_case_2", 
  digits = 4
)

# Specify alignment using "X" for dynamic sizing
align(xtable_gg) <- c("l", "X", "X", "X", "X", "X", "X")

# Output xtable as LaTeX code
print(
  xtable_gg, 
  include.rownames = FALSE, 
  booktabs = TRUE, 
  floating = TRUE, 
  tabular.environment = "tabularx", 
  width = "\\textwidth"
)
###############################################################################
##################################################### Case 3 ##################
will_use_a1 <- TRUE
will_use_a2 <- TRUE
will_use_pop <- FALSE
will_use_development <- TRUE  # Flag to include development-based fairness constraint

# Define bounds for free allocation and population factors
bounds <- list(
  free_lower = 0.25,   # Lower bound coefficient for last year's free allocation (alpha_1)
  free_upper = 15,     # Upper bound coefficient for last year's free allocation (alpha_2)
  dev_lower = 0.5,    # Lower bound coefficient for development-based allocation (beta_1)
  dev_upper = 2    # Upper bound coefficient for development-based allocation (beta_2)
)

df_year <- load_data(year_for_comparison)
df_next_year <- load_data(year_for_comparison + 1)

GDPpps <- load_gdp_pps(data_path, year_for_comparison)

# Merge GDP PPS data with the main dataset
df_year <- merge(df_year, GDPpps, by = "GEO")

f.obj <- compute_objective(df_year)

# Create constraint matrix, direction, and right-hand side
f.con <- matrix(1, nrow = 1, ncol = nrow(df_year)) # x1 + x2 + x3 + ... = 1
f.dir <- "="
f.rhs <- 1

# Add lower bound constraints based on last year's free allocation (alpha_1)
if (will_use_a1){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$free_lower * df_year$Free)
}

# Add upper bound constraints based on last year's free allocation (alpha_2)
if (will_use_a2){
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$free_upper * df_year$Free)
}

# Add Development-Based Equity constraints (beta_1 and beta_2)
if (will_use_development){
  # Normalize GDP per capita
  df_year$GDP_per_capita_norm <- df_year$GDPpps / sum(df_year$GDPpps, na.rm = TRUE)
  
  # Calculate inverse economic capacity index D_i
  df_year$D_i <- 1 / df_year$GDP_per_capita_norm
  
  # Handle infinite values if GDP_per_capita_norm is zero
  df_year$D_i[is.infinite(df_year$D_i)] <- 0
  
  # Calculate total D_i across all countries
  D_total <- sum(df_year$D_i, na.rm = TRUE)
  
  # Calculate proportional allocation based on D_i
  allocation_proportion <- df_year$D_i / D_total
  
  # Add lower bound constraints based on D_i
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep(">=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$dev_lower * allocation_proportion)
  
  # Add upper bound constraints based on D_i
  f.con <- rbind(f.con, diag(nrow(df_year)))
  f.dir <- c(f.dir, rep("<=", nrow(df_year)))
  f.rhs <- c(f.rhs, bounds$dev_upper * allocation_proportion)
}

# Solve the LP problem with all constraints
sol <- lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# Create data frame 'gg' by calculating columns vector-wise
gg <- data.frame(
  Country = df_year$GEO,
  Calculated_Efficiency = with(df_year, GDPpps * Population / Verified_emissions * Industry / 100),
  This_year_Allocation = sprintf("%.2f %%", (df_year$Free * 100)),
  next_year_Allocation = sprintf("%.2f %%", (df_next_year$Free * 100)),
  forecasted = sprintf("%.2f %%", (sol * 100)),
  change = sprintf("%.2f %%", (sol - df_next_year$Free) / df_next_year$Free * 100) # Format percentage
)

# Sort by efficiency in decreasing order
gg <- gg[order(gg$Calculated_Efficiency, decreasing = TRUE), ]

# Output LaTeX table
print(
  xtable(
    gg, 
    caption = "Forecasted Allocation with Development-Based Fairness Constraint", 
    label = "tab:Forecasted_Allocation_Development_Fairness", 
    digits = 2,  # Set to 2 decimal points as per your requirement
    align = c("l", "X", "X", "X", "X", "X", "X")
  ), 
  include.rownames = FALSE, 
  booktabs = TRUE, 
  floating = TRUE, 
  tabular.environment = "tabularx", 
  width = "\\textwidth"
)
