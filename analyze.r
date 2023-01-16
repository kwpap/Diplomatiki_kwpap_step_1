source("read_data.R")
source("read_free.R")
source("find_slopes.R")


# find_slopes <- function(will_use_log = TRUE,
#                         year_for_comparison = 2017,
#                         will_use_total_energy_supply = TRUE,
#                         will_use_inflation = TRUE,
#                         will_use_GDPpc = TRUE,
#                         will_use_population = TRUE,
#                         will_use_verified_emisions = TRUE,
#                         will_use_agriculture = TRUE,
#                         will_use_industry = TRUE,
#                         will_use_manufacturing = TRUE,
#                         will_normalise = TRUE,
#                         force_fresh_data = TRUE,
#                         use_mean_for_missing_data = TRUE)

test <- find_slopes( TRUE, 2018,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
print(paste("The slope is: ", coef(test$linear)[2], " and the intercept is: ", coef(test$linear)[1], sep = ""))

# Plot the slope for years 2005-2018

for (i in 2012:2018){
  test <- find_slopes( TRUE, i,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  print(paste("The slope is: ", coef(test$linear)[2], " and the intercept is: ", coef(test$linear)[1], sep = ""))
}
