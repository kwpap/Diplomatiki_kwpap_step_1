source("read_data.R")
source("read_free.R")
source("find_slopes.R")



find_the_best_combo <- function(){
  weights <- rep(1,8)
  r_squared <- summary(find_slopes()$linear)$r.squared
  step <- 1
  low <- 0
  high <- 10
  for (i in 1:40){
    # index <- i %% 8 +1
    index <- sample(1:8, 1)
    worth_doing_it <- TRUE
    while(worth_doing_it){
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- summary(find_slopes_with_weights(weights)$linear)$r.squared
        weights[index] <- weights[index] + step
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- summary(find_slopes_with_weights(weights)$linear)$r.squared
        weights[index] <- weights[index] - step
      }
      if (lowered > r_squared){
        r_squared <- lowered
        weights[index] <- weights[index] - step
      } else if (raised > r_squared){
        r_squared <- raised
        weights[index] <- weights[index] + step
      } else {
        worth_doing_it <- FALSE
      }
    }
  }
  print(weights)
  print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  print(paste("R^2:", r_squared))
}
# create_graph(weights=c(100,0,0,51,626,1,401,1001))

# create an average allocation comparing to the median of all.

find_the_best_combo_with_one <- function(){
  weights <- rep(1,8)
  r_squared <- summary(find_slopes_with_one_country_with_weights(weights = weights)$linear)$r.squared
  step <- 0.1
  low <- 0
  high <- 10
  for (i in 1:40){
    # index <- i %% 8 +1
    index <- sample(1:8, 1)
    worth_doing_it <- TRUE
    while(worth_doing_it){
      lowered <- 0
      raised <- 0
      if (weights[index]>low+step){
        weights[index] <- weights[index] - step
        lowered <- summary(find_slopes_with_one_country_with_weights(weights = weights)$linear)$r.squared
        weights[index] <- weights[index] + step
      }
      if (weights[index]<high){
        weights[index] <- weights[index] + step
        raised <- summary(find_slopes_with_one_country_with_weights(weights = weights)$linear)$r.squared
        weights[index] <- weights[index] - step
      }
      if (lowered > r_squared){
        r_squared <- lowered
        weights[index] <- weights[index] - step
      } else if (raised > r_squared){
        r_squared <- raised
        weights[index] <- weights[index] + step
      } else {
        worth_doing_it <- FALSE
      }
    }
  }
  print(weights)
  print(paste("Population: ", weights[1], "GDPpc: ", weights[2], "Inflation: ", weights[3], "Agriculture: ", weights[4], "Industry: ", weights[5], "Manufacturing: ", weights[6], "Total Energy Supply: ", weights[7], "Verified Emissions: ", weights[8]))
  print(paste("R^2:", r_squared))
}
