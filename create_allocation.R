source("find_slopes.r")
source("read_data.r")
source("find_slope.r")

resaults = list()

for (i in 2013:2016){
  gg <- find_slope(read_data(year_for_comparison = i)[-c(1)], read_free(read_data(year_for_comparison = i)$GEO, year= i))
  print(gg[2])
}

