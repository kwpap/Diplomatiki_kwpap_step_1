library("ggplot2")
library("stringr")
source("~/GitHub/Diplomatiki_kwpap_step_1/test.R")
library("dplyr") # Des ti mporei na kanei


df_1D$category <- ifelse(str_detect(df_1D$pair, pattern = "Germany"), "Germany", ifelse(str_detect(df_1D$pair, pattern = "Greece"), "Greece", "else"))
ggplot(data = df_1D, aes(x = df_distance, y = df_actual_distance, size = category)) + geom_point(color = "red") + 
  xlab("calculated Distance") + ylab("Free's Distance") + ggtitle( "A fancy graph", subtitle = "a fancy subtitle") #+
  #geom_hline(yintercept = 0.1, size = 0.5, aes(color = "the limit"))
  
df_1D$cat2 <- df_1D$df_distance < 0.5 & df_1D$df_distance > 0.2

ggplot(data = df_1D) + geom_point(aes(x = df_distance, y = df_actual_distance, color = category, shape = cat2)) + 
  xlab("calculated Distance") + ylab("Free's Distance") + ggtitle( "A fancy graph", subtitle = "a fancy subtitle") #+
geom_