# Load necessary libraries
library(dplyr)
library(kableExtra)

# Define the log file path
log_file_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/log_file.txt"

# Read the log file
log_lines <- readLines(log_file_path)

# Initialize an empty list to store data
data_list <- list()

# Loop over each line in the log file
for (line in log_lines) {
  # Check if the line contains "Completed analysis for Country"
  if (grepl("Completed analysis for Country:", line)) {
    # Extract the timestamp
    timestamp <- sub("^\\[\\s*(.*?)\\s*\\].*$", "\\1", line)
    # Extract the country name (handles multi-word country names)
    country <- sub(".*Completed analysis for Country:\\s*(.*?)\\s*Year:.*", "\\1", line)
    # Extract the year
    year <- as.numeric(sub(".*Year:\\s*(\\d+).*", "\\1", line))
    # Extract the weights
    weights_str <- sub(".*Weights:\\s*(.*?)\\s*-\\s*R\\^2.*", "\\1", line)
    weights <- as.numeric(unlist(strsplit(weights_str, ",\\s*")))
    # Check if we have exactly 8 weights
    if (length(weights) != 8) {
      warning(paste("Unexpected number of weights at line:", line))
      next
    }
    # Extract R^2, P-Value, and MSE
    r_squared <- as.numeric(sub(".*- R\\^2:\\s*([\\d\\.e\\-]+)\\s*-.*", "\\1", line))
    p_value <- as.numeric(sub(".*- P-Value:\\s*([\\d\\.e\\-]+)\\s*-.*", "\\1", line))
    mse <- as.numeric(sub(".*- MSE:\\s*([\\d\\.e\\-]+)\\s*$", "\\1", line))
    # Store the extracted data in a data frame
    data_list[[length(data_list) + 1]] <- data.frame(
      Timestamp = timestamp,
      Country = country,
      Year = year,
      Value1 = weights[1],
      Value2 = weights[2],
      Value3 = weights[3],
      Value4 = weights[4],
      Value5 = weights[5],
      Value6 = weights[6],
      Value7 = weights[7],
      Value8 = weights[8],
      R_Squared = r_squared,
      P_Value = p_value,
      MSE = mse
    )
  }
}

# Combine the list into a single data frame
data_df <- do.call(rbind, data_list)

# Define time periods (adjust the breaks as needed)
data_df$Period <- cut(
  data_df$Year,
  breaks = c(-Inf, 2009, 2014, Inf),
  labels = c("Period 1", "Period 2", "Period 3")
)

# Group data by Country and Period and calculate averages and standard deviations
summary_df <- data_df %>%
  group_by(Country, Period) %>%
  summarise(
    Avg_Value1 = mean(Value1, na.rm = TRUE),
    Avg_Value2 = mean(Value2, na.rm = TRUE),
    Avg_Value3 = mean(Value3, na.rm = TRUE),
    Avg_Value4 = mean(Value4, na.rm = TRUE),
    Avg_Value5 = mean(Value5, na.rm = TRUE),
    Avg_Value6 = mean(Value6, na.rm = TRUE),
    Avg_Value7 = mean(Value7, na.rm = TRUE),
    Avg_Value8 = mean(Value8, na.rm = TRUE),
    Std_Value1 = sd(Value1, na.rm = TRUE),
    Std_Value2 = sd(Value2, na.rm = TRUE),
    Std_Value3 = sd(Value3, na.rm = TRUE),
    Std_Value4 = sd(Value4, na.rm = TRUE),
    Std_Value5 = sd(Value5, na.rm = TRUE),
    Std_Value6 = sd(Value6, na.rm = TRUE),
    Std_Value7 = sd(Value7, na.rm = TRUE),
    Std_Value8 = sd(Value8, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate the maximum standard deviation for each period
summary_df <- summary_df %>%
  rowwise() %>%
  mutate(
    Max_Std = max(c_across(Std_Value1:Std_Value8), na.rm = TRUE)
  ) %>%
  ungroup()

# Prepare the table data
table_df <- summary_df %>%
  arrange(Country, Period) %>%
  select(
    Country,
    Period,
    Avg_Value1,
    Avg_Value2,
    Avg_Value3,
    Avg_Value4,
    Avg_Value5,
    Avg_Value6,
    Avg_Value7,
    Avg_Value8,
    Max_Std
  )

# Create a label for the country to display only once per group
table_df <- table_df %>%
  group_by(Country) %>%
  mutate(Country_Label = if_else(row_number() == 1, Country, "")) %>%
  ungroup()

# Rename columns for the table
kable_df <- table_df %>%
  select(
    Country = Country_Label,
    Period,
    `Value 1 (avg)` = Avg_Value1,
    `Value 2 (avg)` = Avg_Value2,
    `Value 3 (avg)` = Avg_Value3,
    `Value 4 (avg)` = Avg_Value4,
    `Value 5 (avg)` = Avg_Value5,
    `Value 6 (avg)` = Avg_Value6,
    `Value 7 (avg)` = Avg_Value7,
    `Value 8 (avg)` = Avg_Value8,
    `Max Std` = Max_Std
  )

kable_df <- kable_df %>% mutate(across(where(is.numeric), round, 2))

# Generate the LaTeX table using kableExtra
latex_table <- kable(
  kable_df,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  linesep = "",
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position", "repeat_header"),
    position = "center"
  ) %>%
  collapse_rows(
    columns = 1,
    latex_hline = "major",
    valign = "top"
  )

# Output the LaTeX table code
cat(latex_table)


############################################## NOT WORKING ###############################################


library(ggplot2)
library(fmsb)

# Define the plot path
plot_path <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/03_best_weights/"

# List of real names for weights
weight_names <- c(
  "total_energy_supply", "GDPpc", "population", 
  "inflation", "verified_emissions", "agriculture", 
  "industry", "manufacturing"
)

# Function to create a spider plot for a country
create_spider_plot <- function(data, country) {
  country = "Greece"
  # Extract data for the specified country
  country_data <- data[data$Country == country, ]
  country_data <- table_df[table_df$Country == country,]
  # Prepare radar data where each row is a weight and columns are periods
  radar_data <- data.frame(
    Period1 = as.numeric(country_data[country_data$Period == "Period 1", c("Avg_Value1", "Avg_Value2", "Avg_Value3", "Avg_Value4", "Avg_Value5", "Avg_Value6", "Avg_Value7", "Avg_Value8")]),
    Period2 = as.numeric(country_data[country_data$Period == "Period 2", c("Avg_Value1", "Avg_Value2", "Avg_Value3", "Avg_Value4", "Avg_Value5", "Avg_Value6", "Avg_Value7", "Avg_Value8")]),
    Period3 = as.numeric(country_data[country_data$Period == "Period 3", c("Avg_Value1", "Avg_Value2", "Avg_Value3", "Avg_Value4", "Avg_Value5", "Avg_Value6", "Avg_Value7", "Avg_Value8")])
  )
  
  # Add row names for each weight
  rownames(radar_data) <- weight_names
  
  # Determine max and min values for each weight across periods for scaling
  max_vals <- apply(radar_data, 1, max, na.rm = TRUE)
  min_vals <- rep(0, length(max_vals))
  
  # Bind max and min rows to radar data for proper scaling
  radar_data <- rbind(max_vals, min_vals, radar_data)
  
  # Define colors for each period
  period_colors <- c("blue", "red", "green")
  
  radar_data <- as.data.frame(t(radar_data))
  # Create the radar plot with distinct colors for each period
  radarchart(radar_data, axistype = 1, 
             pcol = period_colors, pfcol = scales::alpha(period_colors, 0.3), 
             plwd = 2, plty = 1,
             title = paste("Spider Plot for", country),
             vlcex = 0.8)
  
  
  
  
  # Save the plot
  file_name <- paste0(plot_path, "spider_", country, ".png")
  dev.copy(png, file = file_name)
  dev.off()
}

# Create a spider plot for each unique country in the dataset
unique_countries <- unique(table_df$Country)
for (country in unique_countries) {
  create_spider_plot(table_df, country)
}


#################################################### NOT WORKING ###################################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare the data
radar_data <- data.frame(
  Metric = c("total_energy_supply", "GDPpc", "population", "inflation", 
             "verified_emissions", "agriculture", "industry", "manufacturing"),
  Period1 = c(0.93637596, 0.40179822, 0.00000000, 0.01648303, 
              0.04994830, 0.00000000, 4.92976273, 0.00000000),
  Period2 = c(0.93637596, 0.40179822, 0.00000000, 0.01648303, 
              0.04994830, 0.00000000, 4.92976273, 0.00000000),
  Period3 = c(0.93637596, 0.40179822, 0.00000000, 0.01648303, 
              0.04994830, 0.00000000, 4.92976273, 0.00000000)
)

# Reshape data to long format
radar_long <- radar_data %>%
  pivot_longer(cols = starts_with("Period"), 
               names_to = "Period", 
               values_to = "Value")

# Find maximum and minimum values for scaling
max_value <- max(radar_long$Value)
min_value <- 0  # Use 0 as minimum for radar chart baseline

# Create the radar chart with ggplot2
ggplot(radar_long, aes(x = Metric, y = Value, color = Period, group = Period)) +
  geom_polygon(aes(fill = Period), alpha = 0.2) +
  geom_line(size = 1) +
  coord_polar() +
  ylim(min_value, max_value + 1) +  # Adjust max value for spacing
  labs(title = "Radar Chart for Three Periods") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("steelblue", "firebrick", "forestgreen")) +  # Colors for periods
  scale_color_manual(values = c("steelblue", "firebrick", "forestgreen"))
