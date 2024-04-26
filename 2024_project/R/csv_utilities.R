library(readr)
library(dplyr)

# Function to read and process CSV files
read_and_process_csv <- function(file_name, path = "../data", columns = NULL, rows = NULL, skip = 0, transpose = FALSE, rename_cols = NULL) {
  # Construct the full file path
  full_path <- file.path(path, file_name)
  
  # Read the CSV file
  data <- read_csv(full_path, skip = skip, col_names = !is.null(rename_cols))
  
  # If rename_cols is provided, rename the columns
  if (!is.null(rename_cols)) {
    colnames(data) <- rename_cols
  }
  
  # Subset the data by columns if specified
  if (!is.null(columns)) {
    data <- select(data, all_of(columns))
  }
  
  # Subset the data by rows if specified
  if (!is.null(rows)) {
    data <- slice(data, rows)
  }
  
  # Transpose the data if required
  if (transpose) {
    data <- as.data.frame(t(data))
    # Handle column names post-transposition if needed
  }
  
  return(data)
}