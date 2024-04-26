# Load necessary libraries
library(RMariaDB)  # Adjust based on your database driver

# Source the config file (assuming YAML for better readability and management)
library(yaml)
config <- yaml::read_yaml("config/settings.yml")

# Function to establish a database connection
connect_to_db <- function() {
  conn <- dbConnect(MariaDB(),
                    user = config$db$user, 
                    password = config$db$password, 
                    dbname = config$db$dbname, 
                    host = config$db$host, 
                    port = config$db$port)
  return(conn)
}

# Function to close the database connection
disconnect_from_db <- function(connection) {
  dbDisconnect(connection)
}

# Function to fetch data from the database
fetch_data_from_db <- function(connection, query) {
  res <- dbSendQuery(connection, query)
  data <- dbFetch(res, n = -1)  # Fetch all rows
  dbClearResult(res)  # Clear the result set
  return(data)
}
