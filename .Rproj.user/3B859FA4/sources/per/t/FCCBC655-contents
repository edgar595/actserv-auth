library(RMySQL)

# Database configuration
db_config <- list(
  host = "localhost",
  user = "root",
  password = "1234",
  port = 3306,
  dbname = "security"
)

# Connect to the database with local_infile parameter enabled
conn <- dbConnect(MySQL(), host = db_config$host, user = db_config$user, 
                  password = db_config$password, dbname = db_config$dbname,
                  local_infile = TRUE)

# Execute a query to fetch data
query <- "SELECT * FROM user_data"
user_data <- dbGetQuery(conn, query)

# Process the fetched data
print(user_data)


