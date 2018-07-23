
# database connection
databaseConn <- function() {
  
    dbConnect(dbDriver("PostgreSQL"),
            user="app user",
            password="app password",
            dbname="database name",
            host="host location")
  
}
