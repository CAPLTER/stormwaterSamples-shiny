
# database connection
# databaseConn <- function() {
#   
#     dbConnect(dbDriver("PostgreSQL"),
#             user="app user",
#             password="app password",
#             dbname="database name",
#             host="host location")
#   
# }

# postgres local
stormPool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = "srearl",
  host = "localhost",
  user = "shiny",
  password = "shiny"
)

onStop(function() {
  poolClose(stormPool)
})
