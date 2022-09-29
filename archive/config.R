# configuration from config.yml
this_configuration <- config::get(config = "default")

# postgres dev
stormPool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = this_configuration$dbname,
  host = this_configuration$host,
  user = this_configuration$user,
  password = this_configuration$password
)
                                                                                
onStop(function() {
  poolClose(stormPool)
})
