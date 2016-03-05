require("RJDBC")

dbConnectH2 <- function(
  drv.class = "org.h2.Driver",
  drv.jar = "/Users/moving/Documents/Rprojects/Trading/bin/h2-1.4.191.jar",
  conn.URL = "jdbc:h2:tcp://localhost/~/Documents/Rprojects/Trading/data/dbTrade",
  user.name = "sa",
  user.pwd = "sa"
){
### JDBC driver settings
  drv <- JDBC(drv.class, drv.jar)
### Connection settings
  conn <- dbConnect(drv, conn.URL, user.name, user.pwd) 
## test
#dbListTables(conn)
  conn
}


ReadDataFromH2 <- function(con,sql="show tables",...){
dbGetQuery(conn = con, sql)
}

