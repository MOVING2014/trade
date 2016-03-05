setwd("/Users/moving/Documents/Rprojects/Trading")
source("src/read_data_from_dbH2.R")
CreateTableInH2 <- function(conn,
                        spe = "Y0"
                        ){
  
  sql <- paste0("create table if not exists ",spe,"(
		OPEN int,
		CLOSE int,
		HIGH int,
		LOW int,
		VOLUME int,
		DATE date unique )")
  
  dbSendUpdate(conn,sql)
  sql <- paste0("drop table if exists tmp_",spe)
  dbSendUpdate(conn,sql)
  db.write.succ <- dbWriteTable(con,paste0("tmp_",spe),ReadDataFromSina(spe))
  

  if (db.write.succ){
  sql <- paste0("merge into ",spe ,
                  " Key(Date) select * from tmp_", spe) 
  dbSendUpdate(conn,sql)
  
  sql <- paste0("drop table if exists tmp_",spe)
  dbSendUpdate(conn,sql)
  }
}


####UpdateTableInH2
con <- dbConnectH2()
variales <- c("TA0","RB0","L0","Y0","m0","a0","c0","P0","V0","SR0","AG0","CF0")
for(spe in variales){
CreateTableInH2(con,spe)
}
sql <- "
  drop table if exists merged;
create table merged as 
select a0.date as DATE
,a0.CLOSE as a0
,AG0.CLOSE as AG0
,C0.CLOSE as C0
,CF0.CLOSE as CF0
,L0.CLOSE as L0
,M0.CLOSE as M0
,P0.CLOSE as P0
,RB0.CLOSE as RB0
,SR0.CLOSE as SR0
,TA0.CLOSE as TA0
,V0.CLOSE as V0
,Y0.CLOSE as Y0
from a0
left join AG0 on a0.date = AG0.date
left join C0 on a0.date = c0.date
left join CF0 on a0.date = cf0.date
left join L0 on a0.date = L0.date
left join M0 on a0.date = M0.date
left join P0 on a0.date = P0.date
left join RB0 on a0.date = RB0.date
left join SR0 on a0.date = SR0.date
left join TA0 on a0.date = TA0.date
left join V0 on a0.date = V0.date
left join Y0 on a0.date = Y0.date
;"
dbSendUpdate(con,sql)
dbDisconnect(con)

####


# 
# xx <- function(){
# con <- dbConnectH2()
# x<-ReadDataFromH2(con,"select * from Y0 limit 10")
# dbDisconnect(con)
# x
# }
