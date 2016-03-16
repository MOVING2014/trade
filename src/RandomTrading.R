require("reshape2")
require("e1071")
source("src/read_data_from_dbH2.R")
PriceCondition <- function(df = tradingData){
  
  df[,c("LLOW","LHIGH")] <- cbind(c(0,df$"LOW"[1:(nrow(df)-1)]), c(0,df$"HIG"[1:(nrow(df)-1)]))
  df$PC <- "Normal"  
  df$PC[df$LOW == df$HIGH & df$HIGH > df$LHIGH] <- "ZT"
  df$PC[df$LOW == df$HIGH & df$HIGH < df$LLOW] <- "DT"
  
  df$tradeprice <- NA
  df$position <- 0
  df$Syield <- 0
  df$Cyield <-0
  df$TS <- "HOLD"
  
  df[-1,!names(df)%in%c("LLOW","LHIGH")]
}


ConditionTransfer <- function(td = tradingData[i,], ltd = tradingData[i-1,]){
  
  if(ltd$position == -1){
    l <- c("pSOH","pSOBC")
    if(td$PC == "ZT"){ l <- c("pSOH")}
    
  }else if(ltd$position == 0){
    l <- c("pSO","pH","pBO")
    if(td$PC == "ZT"){ l <- c("pSO","pH") }
    if(td$PC == "DT"){ l <- c("pH","pBO") }
    
  }else if(ltd$position == 1){
    l <- c("pBOSC","pBOH")
    if(td$PC == "DT"){ l <- c("pBOH")}
  }
  
#   l <- c("pSOBC","pSOH")
#   pSOBC <-0.9
#   pSOH <- 0.1
#   sample(l,1,mget(l))

  action <- sample(l,1)
  
  if(action == "pSOH"){
    tradeprice <- ltd$tradeprice
    position <- ltd$position
    Syield <- 0
    Cyield <- ltd$Cyield + Syield
    TS <- "HOLD"
  }else if (action == "pSOBC"){
    tradeprice <- td$OPEN
    position <- 0
    Syield <- - (tradeprice - ltd$tradeprice)
    Cyield <- ltd$Cyield + Syield
    TS <- "BC"
  }else if(action == "pSO"){
    tradeprice <- td$OPEN
    position <- -1
    Syield <- 0
    Cyield <- ltd$Cyield + Syield
    TS <- "SO"
  }else if(action == "pH"){
    tradeprice <- ltd$tradeprice
    position <- ltd$position
    Syield <- 0
    Cyield <- ltd$Cyield + Syield
    TS <- "HOLD"
  }else if(action == "pBO"){
    tradeprice <- td$OPEN
    position <- 1
    Syield <- 0
    Cyield <- ltd$Cyield + Syield
    TS <- "BO"
  }else if(action == "pBOSC"){
    tradeprice <- td$OPEN
    position <- 0
    Syield <- tradeprice - ltd$tradeprice
    Cyield <- ltd$Cyield + Syield
    TS <- "SC"
  }else if(action == "pBOH"){
    tradeprice <- ltd$tradeprice
    position <- ltd$position
    Syield <- 0
    Cyield <- ltd$Cyield + Syield
    TS <- "HOLD"
  }
  
  list(c(tradeprice,position,Syield,Cyield),TS)
  
  
}


TMPfunction <- function(spe = "y0"){
con <- dbConnectH2()
sql <- paste0("select * from ",spe )
y0 <- PriceCondition(ReadDataFromH2(con,sql))
#y0[3,c("tradeprice","position","Syield","Cyield","TS")] <- ConditionTransfer(y0[3,],y0[2,])


for(i in 2:nrow(y0)){
  result <- ConditionTransfer(y0[i,],y0[i-1,])
  y0[i,c("tradeprice","position","Syield","Cyield")] <- result[[1]]
  y0[i,c("TS")] <- result[[2]]
}
#plot(y0$Cyield)
dbDisconnect(con)
y0
}

#y0 <- TMPfunction()

AddParameters <- function(y0){

y0$MA60[61:nrow(y0)] <- y0$CLOSE[60:(nrow(y0)-1)] - y0$CLOSE[1:(nrow(y0)-60)]
y0$MA40[41:nrow(y0)] <- y0$CLOSE[40:(nrow(y0)-1)] - y0$CLOSE[1:(nrow(y0)-40)]
y0$MA20[21:nrow(y0)] <- y0$CLOSE[20:(nrow(y0)-1)] - y0$CLOSE[1:(nrow(y0)-20)]
y0$MA10[11:nrow(y0)] <- y0$CLOSE[10:(nrow(y0)-1)] - y0$CLOSE[1:(nrow(y0)-10)]
y0$lastCO[2:nrow(y0)] <- y0$CLOSE[1:(nrow(y0)-1)] - y0$OPEN[1:(nrow(y0)-1)]
y0$OO[2:nrow(y0)] <- y0$OPEN[2:(nrow(y0))] - y0$OPEN[1:(nrow(y0)-1)]
y0$H30 <- NA
y0$L30 <- NA
y0$stdH30 <- NA
y0$stdL30 <- NA
y0$duration <- NA

for(i in 61:nrow(y0)){
y0$H30[i] <- mean(sort(y0$HIGH[(i-1):(i-60)],decreasing = TRUE)[1:10])
y0$stdH30[i] <- sd(sort(y0$HIGH[(i-1):(i-60)],decreasing = TRUE)[1:10])
y0$L30[i] <- mean(sort(y0$LOW[(i-1):(i-60)])[1:10])
y0$stdL30[i] <- sd(sort(y0$LOW[(i-1):(i-60)])[1:10])

}

#####Filter data
y0 <- y0[61:nrow(y0),]
y0 <- y0[y0$TS != "HOLD",]
if(y0$TS[1] %in% c("BC","SC")){y0 <- y0[2:nrow(y0),]}
if(y0$TS[nrow(y0)] %in% c("BO","SO")){y0 <- y0[1:(nrow(y0)-1),]}

y0$duration[2:nrow(y0)] <- as.numeric(as.POSIXct(y0$DATE[2:nrow(y0)]) - 
  as.POSIXct(y0$DATE[1:(nrow(y0)-1)]))
for(i in seq(2,nrow(y0),by= 2)){
  y0$Syield[i-1] <- y0$Syield[i]
}
y0
}


TMPfunction2 <- function(spe="y0"){

x <- AddParameters(TMPfunction(spe))
btest <- x[,c("duration","tradeprice","MA60","MA40","MA20","MA10","H30","L30","stdH30","stdL30","TS","lastCO","OO","Syield")]
# btest$flag[btest$Syield > 0] <- "winsmall"
# btest$flag[btest$Syield > 50] <- "winmedian"
# btest$flag[btest$Syield > 100] <- "winbig"
# btest$flag[btest$Syield < 0] <- "losssmall"
# btest$flag[btest$Syield < -50] <- "lossmedian"
# btest$flag[btest$Syield < -100] <- "lossbig"

btest$flag[btest$Syield > 0 ] <- "win"
btest$flag[btest$Syield <= 0 ] <- "loss"

btest$H30 <- btest$H30 - btest$tradeprice
btest$L30 <- btest$L30 - btest$tradeprice
btest$flag <- as.factor(btest$flag)
btest <- btest[,c("MA60","MA40","MA20","MA10","H30","L30","stdH30","stdL30","TS","lastCO","OO","flag")]
mBO <- naiveBayes(flag ~ . ,data = btest[btest$TS == "BO",])
BO <- table(predict(mBO,btest[btest$TS == "BO",]),btest[btest$TS == "BO","flag"])
mBC <- naiveBayes(flag ~ . ,data = btest[btest$TS == "BC",])
BC <- table(predict(mBC,btest[btest$TS == "BC",]),btest[btest$TS == "BC","flag"])
mSO <- naiveBayes(flag ~ . ,data = btest[btest$TS == "SO",])
SO <- table(predict(mSO,btest[btest$TS == "SO",]),btest[btest$TS == "SO","flag"])
mSC <- naiveBayes(flag ~ . ,data = btest[btest$TS == "SC",])
SC <- table(predict(mSC,btest[btest$TS == "SC",]),btest[btest$TS == "SC","flag"])

SC_loss <- SC[1,1]/(SC[1,1]+SC[1,2])
SC_win <- SC[2,2]/(SC[2,2]+SC[2,1])
BC_loss <- BC[1,1]/(BC[1,1]+BC[1,2])
BC_win <- BC[2,2]/(BC[2,2]+BC[2,1])
SO_loss <- SO[1,1]/(SO[1,1]+SO[1,2])
SO_win <- SO[2,2]/(SO[2,2]+SO[2,1])
BO_loss <- BO[1,1]/(BO[1,1]+BO[1,2])
BO_win <- BO[2,2]/(BO[2,2]+BO[2,1])

l<-list(BO_win=BO_win,
     BO_loss = BO_loss,
     BC_win=BC_win,
     BC_loss = BC_loss,
     SO_win = SO_win,
     SO_loss = SO_loss,
     SC_win = SC_win,
     SC_loss = SC_loss
     )
as.data.frame(l)
}




TMPfunction3 <- function(spe = "y0"){
con <- dbConnectH2()
#dbWriteTable(con, "a0test", TMPfunction2("a0"))

for(i in 1:100){
  l <- TMPfunction2(spe)
sql <- paste0("insert into ",spe,"test values (",paste0(l,collapse=","),")")
dbSendUpdate(con,sql)
}
dbDisconnect(con)
}


plotProb <- function(spe="a0"){
con <- dbConnectH2()  
sql <- paste0("select * from ",spe,"test")
test <- ReadDataFromH2(con,sql)
x <- melt(test,value.name = "prob")
p <- ggplot(x,aes(as.factor(variable),prob))+geom_boxplot() + labs(title = spe)
print(p)
dbDisconnect(con)
}

