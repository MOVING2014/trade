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


`%+%` <- function(a,b) paste0(a,b)


MA <- function( pa = 10){
  function(spe = y0, papa =0){
  papa <-0
  p1 <- paste0("MA",pa)
  num <- pa
  z <- data.frame(c(rep(NA,num), spe$CLOSE[num:(nrow(spe)-1)] - spe$CLOSE[1:(nrow(spe)-num)]))
  names(z) <- p1
  z
  }
}


MA60 <- MA(60)
MA40 <- MA(40)
MA20 <- MA(20)
MA10 <- MA(10)

genMetrics <- function(metrics = "HIGH", rulel = mean){
z<-deparse(substitute(rulel))
function(spe = y0, pa = 10){
  pa <- 10
  p1 <- paste0(metrics,pa,z )
  l <- data.frame(rep(NA,nrow(spe)))
  names(l) <- p1
  
  for(i in 61:nrow(spe)){ 
    l[i,] <- rulel(sort(spe[[metrics]][(i-1):(i-60)],decreasing = TRUE)[1:10])/spe[["CLOSE"]][i-1]
  }
  l
}
}

H30 <- genMetrics(metrics = "HIGH")
L30 <- genMetrics(metrics = "LOW")
sdH30 <- genMetrics(metrics = "HIGH", rulel = sd)
sdL30 <- genMetrics(metrics = "LOW", rulel = sd)


AddParameters <- function(spe=y0, pa = 10, rules = MA){
  rules(spe, pa)
}


AddParametersAll <- function(spe=y0,l = c(H30,L30,sdH30,sdL30,MA60,MA40,MA20,MA10)){
  z <- as.data.frame(lapply(l, function(x) AddParameters(spe,rules =x)))
  m <- cbind(y0,z)
  subset(m,!is.na(MA60))
}


AddMAParameters <- function(spe = y0, MA.parameters = c(1,2,3) ){
names(MA.parameters) <- paste0("MA", MA.parameters)
as.data.frame((lapply(MA.parameters,function(num) (modelParameters(spe=spe,method = "MA",num)))))
}



ConditionTransfer <- function(td = tradingData[i,], ltd = tradingData[i-1,]
                              ,trading.fee = 2
                              ,plist = list(pSOH=0.8
                                             ,pSOBC=0.2
                                             ,pSO=0.1
                                             ,pH=0.8
                                             ,pBO=0.1
                                             ,pBOSC=0.2
                                             ,pBOH=0.8)){
  
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
  
  action <- sample(l,1,prob=plist[l])
  
  
  if(action == "pSOH"){
    tradeprice <- ltd$tradeprice
    position <- ltd$position
    Syield <- 0
    Cyield <- ltd$Cyield + Syield 
    TS <- "HOLD"
  }else if (action == "pSOBC"){
    tradeprice <- td$OPEN
    position <- 0
    Syield <- - (tradeprice - ltd$tradeprice) - trading.fee
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
    Syield <- tradeprice - ltd$tradeprice - trading.fee
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

TradeFunction <- function(spe = "y0",tradeFunction = ConditionTransfer
                          ,plist = list(pSOH=0.8
                                        ,pSOBC=0.2
                                        ,pSO=0.1
                                        ,pH=0.8
                                        ,pBO=0.1
                                        ,pBOSC=0.2
                                        ,pBOH=0.8)){

  for(i in 2:nrow(y0)){
    result <- tradeFunction(y0[i,],y0[i-1,],plist=plist)
    y0[i,c("tradeprice","position","Syield","Cyield")] <- result[[1]]
    y0[i,c("TS")] <- result[[2]]
  }
  y0
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

y0 <- TMPfunction()

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
mm <- TMPfunction(spe)
x <- AddParametersAll()
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

pf <- function(plist = list(pSOH=0.8
                ,pSOBC=0.2
                ,pSO=0.1
                ,pH=0.8
                ,pBO=0.1
                ,pBOSC=0.2
                ,pBOH=0.8)){
x <- TradeFunction(plist=plist)
x$year <- substr(x$DATE,1,4)
p1 <- ggplot(data = x, aes(x = as.POSIXct(DATE), y = Cyield,color=year)) + 
  geom_point() + 
  scale_x_datetime(date_breaks = "1 year") +
  geom_hline(yintercept = 0,color = "blue") +
  #theme(axis.text.x  = element_text(angle=90)) 
  theme(axis.text.x  = element_blank()
        ,axis.title.x = element_blank()
        ,axis.title.y = element_blank()
        ,legend.position = "none") 

p1
}

getplist <- function(pHold = 0.8){
  plist = list(pSOH=pHold
               ,pSOBC=1-pHold
               ,pSO=(1-pHold)/2
               ,pH=pHold
               ,pBO=(1-pHold)/2
               ,pBOSC=1-pHold
               ,pBOH=pHold)
  plist
}

# plist <- getplist(pHold = 0.9)
# 
# MultiPlot(cols=5,pf(),pf(),pf(),pf(),pf()
#           ,pf(),pf(),pf(),pf(),pf()
#           ,pf(),pf(),pf(),pf(),pf()
#           ,pf(),pf(),pf(),pf(),pf()
#           ,pf(),pf(),pf(),pf(),pf())
# 
# 
# ggplot(data = x, aes(x = as.POSIXct(DATE), y = Cyield,color=year)) + 
#   geom_point() + 
#   scale_x_datetime(date_breaks = "1 year") +
#   theme(axis.text.x  = element_blank()
#         ,axis.title.x = element_blank()
#         ,axis.title.y = element_blank()
#         ,legend.position = "none") 
# 
#         
# 
# ####flow####
# #1
# 
# con <- dbConnectH2()
# sql <- paste0("select * from ",spe )
# y0 <- PriceCondition(ReadDataFromH2(con,sql))
# y0 <- AddParametersAll(spe = y0)
# head(y0)
# dbDisconnect(con)
# zz <- TradeFunction(spe = "y0")
# mm <- subset(zz, TS!="HOLD")
# se <- rep(seq(2,nrow(mm),by = 2),2)
# rid <- se[order(se)]
# 
# mm$Syield[1:length(rid)] <- mm$Syield[rid]
# 
# x <- rbind(x,mm)
# nrow(x)
# 
# 
# con <- dbConnectH2()
# 
# 
# x$flag <- cut(x$Syield,breaks = c(-1000,-200,-100,-30,30,100,200,1000))
# dbSendUpdate(con,statement = "drop table mm")
# dbWriteTable(con, "mm",x)
# 
# 
# mm <- x
# buy.model.raw.data <- subset(mm, TS == "BO")
# bks <- buy.model.raw.data$Syield
# bks <- bks[order(bks)]
# bks <- bks[(1:9)*as.integer(length(bks)/10)]
# bks <- c(bks,0)
# buy.model.raw.data$flag <- cut(buy.model.raw.data$Syield,breaks = bks)
# 
# sell.model.raw.data <- subset(mm, TS == "SO")
# bks <- sell.model.raw.data$Syield
# bks <- bks[order(bks)]
# bks <- bks[(1:9)*as.integer(length(bks)/10)]
# bks <- c(bks,0)
# 
# sell.model.raw.data$flag <- cut(sell.model.raw.data$Syield,breaks = bks)
# 
# 
# 
# inds <- c("MA10","MA20","MA40","MA60","HIGH10mean","LOW10mean","HIGH10sd","LOW10sd" )
# ind <- paste0(inds,collapse = "+")
# f <- as.formula(paste0("flag~",ind))
# buy.model <- naiveBayes(f, data=buy.model.raw.data)
# 
# buy.model <- randomForest(f, data=subset(buy.model.raw.data, !is.na(flag)))
# 
# table(buy.model.raw.data$flag,predict(buy.model ,newdata=buy.model.raw.data[,inds]))
# 
# result <- predict(buy.model,newdata = y0[nrow(y0),inds],type = "vote")
# result
# 
# 
# 
# sell.model <- randomForest(f, data=subset(sell.model.raw.data,!is.na(flag)))
# table(sell.model.raw.data$flag,predict(sell.model ,newdata=sell.model.raw.data[,inds]))
# result <- predict(sell.model,newdata = y0[nrow(y0),inds],type = "prob")
# result
# 
# 
####flow model####
#2

species <- c("p0","ta0","l0","sr0","y0","m0","a0")
#####print p0
#spe <- "p0"
for(i in 1:20){
for(spe in species){

con <- dbConnectH2()
sql <- paste0("select * from ",spe )
y0 <- PriceCondition(ReadDataFromH2(con,sql))
y0 <- AddParametersAll(spe = y0)


zz <- TradeFunction(spe = spe)
mm <- subset(zz, TS!="HOLD")
se <- rep(seq(2,nrow(mm),by = 2),2)
rid <- se[order(se)]
mm$Syield[1:length(rid)] <- mm$Syield[rid]
x <- mm
for(i in 1:20){
  y0 <- PriceCondition(ReadDataFromH2(con,sql))
  y0 <- AddParametersAll(spe = y0)
  zz <- TradeFunction(spe = spe)
  mm <- subset(zz, TS!="HOLD")
  se <- rep(seq(2,nrow(mm),by = 2),2)
  rid <- se[order(se)]
  mm$Syield[1:length(rid)] <- mm$Syield[rid]
  x <- rbind(x,mm)  
}

dbDisconnect(con)
#x

mm <- x
buy.model.raw.data <- subset(mm, TS == "BO")
bks <- buy.model.raw.data$Syield
bks <- bks[order(bks)]
bks <- bks[(1:9)*as.integer(length(bks)/10)]
bks <- unique(c(bks,0))
buy.model.raw.data$flag <- cut(buy.model.raw.data$Syield,breaks = bks)

inds <- c("MA10","MA20","MA40","MA60","HIGH10mean","LOW10mean","HIGH10sd","LOW10sd" )
ind <- paste0(inds,collapse = "+")
f <- as.formula(paste0("flag~",ind))
require(randomForest)
buy.model <- randomForest(f, data=subset(buy.model.raw.data, !is.na(flag)))
table(buy.model.raw.data$flag,predict(buy.model ,newdata=buy.model.raw.data[,inds]))
z <- predict(buy.model,newdata = y0[nrow(y0),inds],type = "vote")

con <- dbConnectH2()
maxseq <- ReadDataFromH2(con,sql = "select max(seq) from randomForest where direction = 'BO' and spe = '" %+%spe%+%"'")
if(is.na(maxseq)) maxseq = 1 else maxseq = maxseq +1

today <- ReadDataFromH2(con,sql = "select max(date) from a0")[1,1]

for(i in 1:length(z)){
sql <- "insert into randomForest values ('"%+%spe%+%"','BO','"%+%dimnames(t(z))[[1]][i]%+% "'," %+% t(z)[i]%+%","%+%maxseq %+%",'"%+%today%+%"')"
dbSendUpdate(con,sql)
}
dbDisconnect(con)


mm <- x
sell.model.raw.data <- subset(mm, TS == "SO")
bks <- sell.model.raw.data$Syield
bks <- bks[order(bks)]
bks <- bks[(1:9)*as.integer(length(bks)/10)]
bks <- unique(c(bks,0))
sell.model.raw.data$flag <- cut(sell.model.raw.data$Syield,breaks = bks)

inds <- c("MA10","MA20","MA40","MA60","HIGH10mean","LOW10mean","HIGH10sd","LOW10sd" )
ind <- paste0(inds,collapse = "+")
f <- as.formula(paste0("flag~",ind))
sell.model <- randomForest(f, data=subset(sell.model.raw.data,!is.na(flag)))
print("SELL Models")
table(sell.model.raw.data$flag,predict(sell.model ,newdata=sell.model.raw.data[,inds]))
z <- predict(sell.model,newdata = y0[nrow(y0),inds],type = "prob")


con <- dbConnectH2()
maxseq <- ReadDataFromH2(con,sql = "select max(seq) from randomForest where direction = 'SO' and  spe = '" %+%spe%+%"'")
if(is.na(maxseq)) maxseq = 1 else maxseq = maxseq +1
today <- ReadDataFromH2(con,sql = "select max(date) from a0")[1,1]

for(i in 1:length(z)){
  sql <- "insert into randomForest values ('"%+%spe%+%"','SO','"%+%dimnames(t(z))[[1]][i]%+% "'," %+% t(z)[i]%+%","%+%maxseq %+%",'"%+%today%+%"')"
  dbSendUpdate(con,sql)
}
dbDisconnect(con)
}

}