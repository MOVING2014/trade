require("ggplot2")
ReadDataFromH2Delta <- function(spe = "Y0"){
  con <- dbConnectH2()
  sql <- paste0("select 
  y1.date , y1.close as close,
  y1.close - y1.open as CO, 
  y1.high - y1.open as HO,
  y1.high - y1.LOW as HL,
  y1.LOW - y1.OPEN as LO,
  y1.close - ylast.close as CC,
  y1.open - ylast.close as OC,
  substr(y1.date,1,4) as year
  from "
  ,spe, 
  " y1 join "
  ,spe,"  ylast on y1.date = ylast.date + 1 
  where y1.close - ylast.close is not null" )
  result <- ReadDataFromH2(con,sql)
  dbDisconnect(con)
  result 
}

DensityPlot <- function(delta = "CC",spe = "Y0"){

test.data <- ReadDataFromH2Delta(spe) 

year <- unique(test.data$YEAR)
plist <- list()
for(i in year){
  plot.data <- test.data[test.data$YEAR == i, ]
  boxplot.stats <- boxplot.stats(plot.data[,delta])
  sd.plot <- sd(plot.data[,delta])
  mean.plot <- mean(plot.data[,delta])
  m <- ggplot(data = plot.data,aes(x=CO))
  p <- m +  geom_histogram(aes_string(x = delta,y = "..density..")) + 
    #facet_wrap( ~ YEAR) +
    stat_function(fun = dnorm,arg = list(mean = mean.plot, sd=sd.plot), 
                  colour = "red") +
    geom_vline(xintercept = c(mean.plot-sd.plot,mean.plot+sd.plot,mean.plot),color="blue") +
    geom_vline(xintercept = boxplot.stats$stats,color="gray") +
    labs(title = paste0("m=",format(mean.plot,digits = 2),
                        "|s=",format(sd.plot,digits = 2),
                        "|ol=",length(boxplot.stats$out),
                        "|os=",format(sum(boxplot.stats$out)/nrow(plot.data),digits = 2)))+
    labs(x = paste0(i,spe,"|cnt",nrow(plot.data),"|",delta))
    
  plist[[i]] <- p
}

MultiPlotList(plist,cols = 3)
}

qqPlot <- function(delta = "CC",spe = "Y0"){
  test.data <- ReadDataFromH2Delta(spe) 
  ggplot(data=test.data) + 
    stat_qq(aes_string(sample = delta,colour="YEAR")) + 
    facet_wrap(~YEAR,scales = "free") + 
    labs(title = spe)
}

scatterPlot <- function(){
  test.data <- ReadDataFromH2Delta(spe) 
  test.data$DATE <- as.POSIXct(test.data$DATE)
  ggplot(data=test.data[test.data$DATE > '2012-01-01',]) + 
    geom_point(aes(x=DATE, y =CO),color="blue") + 
    geom_line(aes(x=DATE, y =CC),color = "red", alpha =0.6) + 
    scale_x_datetime(date_breaks  = "3 month") +
    theme(axis.text.x  = element_text(angle=90)) 
}
