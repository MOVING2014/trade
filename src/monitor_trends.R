setwd("/Users/moving/Documents/Rprojects/Trading/")
source("src/read_data_from_dbH2.R")
source("src/multi_ggplot.R")
require("ggplot2")

# product.list <-c("A0","Y0"
#                  ,"M0","C0"
#                  ,"SR0","TA0"
#                  ,"L0","P0"
#                  ,"V0","RB0")
# 
# cbn.product.list <- combn(product.list[1:length(product_list)], 2)


PlotMonitorTrend <- function(fomulars ="P0*1 + TA0*(-1)",start.date="2010-01-01"){
  con <- dbConnectH2()
  sql <- paste0("select date, 
                ",fomulars," 
                as metrics 
                from merged 
                where 
                ",fomulars," 
                is not null and date >= '",start.date,"';")
  
  index.data <- ReadDataFromH2(con,sql)
  index.data$DATE <- as.POSIXct(index.data$DATE)
  last.price <- as.character(index.data$METRICS[nrow(index.data)])
  
  p1 <-
    ggplot(data = index.data) + 
    geom_histogram(aes_string(x = "METRICS")) +
    labs(title = fomulars)+
    geom_vline(xintercept = as.integer(last.price),color="red",alpha=0.6)
  
  p2 <- 
    ggplot(data = index.data) + 
    geom_point(aes_string(x = "DATE",
                          y = "METRICS")) +
    scale_x_datetime(date_breaks  = "3 month") +
    geom_point(aes_string(x= "DATE[nrow(index.data)]",
                          y= "METRICS[nrow(index.data)]",
                          color="last.price"
                          ),
               size = 3, alpha = 0.6 ) + 
    geom_hline(aes_string(yintercept = "max(METRICS)"),color="blue",alpha=0.7)+
    geom_hline(aes_string(yintercept = "min(METRICS)"),color="blue",alpha=0.7)+
    geom_hline(aes_string(yintercept = "as.integer(last.price)"),color="blue",alpha=0.6)+
    theme(axis.text.x  = element_text(angle=90))
  
  MultiPlot(p1,p2)
  fn <- ecdf(index.data$METRICS)
  #fn(lastPrice)
  l <- list(summaries=summary(index.data),pecentiles=fn(last.price))
  dbDisconnect(con)
  l
}
PlotMonitorTrendLast90Days <- function(fomulars,...){
  
  PlotMonitorTrend(fomulars,start.date = format(Sys.Date()-90, "%Y-%m-%d"))
}

