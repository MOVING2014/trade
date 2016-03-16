library("knitr")
setwd("/Users/moving/Documents/Rprojects/Trading/")
go.markdown <- function(){
  rmarkdown::render("report/hedge_trends_monitor.Rmd", output_file="hedge_trends_monitor.pdf")
}
