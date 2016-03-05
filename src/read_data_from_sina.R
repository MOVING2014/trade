require("rjson")
require("RCurl")

ReadDataFromSina <- function(spe="y0"){
  files <- paste0(spe,"_",Sys.Date(),".csv")
  if(files%in%list.files("/tmp/")){
    yy <- read.csv(file = paste0("/tmp/",spe,"_",Sys.Date(),".csv"),colClasses = list(Date = "character"))
    return(yy)
  }
  myHttpheader <- c(
    "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
    "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language"="en-us",
    "Connection"="keep-alive",
    "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
  )  
  
  temp <- getURL(paste0("http://stock.finance.sina.com.cn/futures/api/json.php/InnerFuturesService.getInnerFuturesDailyKLine?symbol=",spe),
                 httpheader=myHttpheader)
  temp <- gsub("date","\"date\"",temp)
  temp <- gsub("open","\"open\"",temp)
  temp <- gsub("high","\"high\"",temp)
  temp <- gsub("low","\"low\"",temp)
  temp <- gsub("close","\"close\"",temp)
  temp <- gsub("volume","\"volume\"",temp)
  x = fromJSON(temp)
  xx <- data.frame()
  for(i in 1:length(x)){
    xx[i,1] <- as.integer(x[[i]]$open)
    xx[i,2] <- as.integer(x[[i]]$close)
    xx[i,3] <- as.integer(x[[i]]$high)
    xx[i,4] <- as.integer(x[[i]]$low)
    xx[i,5] <- as.integer(x[[i]]$volume)
    xx[i,6] <- x[[i]]$date
  }
  names(xx) <- c("OPEN","CLOSE","HIGH","LOW","VOLUME","Date")
  write.csv(xx,file = paste0("/tmp/",spe,"_",Sys.Date(),".csv"),row.names = FALSE)
  xx
}

