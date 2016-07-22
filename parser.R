rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(httr)
library(foreach)
library(varhandle)


#orgURL = 'http://www.tse.com.tw/device/ch/trading/fund/TWT38U/TWT38U.php' # 外資
orgURL = 'http://www.tse.com.tw/device/ch/trading/fund/TWT44U/TWT44U.php' # 投信


startDate = as.Date('2012/12/28')
endDate = as.Date('2016/01/04')
parserDate = seq(startDate, endDate, by = "day")

foreach(i=parserDate) %do% {
  
  html = getURL(orgURL, ssl.verifypeer = FALSE)
  inputDate = as.character(i)
  splitDate = unlist(strsplit(inputDate,"-"))
  splitDate[1] = as.numeric(splitDate[1])-1911
  inputDate = paste(splitDate[1], splitDate[2], splitDate[3], sep='/')
  xml = content(GET(orgURL, config=set_cookies('input_date'=inputDate,'sorting'='by_issue')),'text',  ssl.verifypeer = FALSE, encoding='UTF-8')
  
  returnDate = xpathSApply(htmlParse(xml), "//span[1]/input//@value") #htmlPase: 型態轉換
  splitDate = unlist(strsplit(returnDate,"/"))
  splitDate[1] = as.numeric(splitDate[1])+1911
  returnDate = as.Date(paste(splitDate[1], splitDate[2], splitDate[3], sep='/'))
  
  if (i==returnDate) {
    
    tables = readHTMLTable(xml)
    tableContent = as.data.frame(tables[[1]])
    names(tableContent) = c('label', 'number', 'name', 'buy', 'sale', 'net')
    
    positiveData = data.frame()
    positiveData = tableContent[unfactor(tableContent$net)>0,]
    csvName = paste('./di/',i, '.csv', sep='')
    write.csv(positiveData, csvName, row.names=FALSE)
#    returnData = as.character(returnData)
    write.table(returnDate, './di/list.csv', append=TRUE, row.names=FALSE, col.names=FALSE)
    
  }
  
        
}
