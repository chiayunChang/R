rm(list=ls(all=TRUE))
library(quantmod)
library(TTR)
library(foreach)
library(varhandle)
library(RCurl)

dateList = readLines('dateList.csv')
extendList = as.list(readLines('dateList2.csv'))

beforeDays = 30
afterDays = 40
volTracesDays = 5
finalList = data.frame()

foreach (i = dateList) %do% {
  #i ='2013-01-02'
  fileName = paste('./fi2days/', i, '.csv', sep='')
  #fileName = paste('./innerJoin/', i, '.csv', sep='')
  stockList = read.csv(fileName, header = TRUE)
  numberList = as.character(stockList$number)
  
  allData = data.frame()
  foreach (j = numberList) %do% {
    #j = 4551
    numberTW = paste(j, '.TW', sep='')
    currentDateFlag = which(is.element(extendList, i))
    startDate = as.character(extendList[currentDateFlag-beforeDays])
    endDate = as.character(extendList[currentDateFlag+afterDays])
    
    stockData = tryCatch(
      {
        as.data.frame(getSymbols(Symbols = numberTW, src = "yahoo", from = startDate, to = endDate, env = NULL))
      }, error=function(e) {
        # Choose a return value in case of error
      }, warning=function(w) {
        # Choose a return value in case of warning
      },
      finally={
        # 
      }
    )
    
    if(!is.null(stockData)) {
      names(stockData) = c('O', 'H', 'L', 'C', 'V', 'adj')
      stockData = stockData[stockData$V>0,] # remove the error data in holidays
      
      # 出攻擊量, 交易量> 前五日的1.5倍(至少四天成立), 當日成交量的10%以上為法人買進
      volTracesList = as.character(extendList[c((currentDateFlag-1):(currentDateFlag-volTracesDays))])
      VolCondition = sum(stockData[volTracesList,]$V*1.5 < stockData[i,]$V)
      #if (VolCondition > 3 && (unfactor(stockList[stockList$number==j,]$netFI) > 0.1*stockData[i,]$V || unfactor(stockList[stockList$number==j,]$netDI) > 0.1*stockData[i,]$V)) {
      if (VolCondition > 3 && unfactor(stockList[stockList$number==j,]$netCont2daysCurrentDay) > 0.1*stockData[i,]$V) {
        
        # 價格判斷: 站上ma10, 布林發散, 當日K線上影線少
        ma10 = runMean(stockData$adj, n = 10)
        bbBandsData = as.data.frame(BBands(stockData$adj))
        techFlag = beforeDays+1
        if (stockData[i,]$adj > ma10[techFlag] && 
            (bbBandsData[techFlag,]$up-bbBandsData[techFlag,]$dn) > 1.1*(bbBandsData[(techFlag-1),]$up-bbBandsData[(techFlag-1),]$dn) &&
            (stockData[i,]$C-stockData[i,]$O) > 2*(stockData[i,]$H-stockData[i,]$C) ) {
          
          tempData = data.frame(j, stockList[stockList$number==j,]$name, stockData[i,]$adj, stockData[endDate,]$adj)
          names(tempData) = c('number', 'name', 'buyPrice', 'salePrice')
          allData = rbind(allData, tempData)
          
          tempFinal = data.frame(j, stockList[stockList$number==j,]$name, i, stockData[i,]$adj, endDate, stockData[endDate,]$adj)
          names(tempFinal) = c('number', 'name', 'buyDate', 'buyPrice', 'saleDate', 'salePrice')
          finalList = rbind(finalList, tempFinal)
          #print(stockList[stockList$number==j,]$name)
        }
        
      }
      
    }
  }
  csvName = paste('./techAna_fi2days/',i, '.csv', sep='')
  write.csv(allData, csvName, row.names=FALSE)
}

write.csv(finalList, './techAna_fi2days/tradeList.csv', row.names=FALSE)
