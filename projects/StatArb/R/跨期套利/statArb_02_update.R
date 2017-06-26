rm(list = ls())

source('/home/fl/William/Codes/Rsettings/myInitial.R')
library(zoo)
library(lubridate)
library(RcppRoll)

# Thu Dec 15 14:34:16 2016 ------------------------------

backtesingDate    <- '20170208'
product <- 'cu'

#-------------------------------------------------------------------------------
mysql <- dbConnect(MySQL(), host = '127.0.0.1', dbname = 'dev',
                   user = 'fl', password = 'abc@123')
futures_calendar <- dbGetQuery(mysql, "SELECT * FROM ChinaFuturesCalendar") %>%
  as.data.table()

trainingStartDate <- futures_calendar %>%
  .[which(.$days == as.Date(backtesingDate,'%Y%m%d')) - 1, days]

trainingEndDate <- futures_calendar %>%
  .[which(.$days == as.Date(backtesingDate,'%Y%m%d')) - 1, days]


################################################################################
## Step1: 初始参数设置
################################################################################
cols <- c("TradingDay", "InstrumentID", "LastPrice", "Volume","Turnover"
          ,"BidPrice1","AskPrice1"
          , "UpdateTime", "DeltaVolume", "DeltaTurnover")

# Thu Dec 15 14:13:42 2016 ------------------------------
mysql <- dbConnect(MySQL(), host = '127.0.0.1', dbname = 'china_futures_HFT',
                   user = 'fl', password = 'abc@123')

dt <- dbGetQuery(mysql,
                 paste("SELECT", paste(cols,collapse = ","), "FROM CiticPublic",
                       "WHERE LEFT(InstrumentID,2) = ", paste0("'",product,"'"),
                       "AND ( TradingDay BETWEEN", gsub('-','',trainingStartDate),
                       "AND", backtesingDate, ")")
) %>% as.data.table() %>%
  .[,":="(
    TradingDay = as.Date(TradingDay,'%Y-%m-%d')
  )]

#-------------------------------------------------------------------------------

updateF <- function(x){
    x %>%
      .[UpdateTime %between% c("21:00:00","23:59:59") |
          UpdateTime %between% c("00:00:00","00:59:59") |
          UpdateTime %between% c("09:00:00","10:14:29") |
          UpdateTime %between% c("10:30:00","11:29:29") |
          UpdateTime %between% c("13:30:00","14:59:59")]
}


mySecond <- CJ(hour = seq(0,23), minute = seq(0,59), second = rep(seq(0,59),2)) %>%
  .[,id := seq(1, .N)] %>%
  .[,UpdateTime := paste(sprintf('%02d',hour), sprintf('%02d',minute),
                      sprintf('%02d',second), sep = ":")]
night_end <- "00:59"
night_end_id <- mySecond[hour == substr(night_end,1,2) %>% as.numeric() &
                        minute == substr(night_end,4,5) %>% as.numeric()][.N,id]
day_end   <- "14:59"
day_end_id <- mySecond[hour == substr(day_end,1,2) %>% as.numeric() &
                           minute == substr(day_end,4,5) %>% as.numeric()][.N,id]

mySecond <- list(mySecond[hour >= 21],
                 mySecond[id %between% c(0, night_end_id)],
                 mySecond[id %between% c(64801,82800)],
                 mySecond[id %between% c(93601,day_end_id)]) %>%
  rbindlist() %>% updateF(.)


#-------------------------------------------------------------------------------
# 交易前一天的主力合约信息
main_prod <- dt[TradingDay == trainingEndDate,
                .SD[,.(sum_Volume = sum(DeltaVolume), sum_Turnover = sum(DeltaTurnover))],
                by = .(TradingDay, InstrumentID)] %>%
  .[,.SD[which.max(sum_Turnover)], by = .(TradingDay)]

#-------------------------------------------------------------------------------
# 交易前一天的次主力合约信息
v <- main_prod$InstrumentID
lag_month <- 1
u <- sapply(1:length(v), function(ii){
  temp <- v[ii] %>% gsub('[a-zA-Z].','20',.) %>%
    as.yearmon(.,"%Y%m") %>% as.Date()

  temp <- temp %m+% months(lag_month) %>% as.character() %>% gsub('-','',.) %>% substr(.,3,6) %>%
    paste0(v[ii] %>% gsub('[0-9].','',.), .)

})

sub_prod <- u

#-------------------------------------------------------------------------------
# mainCont

mainCont <- dt[TradingDay == as.Date(backtesingDate,'%Y%m%d') &
               InstrumentID == main_prod[1,InstrumentID]] %>% updateF(.)

subCont  <- dt[TradingDay == as.Date(backtesingDate,'%Y%m%d') &
               InstrumentID == sub_prod] %>% updateF(.)


################################################################################
## 填充数据
################################################################################
cppFunction('
NumericVector na_convert(NumericVector x) {
int n = x.size();
NumericVector out(n);

for(int i = 1; i<(n+1); ++i ) {
      if (NumericVector::is_na(x[i])) {
        x[i] = x[i-1];
      }
}

return x;
}')

addMissingData <- function(x){
  temp <- merge(mySecond, x, by = c("UpdateTime"), all = TRUE) %>%
    .[!duplicated(id)]

  temp <- list(temp[UpdateTime >= "21:00:00"], temp[UpdateTime < "21:00:00"]) %>%
    rbindlist()

  cols <- c("Volume", "Turnover", "BidPrice1", "AskPrice1")
  temp[, (cols) := lapply(.SD, na_convert), .SDcol = cols]
  temp <- temp  %>%
    .[is.na(LastPrice), LastPrice := (BidPrice1 + AskPrice1)/2] %>%   ##-- LastPrice = (bid+ask)/2
    .[is.na(DeltaVolume), ":="(
      DeltaVolume = 0,
      DeltaTurnover = 0
    )]

  return(temp)
}


mainInfo <- addMissingData(mainCont) %>%
  .[, ":="(
    TradingDay = as.Date(backtesingDate,'%Y%m%d'),
    InstrumentID = main_prod[1,InstrumentID]
  )]

subInfo <- addMissingData(subCont) %>%
  .[, ":="(
    TradingDay = as.Date(backtesingDate,'%Y%m%d'),
    InstrumentID = sub_prod
  )]

################################################################################
################################################################################
layout(c(1:2))
plot(mainInfo$LastPrice, type = 'l', col = "steelblue4",
     main = backtesingDate, xaxt = 'n', xlab = "UpdateTime")
par(new = TRUE)
plot(subInfo$LastPrice, type = 'l', col = "violet",
     xaxt = 'n', yaxt = 'n', ylab = '', xlab = '')
axis(1, at = seq(1,nrow(mainInfo), by = 5000),
     labels = mainInfo[seq(1,nrow(mainInfo), by = 5000),UpdateTime])

legend('topleft',c(paste('mainInfo:',main_prod$InstrumentID),
                   paste('subInfo:',sub_prod)),
       col = c("steelblue4","violet"),
       pch = c(1,2),text.width = 1, cex=.8, bty = "n", text.col = c("steelblue4","violet"))

spread <- subInfo$LastPrice - mainInfo$LastPrice
plot(spread, type = 'l',
     col = 'skyblue1',  main = paste('Price Spread: ',sub_prod, '-',main_prod$InstrumentID),
     ylim = c(min(spread), max(spread))
     ,xaxt = 'n', xlab = "UpdateTime")
abline(h = mean(spread), col = 'gray70',
       lwd = 2, lty = 'dashed')
y <- c(rep(NA,60), roll_mean(spread,60))
par(new = TRUE)
plot(y, type = 'l',
     col = 'hotpink1',
     ylim = c(min(spread), max(spread))
     ,xaxt = 'n', xlab = '')
axis(1, at = seq(1,nrow(mainInfo), by = 5000),
     labels = mainInfo[seq(1,nrow(mainInfo), by = 5000),UpdateTime])

################################################################################
## spread
################################################################################
mainInfo_his <- dt[TradingDay == trainingStartDate &
                     InstrumentID == main_prod[1,InstrumentID]] %>% updateF(.) %>%
  addMissingData(.) %>%
  .[, ":="(
    TradingDay = as.Date(trainingStartDate),
    InstrumentID = main_prod[1,InstrumentID]
  )]

subInfo_his <- dt[TradingDay == trainingStartDate &
                    InstrumentID == sub_prod] %>% updateF(.) %>%
  addMissingData(.) %>%
  .[, ":="(
    TradingDay = as.Date(trainingStartDate),
    InstrumentID = sub_prod
  )]

spread_his <- data.table(UpdateTime = mainInfo$UpdateTime,
                         spread_his = subInfo_his$LastPrice - mainInfo_his$LastPrice
                         )
spread_his_mean <- spread_his[,mean(spread_his)]
spread_his_sd   <- spread_his[,sd(spread_his)]

################################################################################

################################################################################
upper <- spread_his_mean + 2 * spread_his_sd
lower <- spread_his_mean - 2 * spread_his_sd

signalInfo <- cbind(subInfo[,.(UpdateTime, sub_InstrumentID = InstrumentID,
                            sub_LastPrice = LastPrice,
                            sub_BidPrice1 = BidPrice1,
                            sub_AskPrice1 = AskPrice1)],
                 mainInfo[,.(main_InstrumentID = InstrumentID,
                             main_LastPrice = LastPrice,
                             main_BidPrice1 = BidPrice1,
                             main_AskPrice1 = AskPrice1)]
                 ) %>%
  .[,spread := sub_LastPrice - main_LastPrice] %>%
  .[,":="(signal = 0, return = 0)] %>%
  .[spread >= upper, signal := 1] %>%
  .[spread <= lower, signal := -1] %>%
  .[,":="(
    sub_long_cost  = NA,
    sub_short_cost = NA,
    main_long_cost  = NA,
    main_short_cost = NA
  )]

# nrow(signalInfo)
################################################################################
for( i in 2:nrow(signalInfo)){
  r_sub <- (-signalInfo[i,signal]) * (signalInfo[i,sub_LastPrice] - signalInfo[i-1,sub_LastPrice]) /
    signalInfo[i-1,sub_LastPrice] * 0.5

  r_main <- (+signalInfo[i,signal]) * (signalInfo[i,main_LastPrice] - signalInfo[i,main_LastPrice]) /
    signalInfo[i-1,main_LastPrice] * 0.5

  signalInfo$return[i] <- r_sub + r_main + signalInfo$return[i]

  pb <- txtProgressBar(min = 0, max = nrow(signalInfo), style = 3)
  setTxtProgressBar(pb,i)
}


################################################################################
for(i in 2:nrow(signalInfo)){
  i = 2

  if(signalInfo$signal == 1){
    sub_profit <- (-signalInfo$sigal) * ()
  }else{

  }

}








signalInfo[,sum(return) - .N]

################################################################################
layout(c(1:2))
plot(c(mainInfo_his$LastPrice,mainInfo$LastPrice),
     type = 'l', col = "steelblue4",
     main = backtesingDate, xaxt = 'n', xlab = "UpdateTime", ylab = 'LastPrice')
par(new = TRUE)
plot(c(subInfo_his$LastPrice, subInfo$LastPrice),
     type = 'l', col = "violet",
     xaxt = 'n', yaxt = 'n', ylab = '', xlab = '')
rect(xleft=0, xright=55680, ybottom=min(c(mainInfo_his$LastPrice,mainInfo$LastPrice))-500,
     ytop=max(c(mainInfo_his$LastPrice,mainInfo$LastPrice))+500,
     col=rgb(190, 120, 190, alpha=50, maxColorValue=255),
     border = NA)
axis(1, at = seq(1,nrow(mainInfo_his), by = 5000),
     labels = mainInfo[seq(1,nrow(mainInfo_his), by = 5000),UpdateTime]
     ,cex.axis=0.6)
axis(1, at = seq(nrow(mainInfo_his)+1,
                 nrow(mainInfo_his)+nrow(mainInfo), by = 5000),
     labels = mainInfo[seq(1,nrow(mainInfo), by = 5000),UpdateTime]
     ,cex.axis=0.6)

legend('topleft',c(paste('mainInfo:',main_prod$InstrumentID),
                   paste('subInfo:',sub_prod)),
       col = c("steelblue4","violet"),
       pch = c(1,2),text.width = 1, cex=.8, bty = "n", text.col = c("steelblue4","violet"))

plot(c(spread_his$spread_his,spread), type = 'l',
     col = 'skyblue1',  main = paste('Price Spread: ',sub_prod, '-',main_prod$InstrumentID),
     ylim = c(min(spread), max(spread))
     ,xaxt = 'n', xlab = "UpdateTime", ylab = 'Spread')
abline(h = mean(c(spread_his$spread_his,spread)), col = 'gray70',
       lwd = 2, lty = 'dashed')
abline(h = mean(c(spread_his$spread_his,spread)) +
         sd(c(spread_his$spread_his,spread)), col = 'gray80',
       lwd = 2, lty = 'dotted')
abline(h = mean(c(spread_his$spread_his,spread)) -
         sd(c(spread_his$spread_his,spread)), col = 'gray80',
       lwd = 2, lty = 'dotted')
rect(xleft=0, xright=55680*2, ybottom=mean(c(spread_his$spread_his,spread)) +
       1*sd(c(spread_his$spread_his,spread)),
     ytop= mean(c(spread_his$spread_his,spread)) -
       1*sd(c(spread_his$spread_his,spread)),
     col=rgb(50, 10, 100, alpha=53, maxColorValue=255),
     border = NA)

rect(xleft=0, xright=55680*2, ybottom=mean(c(spread_his$spread_his,spread)) +
       2*sd(c(spread_his$spread_his,spread)),
     ytop= mean(c(spread_his$spread_his,spread)) -
       2*sd(c(spread_his$spread_his,spread)),
     col=rgb(120, 250, 20, alpha=50, maxColorValue=255),
     border = NA)


y <- c(rep(NA,60), roll_mean(c(spread_his$spread_his,spread),60))
par(new = TRUE)
plot(y, type = 'l',
     col = 'hotpink1',
     ylim = c(min(spread), max(spread))
     ,xaxt = 'n', xlab = '', ylab = '')

rect(xleft=0, xright=55680, ybottom=min(c(spread_his$spread_his,spread))-500,
     ytop=max(c(spread_his$spread_his,spread))+500,
     col=rgb(190, 120, 190, alpha=50, maxColorValue=255),
     border = NA)
axis(1, at = seq(1,nrow(mainInfo_his), by = 5000),
     labels = mainInfo[seq(1,nrow(mainInfo_his), by = 5000),UpdateTime]
     ,cex.axis=0.6)
axis(1, at = seq(nrow(mainInfo_his)+1,
                 nrow(mainInfo_his)+nrow(mainInfo), by = 5000),
     labels = mainInfo[seq(1,nrow(mainInfo), by = 5000),UpdateTime]
     ,cex.axis=0.6)

plot(signalInfo[, cumprod(return)], type = 'l'
     ,col = 'steelblue', lwd = 3, xaxt = 'n')
axis(1, at = seq(1,nrow(mainInfo_his), by = 5000),
     labels =signalInfo[seq(1,nrow(mainInfo_his), by = 5000),UpdateTime]
     ,cex.axis=0.6)

