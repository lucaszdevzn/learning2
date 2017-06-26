rm(list = ls())

library(data.table)
library(plotly)
library(RMySQL)
library(magrittr)
library(zoo)
################################################################################
## Step1: 初始参数设置
################################################################################
product <- 'cu'
cols <- c("TradingDay", "InstrumentID", "Minute", "NumericExchTime", "OpenPrice"
          , "HighPrice", "LowPrice", "ClosePrice", "Volume", "Turnover")

beginDate <- '20160501'
endDate <- '20161103'

# Thu Dec 15 14:13:42 2016 ------------------------------
mysql <- dbConnect(MySQL(), host = '127.0.0.1', dbname = 'china_futures_TD',
                   user = 'fl', password = 'abc@123')

dt <- dbGetQuery(mysql,
                 paste("SELECT", paste(cols,collapse = ","), "FROM DC_minute",
                       "WHERE LEFT(InstrumentID,2) = ", paste0("'",product,"'"),
                       "AND ( TradingDay BETWEEN", beginDate, "AND", endDate, ")")
                 ) %>% as.data.table()

# Thu Dec 15 14:34:16 2016 ------------------------------
mysql <- dbConnect(MySQL(), host = '127.0.0.1', dbname = 'dev',
                   user = 'fl', password = 'abc@123')
dbListTables(mysql)
futures_calendar <- dbGetQuery(mysql, "SELECT * FROM ChinaFuturesCalendar") %>%
  as.data.table()

# Wed Dec 14 15:43:23 2016 ------------------------------
# 计算分钟的时间周期

myMinute <- data.table(
  hour   = seq(0,1440-1,by=1) %/% 60,
  minute = (seq(0,1440-1,by=1) %% 60) %/% 1,
  #  second = (seq(0,86399,by=1) %% 3600) %% 60,
  id     = seq(1,1440,by=1)
)

myMinute <- rbind(myMinute[id %between% c(21*60+1, 23*60+59*1+1)],                  ## 09:00 -- 11:30
                  myMinute[id %between% c(0*60+1, 2*60+30*1)],                  ## 09:00 -- 11:30
                  myMinute[id %between% c(9*60+1, 11*60+30*1)],                  ## 09:00 -- 11:30
                  myMinute[id %between% c(13*60+1, 15*60+15*1)]                  ## 13:00 -- 15:15
) %>%
  .[,':='(
    trading_period = paste(sprintf('%02d', hour),
                           sprintf('%02d', minute),
                           #                           sprintf('%02d', second),
                           sep = ':')
  )] %>%
  .[,Minute := paste0(trading_period,":00")]

library(parallel)
no.cores <- 8
cl <- makeCluster(no.cores)
clusterExport(cl, c("myMinute","futures_calendar"))
clusterEvalQ(cl, {library(data.table); library(magrittr)})
NumericTime <- parSapply(cl, 1:nrow(myMinute), function(ii){
  strsplit(myMinute[ii,Minute],":")[[1]] %>% sapply(.,as.numeric) %*% c(3600,60,1) -
    ifelse(strsplit(myMinute[ii,Minute],":")[[1]][1] < '18', 0, 86400)
})

stopCluster(cl)

myMinute[,NumericTime := NumericTime] %>%
  .[,":="(hour = NULL, minute = NULL, id = NULL, trading_period = NULL)]

tradingTime <- CJ(TradingDay = unique(dt$TradingDay),
                  Minute = myMinute$Minute)

# Fri Dec 16 10:48:57 2016 ------------------------------

main_prod <- dt[,.SD[,.(sum_Volume = sum(Volume), sum_Turnover = sum(Turnover))],
                by = .(TradingDay, InstrumentID)] %>%
  .[,.SD[which.max(sum_Turnover)], by = .(TradingDay)]

##
temp <- lapply(unique(dt$InstrumentID),function(ii){
  dt[InstrumentID == ii]
})

ii=1
print(ii)
  startDate = which(tradingTime$TradingDay == temp[ii][[1]][1,TradingDay])[1]
  endDate  = which(tradingTime$TradingDay == temp[ii][[1]][.N,TradingDay]) %>% .[length(.)]
  y <- merge(temp[ii][[1]], tradingTime[startDate:endDate],
        by = c('TradingDay','Minute'), all = TRUE)
  y[is.na(InstrumentID), InstrumentID := unique(y$InstrumentID[!is.na(y$InstrumentID)])] %>%
  .[myMinute, on = .(Minute)]




# full join

x <- merge(dt,tradingTime, by = c("TradingDay", 'Minute'), all = TRUE)

# Fri Dec 16 10:51:04 2016 ------------------------------
require(lubridate)
for(i in seq(1:11)){
  lag_month <- i
  v <- main_prod$InstrumentID
  u <- sapply(1:length(v), function(ii){
    temp <- v[ii] %>% gsub('[a-zA-Z].','20',.) %>%
      as.yearmon(.,"%Y%m") %>% as.Date()

    temp <- temp %m+% months(lag_month) %>% as.character() %>% gsub('-','',.) %>% substr(.,3,6) %>%
      paste0(v[ii] %>% gsub('[0-9].','',.), .)

  })

  main_prod[, sub_main := u]

  # Wed Dec 14 16:41:45 2016 ------------------------------

  main_info <- dt[main_prod, on = .(TradingDay,InstrumentID)]
  sub_info <- dt[main_prod, on = .(TradingDay,InstrumentID = sub_main)]

  plot(x = main_info$TradingTime, y = main_info$ClosePrice, type = 'l', col = 'blue',
       main = paste("The spread of months:", lag_month))

  lines(x = sub_info$TradingTime, sub_info$ClosePrice, type = 'l', col = 'red')
  Sys.sleep(5)
}


# Fri Dec 16 10:08:25 2016 ------------------------------
# 转化 TradingDay
library(parallel)
no.cores <- 10
cl <- makeCluster(no.cores)
clusterExport(cl, c("dt","futures_calendar"))
clusterEvalQ(cl, {library(data.table); library(magrittr)})

TradingTime <- parSapply(cl, 1:nrow(dt), function(ii){
  temp <- ifelse(dt[ii,NumericExchTime] < 0, futures_calendar[days == dt[ii,TradingDay], nights],
                 dt[ii,TradingDay])
})

NumericTime <- parSapply(cl, 1:nrow(dt), function(ii){
  strsplit(dt[ii,Minute],":")[[1]] %>% sapply(.,as.numeric) %*% c(3600,60,1) -
    ifelse(strsplit(dt[ii,Minute],":")[[1]][1] < '18', 0, 86400)
})

stopCluster(cl)

dt[, ":="(TradingTime = TradingTime, NumericTime = NumericTime)]


