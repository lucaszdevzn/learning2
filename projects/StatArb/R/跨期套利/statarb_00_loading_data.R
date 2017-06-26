################################################################################
## STEP 0: 初始化，载入包，设定初始条件
################################################################################
rm(list = ls())

source('/home/fl/William/Codes/my_ini.R')

################################################################################
mysql <- dbConnect(MySQL(), dbname = "china_futures_TD", host="127.0.0.1",
                   user = "fl", password = "abc@123")

dt <- dbGetQuery(mysql, "SELECT * FROM DC_minute
                 WHERE LEFT(InstrumentID,2) = 'IF'") %>% as.data.table() %>%
  .[order(NumericExchTime), by = .(TradingDay, InstrumentID)]

ih_prod <- dt[grep("^IH",InstrumentID)][,.(Time = as.POSIXct(paste(TradingDay,Minute), '%Y-%m-%d hh:MM:ss') ,
                                          OpenPrice,HighPrice,LowPrice,ClosePrice)]

# Thu Dec 15 10:53:26 2016 ------------------------------








