################################################################################
## StatArb_Calendar_01_fetch_data.R
##
## 1. 参数设置
## 2. 提取数据库的数据
##
## Input:
## @product: 需要处理的合约品种
## @leadMonth: 近月合约与远月合约的月份差
## @estimatePeriod: 参数估计使用的长度，以分钟计算
## @forecastPeriod: 做预测的时间长度
##
## Output:
## @dt
################################################################################

## =============================================================================
## 1. 参数设置
## =============================================================================

## 品种
product <- 'i'

## 近月合约与远月合约的月份差
leadMonth <- 4

## 参数估计使用的区间长度：分钟
estimatePeriod <- 100

## 预测区间长度：分钟
forecastPeriod <- 60

## =============================================================================
## 2. 从数据库提取数据
## =============================================================================

mysql <- mysqlFetch('china_futures_bar')

query <- paste0("SELECT DISTINCT main_contract
                FROM main_contract_daily
                WHERE Product = ", "'", product,"'",
                " AND TradingDay >= 20140101")
temp <- dbGetQuery(mysql,query) %>% as.data.table() %>% .[-c(1:1)] # 去掉第一个
print(temp)

productInfo <- data.table()
for (i in 1:nrow(temp)) {
    ## -------------------------------------------------------------------------
    tempMain <- temp[i,main_contract]
    tempSub <- gsub('[a-zA-Z]', '', tempMain) %>%
               as.yearmon(.,'%y%m')  %>%
               as.Date(.) %m+% months(leadMonth) %>%
               as.character() %>%
               gsub('-','',.) %>%
               substr(.,3,6) %>%
               paste0(product,.)
    query <- paste0("select min(TradingDay), max(TradingDay)
                    from main_contract_daily
                    where main_contract = ", "'", tempMain, "'")
    tempRes <- dbGetQuery(mysql, query)
    ## -------------------------------------------------------------------------
    productInfo <- rbind(productInfo,
        data.table(tempRes[1,1], tempRes[1,2], tempMain, tempSub))
}
names(productInfo) <- c('beginDate', 'endDate', 'mainContract', 'subContract')
# productInfo <- productInfo[-1]
print(productInfo)

tempFields <- c('TradingDay', 'Minute', 'NumericExchTime',
                'InstrumentID',
                'OpenPrice as open',
                'HighPrice as high',
                'LowPrice as low',
                'ClosePrice as close',
                'Volume as volume', 'Turnover as turnover')

## -----------------------------------------------------------------------------
## 主力合约数据：近月合约　--> y
dtMain <- lapply(1:nrow(productInfo), function(i){
    query <- paste("select", paste(tempFields, collapse = ','),
                   "from minute
                    where TradingDay between",productInfo[i, gsub('-','',beginDate)],
                    "and ",productInfo[i, gsub('-','',endDate)],
                    "and InstrumentID =", productInfo[i,paste0("'",mainContract,"'")])
    tempRes <- dbGetQuery(mysql,query) %>% as.data.table()
}) %>% rbindlist()

## -----------------------------------------------------------------------------
## 次主力合约数据：远月合约 --> x
dtSub <- lapply(1:nrow(productInfo), function(i){
    query <- paste("select", paste(tempFields, collapse = ','),
                   "from minute
                    where TradingDay between",productInfo[i, gsub('-','',beginDate)],
                    "and ",productInfo[i, gsub('-','',endDate)],
                    "and InstrumentID =", productInfo[i,paste0("'",subContract,"'")])
    tempRes <- dbGetQuery(mysql,query) %>% as.data.table()
}) %>% rbindlist()

## -----------------------------------------------------------------------------
## 把数据拼接在一起，
## 并按照日期与分钟做排序，
## 形成对齐的时间序列数据
dt <- merge(dtSub, dtMain, by = c('TradingDay','Minute','NumericExchTime')) %>%
        .[order(TradingDay, NumericExchTime, InstrumentID.x, InstrumentID.y)]

## -----------------------------------------------------------------------------
## 把价格的走势图拿出来溜溜
tempDT <- dt[,.(minuteIndex = 1:nrow(dt), close.x,　close.y)]

## 使用　gather 函数拼接
temp <- gather(tempDT,instrumentID,closePrice, -minuteIndex) %>% as.data.table()

p <- ggplot(temp, aes(x = minuteIndex, y = closePrice, color = instrumentID)) +
  geom_line() +
  labs(title = paste(product,'==> 近月合约与远月合约的分钟 closePrice'),
       x = 'minuteIndex', y = 'closePrice',
       caption = '@williamfang')
print(p)
