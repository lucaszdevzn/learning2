################################################################################
## oiRank_01_fetch_data.R
## 
## 从数据库获取数据
## 
## Input:
## @param tempFile: 数据文件
## 
## Output:
## @param dtOiRank：数据
## @param dtMain: 主力合约信息
## 
## Author: William Fang
## Created Date: 2017-07-24
################################################################################

## =============================================================================
## china_futures_bar
## 需要设置为 SET NAMES utf8
## =============================================================================
tempFile <- './data/oiRank.fst'

if (! file.exists(tempFile) ) {
    mysql <- mysqlFetch('china_futures_bar')
    dbSendQuery(mysql, "SET NAMES utf8")
    dtOiRank <- dbGetQuery(mysql, "
            SELECT *
            FROM oiRank
    ") %>% as.data.table() %>% 
    .[, ProductID := gsub('[0-9]', '', InstrumentID)]

    setcolorder(dtOiRank, c('TradingDay', 'InstrumentID', 'ProductID', 
                           colnames(dtOiRank)[3:(ncol(dtOiRank)-1)])
               )
    write.fst(dtOiRank, tempFile)
}

dtOiRank <- read.fst(tempFile, as.data.table=TRUE)
dtOiRank[, TradingDay := as.Date(TradingDay)]
## =============================================================================

## =============================================================================
## 因为有部分期货公司改名了，
## 所以需要用新的期货公司名称来替代原来的名称
## =============================================================================
futuresComp <- fread('./data/futures_company_names.csv')

for (i in 1:nrow(futuresComp)) {
    dtOiRank[BrokerID == futuresComp[i,oldNames], 
             BrokerID := futuresComp[i,newNames]]
}
## =============================================================================


## =============================================================================
## china_futures_bar
## =============================================================================
tempFile <- './data/main.fst'

if (! file.exists(tempFile)) {
    mysql <- mysqlFetch('china_futures_bar')
    dtMain <- dbGetQuery(mysql,"
            SELECT a.TradingDay,
                   a.InstrumentID,
                   a.ClosePrice as close,
                   a.Volume as volume,
                   a.Turnover as turnover,
                   b.Main_contract as main,
                   b.Prod_index as priceIndex
            FROM daily AS a, main_contract_daily AS b
            WHERE a.TradingDay = b.TradingDay
            AND a.InstrumentID = b.Main_contract
            # AND a.TradingDay >= 20151001
            AND a.Sector = 'allday';
    ") %>% as.data.table()

    write.fst(dtMain, tempFile)
}

dtMain <- read.fst(tempFile, as.data.table=TRUE)
dtMain[, TradingDay := as.Date(TradingDay)]
