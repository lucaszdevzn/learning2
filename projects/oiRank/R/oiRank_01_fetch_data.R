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
