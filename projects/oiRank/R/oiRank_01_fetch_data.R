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
    ") %>% as.data.table()

    write.fst(dtOiRank, tempFile)
}

dtOiRank <- read.fst(tempFile, as.data.table=TRUE)
## =============================================================================
