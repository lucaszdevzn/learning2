################################################################################
## 策略主函数
## 用于搭建相关的配置
################################################################################

rm(list = ls())
################################################################################
## Step0: 初始参数设置
################################################################################
## temp <- dirname(parent.frame(1)$ofile)
## setwd(temp)
setwd("/home/william/Documents/myStrat/")

suppressMessages(
  suppressWarnings({
    source('./conf//Rconf/myInit.R')
    ChinaFuturesCalendar <- fread('./data/ChinaFuturesCalendar_2011_2017.csv')
  })
)

## =============================================================================
## 生成期货交易所与合约品种对应列表
## =============================================================================
dataFile <- "./data/ContractInfo_20170802.csv"

dt <- fread(dataFile, select = c('symbol','name','exchange',
                                 'productClass','size','priceTick')) %>% 
    .[! grep('SPD|\\&|.*efp$',symbol)] %>% 
    .[productClass == '期货'] %>% 
    .[, ":="(productID = gsub('[0-9]','',symbol),
             productName = gsub('[0-9]','',name))] %>% 
    .[!duplicated(.[,.(exchange,productID)])] %>% 
    .[, .(productID, productName, exchange, size, priceTick)] %>% 
    .[order(exchange)]
# dt
write_csv(dt, './data/product.csv')
