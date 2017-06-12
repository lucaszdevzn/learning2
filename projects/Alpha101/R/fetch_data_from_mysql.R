mysql <- mysqlFetch('china_futures_bar')

sqlQuery <- "SELECT TradingDay,Product as mainProduct,
                    Main_contract as mainCont
             FROM main_contract_daily"
mainCont <- dbGetQuery(mysql, sqlQuery) %>% as.data.table() %>%
            .[,TradingDay := as.Date(TradingDay)]
# ------------------------------------------------------------------------------
# 所有的数据
# ------------------------------------------------------------------------------
allData <- dbGetQuery(mysql, "
                SELECT * 
                FROM daily") %>%
        as.data.table() %>%
        .[,":="(TradingDay = as.Date(TradingDay)
               ,Volume     = as.numeric(Volume)
               ,Turnover   = as.numeric(Turnover))] %>%
        .[order(TradingDay)]

# ------------------------------------------------------------------------------
# inSample 数据
# ------------------------------------------------------------------------------
inSample <- allData[format(TradingDay,'%Y') %in% c('2011','2012','2014','2015')]
inSample[,unique(format(TradingDay,"%Y"))]
# ------------------------------------------------------------------------------
# outSample 数据
# ------------------------------------------------------------------------------
outSample <- allData[! (TradingDay %in% inSample[,TradingDay])]
outSample[,unique(format(TradingDay,"%Y"))]

# ==========: 提取 inSample  ========================================================================
# ------------------------------------------------------------------------------
# inSample
# ------------------------------------------------------------------------------ 
inSample <- inSample[Sector == 'allday'] %>%
                .[,.(TradingDay,InstrumentID
                    ,open = OpenPrice
                    ,high = HighPrice
                    ,low  = LowPrice
                    ,close = ClosePrice
                    ,volume = Volume
                    ,turnover = Turnover
                    ,oi = CloseOpenInterest)]
## 做主力合约
mainCont
inSample <- merge(inSample, mainCont, by.x = c('TradingDay','InstrumentID'),
                                      by.y = c('TradingDay','mainCont')
                                      )

mysql <- mysqlFetch('china_futures_info')
sqlQuery <- "SELECT * FROM VolumeMultiple"
tempMultiple <- dbGetQuery(mysql, sqlQuery) %>% as.data.table() %>%
            .[,TradingDay := as.Date(TradingDay)]

inSample <- merge(inSample, tempMultiple, by = c('TradingDay', 'InstrumentID'))

tempCFFEX <- c("IF","IH","IC","T","TF") %>%
          paste0("^",.,collapse = '[0-9]+|') %>%
          paste0(.,"[0-9]+")

# ==========: 提取vwap ==============================================================================
inSample <- inSample %>%
        .[!grep("[A-Z]", InstrumentID), vwap := turnover / volume / VolumeMultiple] %>%
        .[grep("[A-Z]", InstrumentID), vwap := turnover / volume] %>%
        .[grep(tempCFFEX,InstrumentID), vwap := turnover / volume / VolumeMultiple]
        
inSample[vwap > high]
inSample[,c('oi','mainProduct','VolumeMultiple') := NULL]
# ==================================================================================================
inSample[,unique(format(TradingDay,"%Y"))]
