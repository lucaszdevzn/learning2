## =============================================================================
if(! "mainContMinuteData.csv" %in% list.files("./data",pattern='\\.csv')){
    mysql <- mysqlFetch('china_futures_bar')
    # dbListTables(mysql)
    dt <- dbGetQuery(mysql,"
                      select * from mainContractMinuteData
                    ") %>%
      as.data.table() %>%
      .[! grep("IF|IC|IH|T|TF",InstrumentID)] %>% 
      .[,Minute := substr(Minute, 1, 5)]
    fwrite(dt,"./data/mainContMinuteData.csv")
}
## =============================================================================


## =============================================================================
outSampleYear <- c(2013, 2016, 2017)

if(! "inSample.csv" %in% list.files("./data",pattern='\\.csv')){
    dt <- fread("./data/mainContMinuteData.csv") %>%
            .[,.SD[order(TradingDay,NumericExchTime)],by='InstrumentID']
    dt[,TradingDay := as.Date(TradingDay,'%Y-%m-%d')]

    inSample <- dt[! format(TradingDay,'%Y') %in% outSampleYear]
    fwrite(inSample,'./data/inSample.csv')
}

inSample <- fread('./data/inSample.csv') %>%
  .[,TradingDay := as.Date(TradingDay)]
## =============================================================================


## =============================================================================
dtY <- inSample %>%
  .[,.(closeRtn = (.SD[Minute <= "15:00"][.N,close] - .SD[Minute <= "14:30"][.N,close]) /
                   .SD[Minute <= "14:30"][.N,close]),
      by = c('TradingDay','InstrumentID')] %>% 
  .[!is.na(closeRtn)]
## =============================================================================
