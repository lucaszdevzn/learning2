## =============================================================================
mysql <- mysqlFetch('china_futures_bar')
dbListTables(mysql)
dt <- dbGetQuery(mysql,"
                  select * from mainContractMinuteData
                ") %>%
  as.data.table() %>%
  .[! grep("IF|IC|IH|T|TF",InstrumentID)] %>% 
  .[,TradingDay := as.Date(TradingDay,'%Y-%m-%d')]
## =============================================================================


## =============================================================================
ourSampleYear <- c(2013, 2016, 2017)
inSample <- dt[! format(TradingDay,'%Y') %in% ourSampleYear]
# outSample <- dt[format(TradingDay,'%Y') %in% ourSampleYear]
## =============================================================================


## =============================================================================
dtY <- inSample %>%
  .[,.(closeRtn = (.SD[Minute <= "15:00:00"][.N,close] -.SD[Minute <= "14:30:00"][.N,close]) /
                   .SD[Minute <= "14:30:00"][.N,close]),
      by = c('TradingDay','InstrumentID')]
## =============================================================================
