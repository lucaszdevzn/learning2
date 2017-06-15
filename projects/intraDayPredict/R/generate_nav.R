# ==============================================================================
rm(list = ls())
setwd("~/myCodes/CTA_intraday_predictability")

suppressMessages({
  source("./conf/myInit.R");
  source("./conf/myConfig.R")
})
# ==============================================================================


# ==============================================================================
startTime <- c("09:00:00","09:00:00","10:00:00","11:00:00","13:00:00","14:00:00",
			   "10:00:00")
endTime   <- c("09:30:00","10:00:00","11:00:00","13:30:00","14:30:00","14:30:00",
	           "13:30:00")

bkTestInterval <- data.table(startTime, endTime) %>%
					.[order(startTime,endTime)]

# ==============================================================================


# ==============================================================================
inSample <- fread('./data/inSample.csv') %>%
  .[,TradingDay := as.Date(TradingDay)]

dtY <- inSample %>%
  .[,.(closeRtn = (.SD[NumericExchTime <= .SD[Minute <= "15:00:00"][.N,NumericExchTime]][.N,close] -
                     .SD[NumericExchTime <= .SD[Minute <= "14:30:00"][.N,NumericExchTime]][.N,close]) /
         .SD[NumericExchTime <= .SD[Minute <= "14:30:00"][.N,NumericExchTime]][.N,close]),
    by = c('TradingDay','InstrumentID')]

dtPortRtn <- inSample %>%
  .[,.(closeRtn = (.SD[NumericExchTime <= .SD[Minute <= "15:00:00"][.N,NumericExchTime]][.N,close] -
                     .SD[NumericExchTime <= .SD[Minute <= "14:31:00"][.N,NumericExchTime]][.N,close]) /
         .SD[NumericExchTime <= .SD[Minute <= "14:31:00"][.N,NumericExchTime]][.N,close]),
    by = c('TradingDay','InstrumentID')]
# ==============================================================================

