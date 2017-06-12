

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_05'

## 需要去除的日期
truncatedTD <- 10



# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 signal

# ------------------------------------------------------------------------------
dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>%
        .[,meanVwap := c(rep(NA,truncatedTD-1), rollmean(vwap, 10)), by = 'InstrumentID'] %>%
        .[!is.na(meanVwap)] %>%
        .[, ":="(rank1 = rank(open - meanVwap, ties.method = 'max'),
                 rank2 = rank(close - vwap, ties.method = 'max')
                 ), by = 'TradingDay'] %>%
        .[,":="(signal1 = (rank1 - mean(rank1)) / sd(rank1) * nrow(.SD),
                signal2 = (rank2 - mean(rank2)) / sd(rank2) * nrow(.SD)
                 ), by = 'TradingDay'] %>%
        .[, mySignal := signal1 * signal2, by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
        
# ==================================================================================================
