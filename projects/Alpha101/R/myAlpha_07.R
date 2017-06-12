# ==============================================================================
# Ref: "my Alpha 07"   

# [
#         stddev(volume,5)
#     ] *
# rank[
#         ts_min(
#                 return(vwap,1),3
#               )
#     ]

# 算法
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_07'

## 需要去除的日期
truncatedTD <- 3
truncatedTD2 <- 7
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
    temp <- Cal_Signal1(x)
}

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, signal1 = NA)

    tempDT <- sapply((1+truncatedTD2) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD2 + 1) : .N]
        temp <- temp[,min(returnVwap)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD2),.)

    tempRes[, signal1 := tempDT]
    return(tempRes)
}

Cal_Signal2 <- function(x){
    tempRes <- cbind(x, signal2 = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,sd(volume)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, signal2 := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[,returnVwap := log(vwap) - log(shift(vwap,1L, type = 'lag')), by = 'InstrumentID'] %>% 
        .[!is.na(returnVwap)] %>% 
        .[,.SD[.N > truncatedTD2], by = 'InstrumentID'] %>% 
        .[, Cal_Signal1(.SD), by = "InstrumentID"] %>% 
        .[!is.na(signal1)] %>%
        .[,.SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, Cal_Signal2(.SD), by = "InstrumentID"] %>% 
        .[!is.na(signal2)] %>% 
        .[, signal2 := (signal2 - mean(signal2))/sd(signal2) * sqrt(nrow(.SD)), by = "InstrumentID"] %>% 
#        .[, rank2 := rank(signal2, ties.method = 'max') %>% as.numeric(), by = "TradingDay"] %>%
#        .[, rank2 := (rank2 - mean(rank2))/sd(rank2) * sqrt(nrow(.SD)), by = "TradingDay"] %>%
        .[, mySignal := signal1 * signal2]
# ===== dtX ========================================================================================
