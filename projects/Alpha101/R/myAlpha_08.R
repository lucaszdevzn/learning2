# ==============================================================================
# Ref: "my Alpha 07"   

# -rank[
#         ts_max(return(volume,1),5)
#     ] * 
# rank[
#         ts_min(return(close,1),5)
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
alphaName <- 'myAlpha_08'

## 需要去除的日期
truncatedTD <- 5
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
    temp <- Cal_Signal1(x)
}

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, signal1 = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,min(returnClose)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, signal1 := tempDT]
    return(tempRes)
}

Cal_Signal2 <- function(x){
    tempRes <- cbind(x, signal2 = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,max(returnVolume)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, signal2 := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, ":="(returnClose  = log(close) - log(shift(close, 1L, type = 'lag'))
                ,returnVolume = log(volume) - log(shift(volume, 1L, type = 'lag')))
          , by = "InstrumentID"] %>% 
        .[!is.na(returnClose) & !is.na(returnVolume)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(signal1)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, Cal_Signal2(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(signal2)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, ":="(rank1 = rank(signal1, ties.method = 'max') %>% as.numeric()
                ,rank2 = rank(-signal2, ties.method = 'min') %>% as.numeric())
          , by = 'TradingDay'] %>% 
        .[, mySignal := rank1 * rank2] %>% 
        .[, mySignal := (mySignal - mean(mySignal)) / sd(mySignal) * sqrt(nrow(.SD))
          , by = 'TradingDay']
# ===== dtX ========================================================================================
