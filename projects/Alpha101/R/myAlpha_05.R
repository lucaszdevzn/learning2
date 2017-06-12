# ==============================================================================
# Ref: "my Alpha 05"   

# cor{
#     rank[
#             ts_min(return(vwap,1),5)
#         ],
#     rank[
#             stddev(volume,5)
#         ]
# }

# 算法
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_05'

## 需要去除的日期
truncatedTD <- 2
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
    temp <- Cal_Signal1(x)
}

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, rank1 = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,min(returnVwap)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, rank1:= tempDT]
    return(tempRes)
}

Cal_Signal2 <- function(x){
    tempRes <- cbind(x, rank2 = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,sd(volume)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, rank2 := tempDT]
    return(tempRes)
}

Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,cor(signal1, signal2)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, mySignal := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        # .[, volume := (volume - mean(volume))/sd(volume) * sqrt(nrow(.SD)) %>% as.numeric()
        #   , by = 'InstrumentID'] %>%
        .[, ":="(returnVwap = log(vwap) - log(shift(vwap, 1L, type = 'lag'))
                )
          , by = 'InstrumentID'] %>% 
        .[!is.na(returnVwap)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(rank1)] %>% 
        .[, signal1 := rank(rank1, ties.method = 'max') %>% as.numeric(), by = 'TradingDay'] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, Cal_Signal2(.SD), by = 'InstrumentID'] %>% 
        .[, signal2 := rank(rank2, ties.method = 'max') %>% as.numeric(), by = 'TradingDay'] %>%
        .[!is.na(signal2)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, Cal_Signal(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(mySignal)]
# ===== dtX ========================================================================================
