# ==============================================================================
# Ref: "my Alpha 10"   

# rank[
#         stddev((close - open) / (high - low), 10)
#     ] *
# cor[open, volume, 10]

# 算法
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_10'

## 需要去除的日期
truncatedTD <- 10
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
    temp <- Cal_Signal(x)
}

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, signal1 = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,sd(returnOHLC)]
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
        temp <- temp[,cor(open, volume)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, signal2 := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, returnOHLC := (close - open)/(high - low)] %>% 
        .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(signal1)] %>%
        .[, rank1 := rank(signal1, ties.method = 'max') %>% as.numeric(), by = 'TradingDay'] %>% 
#        .[, rank1 := (rank1 - mean(rank1)) / sd(rank1) * sqrt(.N), by = 'TradingDay'] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, Cal_Signal2(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(signal2)] %>%
        .[, mySignal := rank1 * signal2] %>% 
        .[!is.na(mySignal)]
# ===== dtX ========================================================================================
