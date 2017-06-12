# ==============================================================================
# Ref: "my Alpha 01"   

# cor{
#     (close - open) /
#     [
#         ts_rank(high,5) - 
#         ts_rank(low, 5)
#     ],
#     change(volume,1),
#     5
# }

# 算法
# 1. 先计算价格的动能
# 2. 计算成交量的动能
# 3. 计算二者的相关系数
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_01'

## 需要去除的日期
truncatedTD <- 5
truncatedTDs <- c(3,5,7,10,15)
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
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,(temp[.N,close] - temp[.N,open]) / (max(high) - min(low))]
    }) %>% c(rep(NA,truncatedTD),.)
    
    tempRes[, signal1 := tempDT]
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
        .[, signal2 := log(volume) - log(shift(volume, 1L, type = 'lag')), by = 'InstrumentID'] %>% 
#        .[!is.na(signal2)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
#        .[!is.na(signal1)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, Cal_Signal(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(mySignal)]
# ===== dtX ========================================================================================

