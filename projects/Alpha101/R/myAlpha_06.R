# ==============================================================================
# Ref: "my Alpha 06"   

# rank[
#         abs(
#                 cor(vwap, volume, 5)
#             )
#     ]

# 算法
# 1. cor
# 2. 取两者同涨或同跌的相关系数
# 3. 
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_06'

## 需要去除的日期
truncatedTD <- 7
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
        temp <- temp[, abs(cor(vwap, volume))]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, signal1 := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[,Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(signal1)] %>% 
        .[, .SD[.N > truncatedTD], by = "InstrumentID"] %>% 
        .[, mySignal := rank(signal1, ties.method = 'max') %>% as.numeric(), by = 'TradingDay'] %>% 
        .[, mySignal := (mySignal - mean(mySignal)) / sd(mySignal) * sqrt(nrow(.SD))
            , by = 'TradingDay'] %>%
        .[!is.na(mySignal)]     
# ===== dtX ========================================================================================
