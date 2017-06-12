# ==============================================================================
# Ref: "my Alpha 09"   

# cor{
#     sqrt(close*open) - sqrt(high * low) / volume,
#     (close - open)/open, 5
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
alphaName <- 'myAlpha_09'

## 需要去除的日期
truncatedTD <- 20
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================

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
        .[,":="(signal1 = (sqrt(close * open) - sqrt(high * low)) / volume
               ,signal2 = (close - open) / open)] %>% 
        .[, Cal_Signal(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(mySignal)]
# ===== dtX ========================================================================================
