# ==============================================================================
# Ref: "my Alpha 04"   

# time_min[
#             delta(volume,1),7
#         ] > 0?
# (close - open)/(high - low):
# return(vwap,1)

# 算法
# 1. 先计算 return of close
# 2. 再计算 return of vwap
# 3. m选择再过去五日成交量连续上涨
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_04'

## 需要去除的日期
truncatedTD <- 7
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
    temp <- Cal_Signal1(x)
}

Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- ifelse(temp[,min(deltaVolume) > 0]
                      ,temp[.N,returnVwap]
                      ,temp[.N,returnOHLC])
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, mySignal := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, ":="(returnVwap = log(vwap) - log(shift(vwap, 1L, type = 'lag'))
                )
          , by = 'InstrumentID'] %>% 
        .[, ":="(returnOHLC  = (close - open)/(high - low)  * returnVwap )
          , by = c('TradingDay', 'InstrumentID')] %>% 
        .[, ":="(deltaVolume = volume - shift(volume, 1L, type = 'lag'))
          , by = 'InstrumentID'] %>%
        .[!is.na(returnVwap) & !is.na(deltaVolume)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, Cal_Signal(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(mySignal)]
# ===== dtX ========================================================================================
