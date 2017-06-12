# ==============================================================================
# Ref: "my Alpha 03"   

# rank[return(close,1)] *
# rank[
#         ts_max(
#                 return(vwap,1), 5
#               )
#     ]

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
alphaName <- 'myAlpha_03'

## 需要去除的日期
truncatedTD <- 5
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
        temp <- temp[,cor(rank1, rank2)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, mySignal := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, ":="(returnClose = log(close) - log(shift(close, 1L, type = 'lag'))
                )
          , by = 'InstrumentID'] %>% 
        .[, ":="(returnvwap1 = log(vwap) - log(shift(vwap, 1L, type = 'lag'))
                ,returnvwap2 = log(shift(vwap, 1L, type = 'lag')) - log(shift(vwap, 2L, type = 'lag'))
                ,returnvwap3 = log(shift(vwap, 2L, type = 'lag')) - log(shift(vwap, 3L, type = 'lag'))
                ,returnvwap4 = log(shift(vwap, 3L, type = 'lag')) - log(shift(vwap, 4L, type = 'lag'))
                ,returnvwap5 = log(shift(vwap, 4L, type = 'lag')) - log(shift(vwap, 5L, type = 'lag'))
                ), by = 'InstrumentID'] %>%
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, ":="(returnvwap = .SD[,min(returnvwap1,returnvwap2,returnvwap3
                                            ,returnvwap4, returnvwap5)])
            , by = c('TradingDay','InstrumentID')] %>%
        .[, ":="(rank1 = rank(returnClose, ties.method = 'max')
                ,rank2 = rank(returnvwap, ties.method = 'max'))
          , by = 'TradingDay'] %>% 
        .[, mySignal := rank1 * rank2 %>% as.numeric()] %>% 
        .[, mySignal := (mySignal - mean(mySignal)) / sd(mySignal) * sqrt(nrow(.SD))
          , by = 'TradingDay']
# ===== dtX ========================================================================================
