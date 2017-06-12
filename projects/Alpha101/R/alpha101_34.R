# ==============================================================================
# Ref: "Alpha 101"
# # Alpha#34:    
# rank{
#     #----
#     {
#         1 - 
#             rank[
#                 stddev(returns, 2)/
#                 stddev(returns, 5)
#             ]
#     } 
#     +
#     {
#         1 - 
#             rank[
#                 delta(close, 1)
#             ]
#     }
#     #---
# }       
# 
# 算法
# 1. 先计算两个 rank
# 2. 再进行排序
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_34'

## 需要去除的日期
truncatedTD <- 5
# ------------------------------------------------------------------------------

# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, mySignal1 = NA)

    tempInfo2 <- sapply((2 + 1):nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N-1) : (.N)]
        temp <- temp[,sd(signal1)]
    }) %>% c(rep(NA,2), .)

    tempInfo5 <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N-truncatedTD + 1) : (.N)]
        temp <- temp[,sd(signal1)]
    }) %>% c(rep(NA,truncatedTD), .)

    tempRes$mySignal1 <- tempInfo2 / tempInfo5

    return(tempRes)
}

Cal_Signal2 <- function(x){
    tempRes <- cbind(x, mySignal2 = NA)
    tempRes[, mySignal2 := close - shift(close,1L, type = 'lag')] 
    return(tempRes)   
}

# ------------------------------------------------------------------------------
# 计算 signal
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
}
# ------------------------------------------------------------------------------
dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>% 
                .[, ":="(signal1 = log(close) - log(shift(close, 1L, type = 'lag'))
                        )
                  , by = 'InstrumentID'] %>% 
                .[!is.na(signal1)] %>% 
                .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
                .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
                .[!is.na(mySignal1)] %>%
                .[, Cal_Signal2(.SD), by = 'InstrumentID'] %>% 
                .[!is.na(mySignal2)] %>% 
                .[, ":="(mySignal1 = 1 - rank(mySignal1, ties.method = 'max')
                        ,mySignal2 = 1 - rank(mySignal2, ties.method = 'max')),
                  , by = 'TradingDay'] %>% 
                .[, mySignal := rank(mySignal1+mySignal2, ties.method = 'max') %>% as.numeric()
                  , by = 'TradingDay'] %>% 
                .[, mySignal := (mySignal - mean(mySignal)) / sd(mySignal) * sqrt(.N)
                  , by = 'TradingDay'] %>% 
                .[!is.na(mySignal)]
# ==================================================================================================
