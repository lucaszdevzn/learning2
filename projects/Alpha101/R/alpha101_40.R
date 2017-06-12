# ==============================================================================
# Ref: "Alpha 101"
# # Alpha#40:    

# -1 * rank[
#             stddev(high, 10)
#         ]
#     *
#     cor(high, volume, 10)

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
alphaName <- 'alpha101_40'

## 需要去除的日期
truncatedTD <- 10
# ------------------------------------------------------------------------------

# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, mySignal1 = NA)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N-truncatedTD + 1) : (.N)]
        temp <- temp[,sd(high)]
    }) %>% c(rep(NA,truncatedTD), .)

    tempRes$mySignal1 <- tempDT
    return(tempRes)
}

Cal_Signal2 <- function(x){
    tempRes <- cbind(x, mySignal2 = NA)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N-truncatedTD + 1) : (.N)]
        temp <- temp[, cor(high, volume)]
    }) %>% c(rep(NA,truncatedTD), .)

    tempRes$mySignal2 <- tempDT
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
                .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>% 
                .[!is.na(mySignal1)] %>% 
                .[, mySignal1 := as.numeric(mySignal1)]%>% 
                .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
                .[, mySignal1 := rank(-mySignal1, ties.method = 'min') %>% as.numeric()
                  , by = 'TradingDay']  %>% 
                .[, mySignal1 := (mySignal1 - mean(mySignal1)) / sd(mySignal1) * sqrt(nrow(.SD))
                  , by = 'TradingDay'] %>% 
                .[, Cal_Signal2(.SD), by = 'InstrumentID'] %>% 
                .[!is.na(mySignal2)] %>% 
                .[, mySignal := mySignal1 * mySignal2]
# ==================================================================================================
