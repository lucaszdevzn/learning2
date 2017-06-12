# ==============================================================================
# Ref: "Alpha 101"
# # Alpha#44:

# -1 *
#     cor[
#         high, 
#         rank(volume),
#         5
#     ]

# #
# 算法
# 1.
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_44'

## 需要去除的日期
truncatedTD <- 5
# ------------------------------------------------------------------------------


# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 
Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NA)

    tempDT <- sapply((truncatedTD+1) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[, cor(high, rankVolume)]
    }) %>% c(rep(NA,truncatedTD), .)

    tempRes$mySignal <- tempDT
    return(tempRes)
}
# ------------------------------------------------------------------------------
# 计算 signal
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
}
# ------------------------------------------------------------------------------

dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>% 
                .[, rankVolume := rank(volume, ties.method = 'max')
                  , by = 'TradingDay'] %>% 
                .[, Cal_Signal(.SD), by = 'InstrumentID'] %>% 
                .[!is.na(mySignal)]

# ==================================================================================================
