# ==============================================================================
# Ref: "Alpha 101"
# Alpha#15:
# -1 * sum{                                      |-----------------------> 4
#                rank[                           |---------------> 3
#                        cor(                    |--------> 2
#                                rank(high),     |---> 1
#                                rank(volume),   |---> 1
#                                3               |---> 1
#                            )
#                    ],
#                3                               |------------------------> 4
#            }                                         
#
# 算法 
# 1.
#
# 含义
# 本质上说，是对 close 与 volume 在形态出现背离的时候，进行操作
# 
# 关联：
# 
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_15'

## 需要去除的日期
truncatedTD <- 3
# ------------------------------------------------------------------------------


# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 signal

Cal_Signal1 <- function(x){
    tempRes <- cbind(x, signal1 = NaN)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp <- temp[, cor(rankHigh,rankVolume)]
        return(temp)
    })

    tempRes[(truncatedTD + 1):nrow(tempRes), signal1 := tempDT]
    return(tempRes)
}


Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NaN)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp <- temp[, sum(signal2)] %>% as.numeric()
        return(temp)
    })

    tempRes[(truncatedTD + 1):nrow(tempRes), mySignal := tempDT]
    return(tempRes)
}


if(FALSE){
    x <- dtX[InstrumentID == 'zn1111']
#    x[,tsRankRow := rank(rankLow, ties.method = 'max')]
    temp <- Cal_Signal(x)
}
# ------------------------------------------------------------------------------
dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>%
        .[, ":="(rankHigh   = base::rank(high, ties.method = 'max'),
                 rankVolume = base::rank(volume, ties.method = 'max'))
          , by = 'TradingDay'] %>%
        .[,.SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[,Cal_Signal1(.SD), by = 'InstrumentID'] %>%
        .[!is.na(signal1)] %>%
        .[,signal2 := base::rank(signal1, ties.method = 'max')
          , by = 'TradingDay'] %>%
        .[,.SD[.N > truncatedTD]
          , by = 'InstrumentID'] %>%
        .[, Cal_Signal(.SD), by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
# ==================================================================================================
