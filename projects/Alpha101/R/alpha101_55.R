# ==============================================================================
# Ref: "Alpha 101"
# Alpha#55:
# -1 * correlation{                               |----------> 4
#    rank[                                        |
#        (close - ts_min(low,12)) /               |---> 1
#        (ts_max(high,12) - ts_min(low,12))       |---> 2
#        ],                                       | 
#    rank[                                        |
#         volume                                  |---> 3
#        ],                                       |
#    6                                            |
# }

# 算法
# 1.分别计算价差的变化情况
# 2.按照价差变化的排名，与 volume 的排名
# 3.统计其相关系数
# 含义
# 本质上说，是对 价差 与 volume 在形态出现背离的时候，进行操作
# 
# 关联：
# Alpha101 #3
# Alpha101 #6
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_55'

## 需要去除的日期
truncatedTD <- 12
truncatedTD2 <- 6
# ------------------------------------------------------------------------------


# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 signal
Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NaN)

    tempDT <- sapply((truncatedTD2 + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD2) : .N]
        temp <- temp[, cor(rank1,rank2)]
        return(temp)
    })

    tempRes[(truncatedTD2 + 1):nrow(tempRes), mySignal := tempDT]
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
        .[,":="(
            rank1 = c(rep(NA,truncatedTD),sapply(1:nrow(.SD[.N > truncatedTD]), function(i){
                tempRes <- (.SD[truncatedTD+i,close] - .SD[(i):(truncatedTD+i-1), min(low)])/
                           (.SD[(i):(truncatedTD+i-1), max(high)] - .SD[(i):(truncatedTD+i-1), min(low)])
                return(tempRes)
                        })
                    )
            )
          ,by = c('InstrumentID')] %>%
        .[!is.na(rank1)] %>%
        .[, rank2 := base::rank(.SD[,volume],ties.method = 'max')
          ,by = c('TradingDay')] %>%
        .[!is.na(rank2)] %>%
        .[,.SD[.N > truncatedTD2], by = 'InstrumentID']

dtX <- dtX %>%
        .[,Cal_Signal(.SD)
        ,by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
# ==================================================================================================
