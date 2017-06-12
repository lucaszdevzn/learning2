# ==============================================================================
# Ref: "Alpha 101"
# Alpha#2:
# -1 * [ correlation ( rank( (delta(log(volume),2) ), rank( (close-open)/open ) , 6 ) ]
#                             |-------- 1 -------|           |------ 1 ------|
#                      |------------ 2 ------------|  |---------- 2 ----------|
#      |--------------------------------- 3 ----------------------------------------| 

# 算法
# 1.分别计算 volume 在过去两天变化率，价格在日内的波动情况(close-open)/open
# 2.分别对 volume 和 price 的变化率做排名
# 3.对二者在过去 6 天的排名进行相关性分析
#
# 含义
# 本质上说，是对 open 与 volume 在变化率出现背离的时候，进行操作
# 
# 关联：
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_02'

## 需要去除的日期
truncatedTD <- 6
# ------------------------------------------------------------------------------



# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 correlation
Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NaN)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp <- temp[, cor(rankVolumeChgPct,rankPriceChgPct)]
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
        .[,":="(volumeChgPct = log(volume) - log(shift(volume,2L,type = 'lag')),
                priceChgPct  = (close - open)/open
                )
          ,by = 'InstrumentID'] %>%
        .[! is.na(volumeChgPct) & !is.na(priceChgPct)] %>%
        .[,":="(rankVolumeChgPct = base::rank(volumeChgPct, ties.method = 'max'),
                rankPriceChgPct  = base::rank(priceChgPct, ties.method = 'max'))
          ,by = 'TradingDay'] %>%
        .[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>%
        .[,Cal_Signal(.SD)
          ,by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
# ==================================================================================================
