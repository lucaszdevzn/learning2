# ==============================================================================
# Ref: "Alpha 101"
# Alpha#3:
# -1 * [ correlation ( rank(open), rank(volume), 9 ) ]
#                      |---------- 1 ---------|
#        |------------------ 2 ------------------|
# |------------------------- 3 ----------------------| 

# 算法
# 1.分别计算 open 和 volume 在横截面的排名     :==> by = 'TradingDay'
# 2.再计算同一品种在时间序列上面的排名相关系数     :==> by = 'InstrumentID'
# 3.取过去的 open 与 volume 相关系数低的做信号
#
# 含义
# 本质上说，是对 open 与 volume 在形态出现背离的时候，进行操作
# 
# 关联：
# Alpha101 #6
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_03'

## 需要去除的日期
truncatedTD <- 9
# ------------------------------------------------------------------------------


# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 signal
Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NaN)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp <- temp[, cor(signal1,signal2)]
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
        .[,":="(rankOpen   = base::rank(open, ties.method = 'max'),
                rankVolume = base::rank(volume, ties.method = 'max')
                )
          ,by = 'TradingDay'] %>%
        .[,":="(
            signal1 = (rankOpen - mean(rankOpen)) / sd(rankOpen) * nrow(.SD),
            signal2 = (rankVolume - mean(rankVolume)) / sd(rankVolume) * nrow(.SD)
                )
          ,by = 'TradingDay'] %>%
        .[, Cal_Signal(.SD) ,by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
# ==================================================================================================
