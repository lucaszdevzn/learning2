# ==============================================================================
# Ref: "Alpha 101"
# Alpha#4:
# -1 * [ ts_Rank ( rank(low), 9 ) ]
#                 |--- 1 --|
#        |-------- 2 ---------|
# |--------------- 3 --------------| 

# 算法
# 1.先计算在同一天的，最低价排名          :==> by = 'TradingDay'
# 2.再对同一个合约，计算过去 9 天的排名    :==> by = 'InstrumentID'
# 3.取过去排名最低的合约做信号
#
# 含义
# 本质上说，是利用了过去 N 天的最低价，在未来有可能出现价格的反转，
#         即我们常说的 mean-reverting
# 或者，可以直观的理解为，买入低价股
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_04'

## 需要去除的日期
truncatedTD <- 9
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
        tempCurrent <- tempRes[TradingDay < tempTradingDay ][.N]

        temp1 <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp2 <- temp1[,rank(rankLow, ties.method = 'max')]

        temp3  <- temp2[which(temp1[,rankLow] == tempCurrent[,rankLow])][1] %>% as.numeric()

        temp <- (temp3 - mean(temp2)) / sd(temp2) * length(temp2)
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
        .[,rankLow := rank(low, ties.method = 'max')
                  ,by = 'TradingDay'] %>%
        .[, Cal_Signal(.SD) ,by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
# ==================================================================================================
