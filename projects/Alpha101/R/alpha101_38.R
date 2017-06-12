# ==============================================================================
# Ref: "Alpha 101"
# Alpha#38:



# 算法
# 1.分别计算 Volume 和 close 的价差
# 2.
#
# 含义
# 如果出现成交量上升，但是收盘价却下跌，则发出买入信号
# 
# 关联：
# 
# ==============================================================================


# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_38'

## 需要去除的日期
truncatedTD <- 10

# ------------------------------------------------------------------------------

# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 计算 signal
Cal_Signal1 <- function(x){
    tempRes <- cbind(x, signal1 = NaN)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        tempCurrent <- tempRes[TradingDay < tempTradingDay][.N]

        temp1 <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp2 <- rank(temp1[,close], ties.method = 'max')

        temp <- temp2[which(temp1[,close] == tempCurrent[,close])][1] %>% as.numeric()
        return(temp)
    })

    tempRes[(truncatedTD + 1):nrow(tempRes), signal1 := tempDT]
    return(tempRes)
}

if(FALSE){
    x <- dtX[InstrumentID == 'zn1111']
#    x[,tsRankRow := rank(rankLow, ties.method = 'max')]
    temp <- Cal_Signal1(x)
}



# ------------------------------------------------------------------------------
dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>%
        .[, Cal_Signal1(.SD), by = 'InstrumentID'] %>%
        .[!is.na(signal1)] %>%
        .[,":="(rank1 = base::rank(signal1, ties.method = 'max'),
                rank2 = base::rank(close / open, ties.method = 'max')
                )
          , by = 'TradingDay'] %>%
        .[, mySignal := rank1 * rank2 %>% as.numeric()] %>%
        .[!is.na(mySignal)] %>% 
        .[, mySignal := (mySignal - mean(mySignal)) / sd(mySignal) * sqrt(nrow(.SD)),
          , by = 'TradingDay']
# ==================================================================================================
