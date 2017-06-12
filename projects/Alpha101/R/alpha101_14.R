# ==============================================================================
# Ref: "Alpha 101"
# Alpha#14:
# -1 * rank[ delta(return,3) ]    |---->
#    * cor[ open, volume, 10]     |---->                                |

# 算法
# 1. 先计算 open 与 volume 在过去 10 天的相关系数，看看二者是不是同方向的变动
# 2. 对过去三天的收益率进行排名
# 3. 选取在过去三天，open 与 volume 同方向变动，且收益为正的票
#
# 含义
# 本质上说， 追随过去比较强势的票
# 
# 关联：
# 
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_14'

## 需要去除的日期
truncatedTD <- 10
# ------------------------------------------------------------------------------


# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 signal

Cal_Signal2 <- function(x){
    tempRes <- cbind(x, signal2 = NaN)

    tempDT <- sapply((truncatedTD + 1):nrow(tempRes), function(i){
        # print(i)
        tempTradingDay <- tempRes[i,TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD) : .N]
        temp <- temp[, cor(open,volume)]
        return(temp)
    })

    tempRes[(truncatedTD + 1):nrow(tempRes), signal2 := tempDT]
    return(tempRes)
}

if(FALSE){
    x <- dtX[InstrumentID == 'zn1111']
#    x[,tsRankRow := rank(rankLow, ties.method = 'max')]
    temp <- Cal_Signal2(x)
}
# ------------------------------------------------------------------------------
dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>%
        .[, ":="(deltaRtn3 = (close - shift(close,3L,type='lag')) / 
                                      shift(close,3L,type='lag') 
                )
          ,by = 'InstrumentID'] %>%
        .[!is.na(deltaRtn3)] %>%
        .[,":="(signal1 = base::rank(deltaRtn3, ties.method = 'max') %>% as.numeric())
          ,by = 'TradingDay'] %>%
        .[, signal1 := (signal1 - mean(signal1)) / sd(signal1) * nrow(.SD), by = 'TradingDay'] %>%
        .[,.SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[,Cal_Signal2(.SD), by = 'InstrumentID'] %>%
        .[,mySignal := signal1 * signal2, by = 'InstrumentID'] %>%
        .[!is.na(mySignal)]
# ==================================================================================================
