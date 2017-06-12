# ==============================================================================
# Ref: "my Alpha 02"   

# cor{
#     rank[
#             return(vwap,5)
#         ],
#     rank[
#             return(volume,5)
#         ],
#     5
# }


# 算法
# 1. 先计算价格的动能
# 2. 计算成交量的动能
# 3. 计算二者的相关系数
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ===== Setting ====================================================================================
## Alpha Name
alphaName <- 'myAlpha_02'

## 需要去除的日期
truncatedTD <- 5
# ===== Setting ====================================================================================



# ===== Cal_Signal =================================================================================
if(FALSE){
    x <- dtX[InstrumentID == 'CF109']
    head(x, 20)
    temp <- Cal_Signal1(x)
}

Cal_Signal <- function(x){
    tempRes <- cbind(x, mySignal = NA)

    tempDT <- sapply((1+truncatedTD) : nrow(tempRes), function(i){
        tempTradingDay <- tempRes[i, TradingDay]
        temp <- tempRes[TradingDay < tempTradingDay][(.N - truncatedTD + 1) : .N]
        temp <- temp[,cor(rank1, rank2)]
        return(temp)
    }) %>% c(rep(NA,truncatedTD),.)

    tempRes[, mySignal := tempDT]
    return(tempRes)
}
# ===== Cal_Signal =================================================================================



# ===== dtX ========================================================================================
dtX <- inSample[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>% 
        .[, ":="(signal1 = log(vwap) - log(shift(vwap, 5L, type = 'lag'))
                ,signal2 = log(volume) - log(shift(volume, 5L, type = 'lag')))
          , by = 'InstrumentID'] %>% 
        .[!is.na(signal1) & !is.na(signal2)] %>% 
        .[, .SD[.N > truncatedTD], by = 'InstrumentID'] %>%
        .[, ":="(rank1 = rank(signal1, ties.method = 'max')
                ,rank2 = rank(signal2, ties.method = 'max'))
          , by = 'TradingDay'] %>% 
        .[, Cal_Signal(.SD), by = 'InstrumentID'] %>% 
        .[!is.na(mySignal)]
# ===== dtX ========================================================================================
