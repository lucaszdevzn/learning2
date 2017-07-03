################################################################################
## StatArb_Calendar_03_predictor_02.R
##
## 1. 计算各种 predictor
##
## predictor
## spdCloseRank,近月合约与远月合约的分钟收盘价格的排序
##
## Input:
## @dt_inSample
##
## Output:
## dtX
################################################################################

dtX <- dt_inSample[, .(TradingDay, Minute, NumericExchTime,
                       InstrumentID.x, close.x,
                       InstrumentID.y, close.y,
                       spdClose, spdCloseMA, spdCloseSD,
                       response = spdCloseRtn)]

myRank <- function(x){
    tempRes <- base::rank(x, ties.method = 'first')[which(x == x[estimatePeriod])][1]
    return(tempRes)
}

dtX[, spdCloseRank := rollapply(data = spdClose, width = estimatePeriod,
                               FUN = myRank, fill = NA, align = 'right')]

dtX[, ":="(spdCloseRankMA = rollapply(data = spdCloseRank, width = estimatePeriod,
                                     FUN = mean, fill = NA, align = 'right'),
          spdCloseRankSD = rollapply(data = spdCloseRank, width = estimatePeriod,
                                     FUN = sd, fill = NA, align = 'right')
)]

dtX[, predictor := (spdCloseRank - spdCloseRankMA) / spdCloseRankSD]

print(dtX[, summary(predictor)])
