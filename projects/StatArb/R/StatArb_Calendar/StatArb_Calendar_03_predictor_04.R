################################################################################
## StatArb_Calendar_03_predictor_04.R
##
## 1. 计算各种 predictor
##
## predictor:
## cor(spdCloseMA, spdTurnoverChgPct)
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
                       turnover.x, turnover.y,
                       spdCloseZ = (spdClose - spdCloseMA)/spdCloseSD,
                       response = spdCloseRtn)]

dtX[, ":="(turnoverChgPct.x = rollapply(turnover.x, estimatePeriod,
                                        FUN = function(x){(x[length(x)] - x[1]) / x[1]},
                                        fill = NA, align = 'right'),
           turnoverChgPct.y = rollapply(turnover.y, estimatePeriod,
                                        FUN = function(x){(x[length(x)] - x[1]) / x[1]},
                                        fill = NA, align = 'right')
)]

dtX[, spdTurnoverChgPct := turnoverChgPct.y - turnoverChgPct.x]
dtX[, spdTurnoverChgPctZ := rollapply(data = spdTurnoverChgPct, width = estimatePeriod,
                             FUN = function(x){return((x[length(x)] - mean(x)) / sd(x))},
                             fill = NA, align = 'right')]
# dtX <- dtX[!is.na(spdCloseZ)][!is.na(spdTurnoverChgPctZ)]
dtX[, predictor := rollapply(data = 1:.N, width = estimatePeriod,
                             FUN = function(i){
                                cor(dtX[i]$spdCloseZ,dtX[i]$spdTurnoverChgPctZ)
                             }, fill = NA, align = 'right')]

print(dtX[, summary(predictor)])

