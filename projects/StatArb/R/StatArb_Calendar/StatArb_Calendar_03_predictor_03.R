################################################################################
## StatArb_Calendar_03_predictor_03.R
##
## 1. 计算各种 predictor
##
## predictor:
## spdTurnoverChgPct，分钟收盘交易额的增速差
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
                       response = spdCloseRtn)]

dtX[, ":="(turnoverChgPct.x = rollapply(turnover.x, estimatePeriod,
                                        FUN = function(x){(x[length(x)] - x[1]) / x[1]},
                                        fill = NA, align = 'right'),
           turnoverChgPct.y = rollapply(turnover.y, estimatePeriod,
                                        FUN = function(x){(x[length(x)] - x[1]) / x[1]},
                                        fill = NA, align = 'right')
)]

dtX[, spdTurnoverChgPct := turnoverChgPct.y - turnoverChgPct.x]
dtX[, predictor := rollapply(data = spdTurnoverChgPct, width = estimatePeriod,
                             FUN = function(x){return((x[length(x)] - mean(x)) / sd(x))},
                             fill = NA, align = 'right')]
print(dtX[, summary(predictor)])
