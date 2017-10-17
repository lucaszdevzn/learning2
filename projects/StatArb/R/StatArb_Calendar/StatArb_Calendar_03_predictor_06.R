################################################################################
## StatArb_Calendar_03_predictor_06.R
##
## 1. 计算各种 predictor
##
## predictor:
##
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
                       high.x, low.x, high.y, low.y,
                       response = spdCloseRtn)]

dtX[, HLRatio := (high.x - low.x) / (high.y - low.y)]

dtX[, predictor := rollapply(data = HLRatio, width = estimatePeriod,
                            FUN = function(x){
                                (x[length(x)] - mean(x)) / sd(x)
                            }, fill = NA, align = 'right'
)]

print(dtX[, summary(predictor)])


