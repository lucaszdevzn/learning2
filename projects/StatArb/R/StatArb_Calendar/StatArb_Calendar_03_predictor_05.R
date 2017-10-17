################################################################################
## StatArb_Calendar_03_predictor_05.R
##
## 1. 计算各种 predictor
##
## predictor:
## cor(highChgPct.x, highChgPct.y)
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
                       high.x, high.y,
                       response = spdCloseRtn)]

dtX[, ":="(
        highChgPct.x = rollapply(data = high.x, width = estimatePeriod,
                                 FUN = function(x){
                                    (x[length(x)] - x[1]) / x[length(x)]},
                                 fill = NA, align = 'right'),
        highChgPct.y = rollapply(data = high.y, width = estimatePeriod,
                                 FUN = function(x){
                                    (x[length(x)] - x[1]) / x[length(x)]},
                                 fill = NA, align = 'right')
)]

dtX[, predictor := rollApply(data = .SD[, .(highChgPct.x, highChgPct.y)], window = estimatePeriod,
                             fun = function(x){
                                # print(x)
                                cor(x$highChgPct.x, x$highChgPct.y)
                             },  align = 'right'
)]

print(dtX[, summary(predictor)])
