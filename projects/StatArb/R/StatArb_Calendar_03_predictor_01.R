################################################################################
## StatArb_Calendar_03_predictor_01.R
##
## 1. 计算各种 predictor
##
## predictor:
## spdClose，近月合约与远月合约的分钟数收盘价差
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
dtX[, ":="(predictor = (spdClose - spdCloseMA) / spdCloseSD)]

print(dtX[, summary(predictor)])
