# Cal_dtXY.R
# ==========: 计算 dtXY =============================================================================
# ------------------------------------------------------------------------------
# dtXY  
# 1.predictor  : 模型的解释变量
# 2.response   : 模型的被解释变量
# 3.predictRtn : 未来的预测
# ------------------------------------------------------------------------------ 
dtX <- dtX[!is.na(mySignal),.(InstrumentID, TradingDay, mySignal)]

dtXY <-  merge(dtX, dtY, by = c("TradingDay","InstrumentID")) %>%
            .[,.(TradingDay,InstrumentID
                ,predictor = mySignal
                ,response  = closeRtn
                ,predictRtn)
            ]

fit <- lm(response ~ predictor + 0, data = dtXY)
print(summary(fit))

summary(fit)$r.squared

# str(fit)
fit$coefficients["predictor"]
summary(fit)$coefficients
# ==================================================================================================


# ==========: 计算 portfoli0 ========================================================================

## 增加 portfolio weight
dtXY[,":="(pWeight = abs(predictor) / (sum(abs(predictor),na.rm = TRUE) + 0.000001))
     , by = c("TradingDay")]

