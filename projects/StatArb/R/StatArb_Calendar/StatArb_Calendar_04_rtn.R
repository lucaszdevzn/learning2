################################################################################
## StatArb_Calendar_04_rtn.R
##
## 1. 计算净值曲线： rtn
##
## Input:
## @dtX
##
## Output:
## rtn
################################################################################

## =============================================================================
##回归分析
## =============================================================================
dtX <- dtX[!is.na(predictor)]

fit <- lm(data = dtX, response ~ predictor + 0)
print(summary(fit))

dtX[, signal := fit$coefficients * predictor]
dtX[, rtn := sign(signal) * response]

dtX <- dtX[! rtn %between% c(-0.0012, 0.0012)]

dtX[, cumRtn := cumsum(rtn)]
dtX

## =============================================================================
## 净值曲线
## =============================================================================
p <- ggplot(dtX, aes(x = 1:nrow(dtX), y = cumRtn)) +
    geom_line(color = 'steelblue', alpha = 0.6, size = 1.1) +
    labs(title = paste(product, '==> 净值曲线'),
             x = 'minuteIndex', y = 'cumRtn',
             caption = '@williamfang')
print(p)

## =============================================================================
## sharpRatio
## =============================================================================
tempDailyRtn <- dtX[, .(dailyRtn = sum(rtn)), by = 'TradingDay']
sharpRatio <- tempDailyRtn[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252)]
print(sharpRatio)
#dev.new()
tempDailyRtn$TradingDay <- ymd(tempDailyRtn$TradingDay)
p <- ggplot() +
        geom_point(data = tempDailyRtn[dailyRtn >= 0],
                 aes(x = TradingDay, y = dailyRtn), color = 'red', size = 2, alpha = 0.5) +
        geom_point(data = tempDailyRtn[dailyRtn < 0],
                 aes(x = TradingDay, y = dailyRtn), color = 'green', size = 2, alpha = 0.5) +
        geom_line(data = tempDailyRtn,
                 aes(x = TradingDay, y = dailyRtn), color = 'gray', size = 0.5) +
        geom_hline(yintercept = 0, color = 'orange', size = 0.8, linetype = 'dashed')
print(p)

