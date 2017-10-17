################################################################################
## StatArb_Calendar_02_response.R
##
## 生成需要预测的反应因子，
## 即未来的收益率预测
##
## Input:
## @dt: 从数据库提取的数据
##
## Output:
## @dt_inSample
## @response
################################################################################

## =============================================================================
## 1. 样本内数据
##    样本外数据
## @dt_inSample
## @dt_outSample
## =============================================================================

dt_inSample <- dt[TradingDay < '2017-01-01']
dt_outSample <- dt[TradingDay >= '2017-01-01']

## =============================================================================
## 2. 计算预测收益率
## @dt_inSample$spdCloseRtn
## =============================================================================

dt_inSample[, ":="(spdClose = close.y - close.x)]

## 因为在合约交割的时候，会出现合约改变
## 所以需要判断是不是同一个合约
dt_inSample[, ":="(InstrumentXLag = shift(InstrumentID.x, forecastPeriod, type = 'lag'),
                   InstrumentYLag = shift(InstrumentID.y, forecastPeriod, type = 'lag'))]
dt_inSample <- dt_inSample[InstrumentID.x == InstrumentXLag & InstrumentID.y == InstrumentYLag]

## 计算 lead 的价值
dt_inSample[, ":="(spdCloseLead = shift(spdClose, forecastPeriod, type = 'lead'))]

dt_inSample <- dt_inSample[!is.na(spdCloseLead)]

## -----------------------------------------------------------------------------
## 得到 spdCloseRtn
## 作为预测 response
dt_inSample[, ":="(spdCloseRtn = (spdCloseLead - spdClose) / (close.x + close.y))]

dt_inSample[, summary(spdCloseRtn)]
print(t.test(dt_inSample$spdCloseRtn))
## -----------------------------------------------------------------------------
## 画图

p <- ggplot(dt_inSample, aes(x = 1:nrow(dt_inSample), y = spdClose)) +
        geom_line(color = 'steelblue', alpha = 0.5) +
        geom_hline(yintercept = dt_inSample[, mean(spdClose)], color = 'hotpink', linetype = 'dashed', size = 1.5) +
        labs(title = paste(product, '==> 期限价差'),
             x = 'minuteIndex', y = 'spdClose',
             caption = '@williamfang')
print(p)

dt_inSample[, minuteIndex := 1:.N]

p <- ggplot() +
        geom_line(data = dt_inSample, aes(x = minuteIndex, y = spdCloseRtn), color = 'steelblue', alpha = 0.5) +
        geom_hline(yintercept = dt_inSample[, mean(spdCloseRtn)], color = 'hotpink', linetype = 'dashed', size = 1.5) +
        geom_point(data = dt_inSample[abs(spdCloseRtn) >= 0.005], aes(x = minuteIndex, y = spdCloseRtn), color = 'orange', size = 1.5, alpha = 0.5) +
        labs(title = paste(product, '==> 预测的收益率： spdCloseRtn'),
             x = 'minuteIndex', y = 'spdClose',
             caption = '@williamfang')
print(p)
dt_inSample <- dt_inSample[! (abs(spdCloseRtn) >= 0.005)][! TradingDay %in% c('2016-11-11','2016-11-14','2016-11-25')]
## =============================================================================
dt_inSample[, ":="(
    spdCloseMA = rollapply(spdClose, estimatePeriod, mean,
                           fill = NA, align = 'right'),
    spdCloseSD = rollapply(spdClose, estimatePeriod, sd,
                           fill = NA, align = 'right')
)]

dt_inSample[, ':='(spdCloseUpper = spdCloseMA + 5 * spdCloseSD,
                   spdCloseLower = spdCloseMA - 5 * spdCloseSD)]

temp <- dt_inSample[, .(minuteIndex = 1:.N, spdCloseMA, spdCloseUpper, spdCloseLower)]
tempG <- gather(temp, id, spread, -minuteIndex)
p <- ggplot(tempG, aes(x = minuteIndex, y = spread, color = id)) +
    geom_line() +
    geom_hline(yintercept = dt_inSample[, mean(spdClose)], color = 'hotpink', linetype = 'dashed') +
    labs(title = paste(product, '==> 期限价差'),
         x = 'minuteIndex', y = 'spdClose',
         caption = '@williamfang')
print(p)
