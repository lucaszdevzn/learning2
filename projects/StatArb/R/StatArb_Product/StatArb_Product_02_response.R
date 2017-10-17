################################################################################
## StatArb_Product_02_response.R
################################################################################

## =============================================================================
dtMinute <- merge(dtMinuteA, dtMinuteB, 
                 by = c('TradingDay','minute', 'NumericExchTime')) %>% 
            .[order(TradingDay, NumericExchTime)]

dtMinute
## =============================================================================

## =============================================================================
dtMinute[, ':='(
        spdClose = close.x - close.y,
        vwapA    = turnover.x / volume.x,
        vwapB    = turnover.y / volume.y
    )]

dtMinute[, ':='(
        spdCloseRtn = (shift(spdClose, 45L, type = 'lead') - spdClose) / spdClose,
        vwapARtn    = (vwapA - shift(vwapA, 30L, type = 'lag')) / shift(vwapA, 30L, type = 'lag'),
        vwapBRtn    = (vwapB - shift(vwapB, 30L, type = 'lag')) / shift(vwapB, 30L, type = 'lag')
    )]
dtMinute <- dtMinute[!is.na(spdCloseRtn)][!is.na(vwapARtn) & !is.na(vwapBRtn)]
## =============================================================================

## =============================================================================
dtMinute[abs(spdCloseRtn) > 0.05]

dtMinute <- dtMinute[abs(spdCloseRtn) <= 0.05]
## =============================================================================
temp <- dtMinute[15000:25000]
p <- ggplot(temp, aes(x = 1:nrow(temp), y = spdCloseRtn)) +
        geom_point(color = 'steelblue', alpha = 0.5) +
        geom_hline(yintercept = 0, color = 'hotpink', size = 2)
print(p)
ggplotly(p)

fit <- lm(spdCloseRtn ~ vwapARtn + 0, data = dtMinute)
summary(fit)


fit <- lm(spdCloseRtn ~ vwapBRtn + 0, data = dtMinute)
summary(fit)
print(as.vector(fit$coefficients))


dtMinute

dtMinute[, ":="(
        rtn = (
                (shift(close.x, 45L, type = 'lead') - close.x) - 
                (shift(close.y, 45L, type = 'lead') - close.y)
              ) / (close.x + close.y) *
              sign(as.vector(fit$coefficients) * vwapBRtn)
    )]

res <- dtMinute[!is.na(rtn)][abs(rtn) <= 0.03]
res[, cumRtn := cumsum(rtn)]
print(res)
p <- ggplot(res, aes(x = 1:nrow(res), y = cumRtn)) +
        geom_line(color = 'steelblue')
print(p)
ggplotly(p)
