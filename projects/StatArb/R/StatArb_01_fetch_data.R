## =============================================================================
## i vs rb
## =============================================================================
library("tseries")

mysql <- mysqlFetch('china_futures_bar')

dt_i <- dbGetQuery(mysql,"
                    select TradingDay, Minute, NumericExchTime, 
                           InstrumentID, ProductID, 
                           open, high, low, close, 
                           volume, turnover
                    from mainContractMinuteData
                    where ProductID = 'i'
                    and TradingDay >= 20170401
                  ") %>% as.data.table()

dt_j <- dbGetQuery(mysql,"
                    select TradingDay, Minute, NumericExchTime, 
                           InstrumentID, ProductID, 
                           open, high, low, close, 
                           volume, turnover
                    from mainContractMinuteData
                    where ProductID = 'j'
                    and TradingDay >= 20170401
                  ") %>% as.data.table()

dt_rb <- dbGetQuery(mysql,"
                    select TradingDay, Minute, NumericExchTime, 
                           InstrumentID, ProductID, 
                           open, high, low, close, 
                           volume, turnover
                    from mainContractMinuteData
                    where ProductID = 'rb'
                    and TradingDay >= 20170401
                  ") %>% as.data.table()

head(dt_i)
head(dt_rb)

dt <- merge(dt_i, dt_j, by = c('TradingDay','Minute','NumericExchTime'))
dt

## =============================================================================
## Test for cointegration
## =============================================================================
temp <- gather(dt[,.(id = 1:nrow(dt), close.x = 3.2821 *close.x, close.y)], 
    key = InstrumentID, value = close, -id)
head(temp)
ggplot(temp, aes(x = id, y = close, color = InstrumentID)) + 
    geom_line()

fit <- lm(data = dt, close.y ~ close.x + 0)
summary(fit)
print(fit$coefficients)
adf.test(fit$residuals)


p  <- ggplot(dt, aes(x = 1:nrow(dt), y = spd)) +
    geom_jitter(alpha = 0.1, color = 'steelblue', size = 0.6)
library(plotly)
ggplotly(p)

dt_inSample <- dt[TradingDay %between% c('2017-04-01', '2017-05-31')]
dt_inSample

dt_outSample <- dt[! TradingDay %between% c('2017-04-01', '2017-05-31')]
dt_outSample

##
## inSample
##
fit <- lm(data = dt_inSample, close.y ~ close.x + 0)
summary(fit)
print(fit$coefficients)
adf.test(fit$residuals)

temp <- gather(dt_inSample[,.(id = 1:nrow(dt_inSample), close.x = fit$coefficients *close.x, close.y)], 
    key = InstrumentID, value = close, -id)
head(temp)
p <- ggplot(temp, aes(x = id, y = close, color = InstrumentID)) + 
    geom_line()
print(p)
ggplotly(p)

## =============================================================================
dt_inSample <- dt[TradingDay %between% c('2017-04-01', '2017-05-31')]
dt_inSample

fit <- lm(data = dt_inSample, close.y ~ close.x + 0)
summary(fit)
print(fit$coefficients)
adf.test(fit$residuals)

dt_inSample[,spd := close.y - fit$coefficients *close.x]
dt_inSample
plot(x = 1:nrow(dt_inSample), y = dt_inSample[,spd], type = 'l')

dt_inSample[, spd30m := shift(spd, 30L, type = 'lag')]
dt_inSample[, spdChg30m := (spd - spd30m)]
dt_inSample[, spdRtn30m := (spd - spd30m) / spd30m]


dt_inSample[, ':='(turnoverXRtn1m = (turnover.x - shift(turnover.x, 1L, type = 'lag')) /
                                    shift(turnover.x, 1L, type = 'lag'),
                  turnoverYRtn1m = (turnover.y - shift(turnover.y, 1L, type = 'lag')) /
                                    shift(turnover.y, 1L, type = 'lag'))]
dt_inSample[,':='(turnoverRatio = turnoverXRtn1m / turnoverYRtn1m,
                 turnoverRtnDiff = turnoverYRtn1m - turnoverXRtn1m,
                 turnoverDiff   = turnover.y - turnover.x)]

dt_inSample <- dt_inSample[!is.na(spd30m)]
dt_inSample

fit <- lm(data = dt_inSample, spdChg30m ~ turnoverRtnDiff + 0)
summary(fit)

ggplot(dt_inSample, aes(x = turnoverDiff, y = spdRtn30m)) +
    geom_point()

ggplot() + 
    geom_line(data = dt_inSample[turnoverRtnDiff %between% c(quantile(turnoverRtnDiff, 0.25),quantile(turnoverRtnDiff, 0.75))], aes(x = 1:6927, y = spdChg30m), color = 'orange') +
    geom_line(data = dt_inSample[turnoverRtnDiff %between% c(quantile(turnoverRtnDiff, 0.25),quantile(turnoverRtnDiff, 0.75))], aes(x = 1:6927, y = 100* turnoverRtnDiff), color = 'steelblue') +
    geom_line(data = dt_inSample[turnoverRtnDiff %between% c(quantile(turnoverRtnDiff, 0.25),quantile(turnoverRtnDiff, 0.75))], aes(x = 1:6927, y = spdChg30m), color = 'orange') 

## =============================================================================
estimatedPeriod <- 100
estimatedRange  <- 3
closingRange    <- 2

dt_inSample[, hedgingRatio := .(sapply(1:nrow(.SD), function(i){
    if (i <= estimatedPeriod){
        return(NA)
    }else{
        temp <- .SD[( i- estimatedPeriod + 1) : (i)]
        fit <- lm(data = temp, close.y ~ close.x + 0)
        return(as.vector(fit$coefficients))
    }
}))]

dt_inSample[, ':='(spd = sapply(1:nrow(.SD), function(i){
    if (i <= estimatedPeriod){
        return(NA)
    }else{
        temp <- .SD[i]
        tempRes <- temp$close.y - temp$hedgingRatio * temp$close.x
    }
}), spdMean = sapply(1:nrow(.SD), function(i){
    if (i <= estimatedPeriod){
        return(NA)
    }else{
        temp <- .SD[( i- estimatedPeriod + 1) : (i)]
        tempRes <- mean(temp$close.y - temp[.N,hedgingRatio] * temp$close.x)
    }
}), spdSD = sapply(1:nrow(.SD), function(i){
    if (i <= estimatedPeriod){
        return(NA)
    }else{
        temp <- .SD[( i- estimatedPeriod + 1) : (i)]
        tempRes <- sd(temp$close.y - temp[.N,hedgingRatio] * temp$close.x)
    }
}))]

dt_inSample <- dt_inSample[! (is.na(spd) | is.na(spdMean))]

ggplot(dt_inSample, aes(x = 1:nrow(dt_inSample), y = spd)) +
    geom_line(color = 'orange', alpha = 0.8)

dt_inSample[, spdUpper := sapply(1:.N, function(i){
    return(.SD[i, (spdMean + estimatedRange  * spdSD)])
})]
dt_inSample[, spdLower := sapply(1:.N, function(i){
    return(.SD[i, (spdMean - estimatedRange  * spdSD)])
})]

tempDT <- dt_inSample[1:.N]
p <- ggplot(data = tempDT) +
    geom_line(aes(x = 1:nrow(tempDT), y = spd), alpha = 0.2, color = 'orange') +
    geom_line(aes(x = 1:nrow(tempDT), y = spdUpper), color = 'blue3', alpha = 0.5) +
    geom_line(aes(x = 1:nrow(tempDT), y = spdLower), color = 'steelblue', alpha = 0.5)
ggplotly(p)
print(p)



dt_inSample[, signal := sapply(1:.N, function(i){
    if (.SD[i, spd > (spdMean + estimatedRange  * spdSD)] ){
        tempRes <- +1
    }
    if (.SD[i, spd < (spdMean + estimatedRange  * spdSD)]) {
        tempRes <- -1
    }
    if (.SD[i, spd %between% c( (spdMean - closingRange * spdSD), (spdMean + closingRange * spdSD) )]) {
        tempRes <- 0
    }
    return(tempRes)
})]

dt_inSample[, signal := shift(signal, 1L, type = 'lag')]
dt_inSample[, spdRtn := (spd - shift(spd, 1L, type = 'lag')) / shift(spd, 1L, type = 'lag')]
dt_inSample <- dt_inSample[!is.na(spdRtn) & !is.na(spd)]

dt_inSample[, rtn := signal * (spd - shift(spd, 1L, type = 'lag')) / (close.y + abs(hedgingRatio) * close.x)]
dt_inSample <- dt_inSample[!is.na(rtn)]
p <- ggplot(dt_inSample, aes(x = 1:nrow(dt_inSample), y = cumsum(rtn))) + 
    geom_line(color = 'steelblue', size = 1.2)
ggplotly(p)
dt_inSample[,tradingCount := signal - shift(signal, 1L, type = 'lag')]

dt_inSample[,sum(rtn)] / nrow(dt_inSample[tradingCount != 0]) / 2


tempDailyRtn <- dt_inSample[, .(dailyRtn = .SD[,sum(rtn)]), by = 'TradingDay']

sharpRatio <- tempDailyRtn[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252/38)]
print(sharpRatio)

