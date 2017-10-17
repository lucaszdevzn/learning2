## =============================================================================
## 
## =============================================================================
product <- 'i'
lagMonth <- 4

estimatedPeriod <- 100
estimatedRange  <- 3
closingRange    <- 2

mysql <- mysqlFetch('china_futures_bar')

query <- paste0("SELECT DISTINCT main_contract
                FROM main_contract_daily
                WHERE Product = ", "'", product,"'",
                " AND TradingDay >= 20160701")
temp <- dbGetQuery(mysql,query) %>% as.data.table() %>% .[-c(1:1)] # 去掉第一个
temp

productInfo <- data.table()
for (i in 1:nrow(temp)) {
    ## -------------------------------------------------------------------------
    tempMain <- temp[i,main_contract]
    tempSub <- gsub('[a-zA-Z]', '', tempMain) %>% 
               as.yearmon(.,'%y%m')  %>% 
               as.Date(.) %m+% months(lagMonth) %>% 
               as.character() %>% 
               gsub('-','',.) %>% 
               substr(.,3,6) %>% 
               paste0(product,.)
    query <- paste0("select min(TradingDay), max(TradingDay)
                    from main_contract_daily
                    where main_contract = ", "'", tempMain, "'")
    tempRes <- dbGetQuery(mysql, query)
    ## -------------------------------------------------------------------------
    productInfo <- rbind(productInfo,
        data.table(tempRes[1,1], tempRes[1,2], tempMain, tempSub))
}
names(productInfo) <- c('beginDate', 'endDate', 'mainContract', 'subContract')
# productInfo <- productInfo[-1]

tempFields <- c('TradingDay', 'Minute', 'NumericExchTime', 
                'InstrumentID',
                'OpenPrice as open',
                'HighPrice as high',
                'LowPrice as low',
                'ClosePrice as close',
                'Volume as volume', 'Turnover as turnover')

dtMain <- lapply(1:nrow(productInfo), function(i){
    query <- paste("select", paste(tempFields, collapse = ','),
                   "from minute
                    where TradingDay between",productInfo[i, gsub('-','',beginDate)],
                    "and ",productInfo[i, gsub('-','',endDate)],
                    "and InstrumentID =", productInfo[i,paste0("'",mainContract,"'")])
    tempRes <- dbGetQuery(mysql,query) %>% as.data.table()
}) %>% rbindlist()

dtSub <- lapply(1:nrow(productInfo), function(i){
    query <- paste("select", paste(tempFields, collapse = ','),
                   "from minute
                    where TradingDay between",productInfo[i, gsub('-','',beginDate)],
                    "and ",productInfo[i, gsub('-','',endDate)],
                    "and InstrumentID =", productInfo[i,paste0("'",subContract,"'")])
    tempRes <- dbGetQuery(mysql,query) %>% as.data.table()
}) %>% rbindlist()

dt <- merge(dtSub, dtMain, by = c('TradingDay','Minute','NumericExchTime')) %>% 
        .[order(TradingDay, NumericExchTime)]

p <- ggplot(dt) +
    geom_line(aes(x = 1:nrow(dt), y = close.x), color = 'orange') +
    geom_line(aes(x = 1:nrow(dt), y = close.y), color = 'steelblue')
print(p)
dev.new()
ggplotly(p)

## =============================================================================

dt_inSample <- dt[TradingDay < '2017-05-01']
dt_outSample <- dt[TradingDay >= '2017-05-01']

dt_inSample[, spd := close.y - close.x]

dt_inSample[, ':='(
    spdMean = sapply(1:nrow(.SD), function(i){
        if (i <= estimatedPeriod) {
            return(NA)
        } else {
            temp <- .SD[(i - estimatedPeriod + 1) : (i)]
            tempRes <- mean(temp$close.y - temp$close.x)
        }
    }),
    spdSD = sapply(1:nrow(.SD), function(i){
        if (i <= estimatedPeriod) {
            return(NA)
        } else {
            temp <- .SD[(i - estimatedPeriod + 1) : (i)]
            tempRes <- sd(temp$close.y - temp$close.x)
        }
    })
)]

dt_inSample <- dt_inSample[!is.na(spdMean) & !is.na(spdSD)]

dt_inSample[, ':='(spdUpper = spdMean + estimatedRange * spdSD,
                   spdLower = spdMean - estimatedRange * spdSD)]

## =============================================================================

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

dt_inSample[, rtn := -1 * signal * (spd - shift(spd, 1L, type = 'lag')) / (close.y + close.x)]
dt_inSample <- dt_inSample[!is.na(rtn)]
p <- ggplot(dt_inSample, aes(x = 1:nrow(dt_inSample), y = cumsum(rtn))) + 
    geom_line(color = 'steelblue', size = 1, alpha = 0.5)
print(p)
ggplotly(p)
dt_inSample[,tradingCount := abs(signal) - abs(shift(signal, 1L, type = 'lag'))]
print(nrow(dt_inSample[tradingCount != 0]))

dt_inSample[,sum(rtn)] / nrow(dt_inSample[tradingCount != 0])

## =============================================================================
dt_inSample[, yRtn :=  (spd - shift(spd, 30L, type = 'lag')) /
                       (close.y + close.x)]
dt_inSample <- dt_inSample[!is.na(yRtn)]

dt_inSample[, spdMeanMA := sapply(1:nrow(.SD), function(i){
    if (i <= 30){
        return(NA)
    } else {
        temp <- .SD[( i- 30 + 1) : (i)]
        tempRes <- temp[,mean(spdMean)]
    }
    return(tempRes)
})]
tempX  <- dt_inSample[!is.na(spdMeanMA), spdMeanMA]
tempY <- dt_inSample[!is.na(spdMeanMA), yRtn]

fit <- lm(tempY ~ tempX + 0);summary(fit)



tempDailyRtn <- dt_inSample[, .(dailyRtn = .SD[,sum(rtn)]), by = 'TradingDay']

sharpRatio <- tempDailyRtn[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252)]
print(sharpRatio)

dtRtn <- dt_inSample[!is.na(spdMeanMA),.(spdMeanMA, yRtn)]
dtRtn[, hatRtn := yRtn * sign(-fit$coefficients)]

dtRtn[, cumRtn := cumsum(hatRtn)]

ggplot(dtRtn, aes(x = 1:nrow(dtRtn), y = cumRtn)) + 
    geom_line(color = 'steelblue')


## =============================================================================
## 预测
## =============================================================================
dt_inSample <- dt[TradingDay < '2017-04-01']

forecastPeriod <- 60

dt_inSample[, spd := close.y - close.x]
dt_inSample[, spdHigh := ((high.y) - (high.x))]

dt_inSample[, spdLead := shift(spd, forecastPeriod, type = 'lead')]
dt_inSample[, spdHighLead := shift(spdHigh, forecastPeriod, type = 'lead')]

dt_inSample[, ":="(
                spdRtn = (spdLead - spd) / (close.x + close.y)
            )]
dt_inSample[, summary(spdRtn)]
ggplot(dt_inSample, aes(x = 1:nrow(dt_inSample), y = spdRtn)) + 
    geom_point(color = 'orange', alpha = 0.1)

## 发现 spdRtn 不对
## i1701  i1705
## ...    ...
## i1701  i1705
## i1705  i1709

dt_inSample[, ":="(InstrumentXLag = shift(InstrumentID.x, forecastPeriod, type = 'lag'),
                   InstrumentYLag = shift(InstrumentID.y, forecastPeriod, type = 'lag'))]

dt_inSample <- dt_inSample[InstrumentID.x == InstrumentXLag][InstrumentID.y == InstrumentYLag]
dt_inSample[, spdRtn := (spdLead - spd) / (close.x + close.y)]
dt_inSample[, spdHighRtn := (spdHighLead - spdHigh) / (close.x + close.y)]

dt_inSample <- dt_inSample[!is.na(spd) & !is.na(spdRtn) & !is.na(spdHighRtn)]

ggplot(dt_inSample, aes(x = 1:nrow(dt_inSample), y = spdRtn)) + 
    geom_point(color = 'hotpink', alpha = 0.1)
dt_inSample[, summary(spdRtn)]

fit <- lm(data = dt_inSample, spdRtn ~ 1); summary(fit)

## spd：做标准化：z-score 
system.time({
## -----------------------------------------------------------------------------
cl <- makeCluster(detectCores(), type = 'FORK')
tempZ <- parSapply(cl, 1:nrow(dt_inSample), function(i){
    if (i <= estimatedPeriod) {
        tempRes <- NA
    } else {
        temp <- dt_inSample[(i - estimatedPeriod) : (i)]
        if (temp[1, InstrumentID.x] != temp[.N, InstrumentID.x] |
           temp[1, InstrumentID.y] != temp[.N, InstrumentID.y] ) {
            tempRes <- NA
        } else {
            tempRes <- (temp[.N, spd] - temp[, mean(spd)]) / temp[, sd(spd)]
        }
    }
    return(tempRes)
})
stopCluster(cl)
## -----------------------------------------------------------------------------
})

dt_inSample[, spdZ := tempZ]

## spdRank：做标准化：z-score 
system.time({
## -----------------------------------------------------------------------------
cl <- makeCluster(detectCores(), type = 'FORK')
tempRank <- parSapply(cl, 1:nrow(dt_inSample), function(i){
    if (i <= estimatedPeriod) {
        tempRes <- NA
    } else {
        temp <- dt_inSample[(i - estimatedPeriod) : (i)]
        if (temp[1, InstrumentID.x] != temp[.N, InstrumentID.x] |
           temp[1, InstrumentID.y] != temp[.N, InstrumentID.y] ) {
            tempRes <- NA
        } else {
            tempRes <- base::rank(temp$spd, ties.method = 'max')[which(temp$spd == temp[.N, spd])][1]
        }
    }
    return(tempRes)
})
stopCluster(cl)
## -----------------------------------------------------------------------------
})

dt_inSample[, spdRank := tempRank]

## spd：做标准化：z-score 
system.time({
## -----------------------------------------------------------------------------
cl <- makeCluster(detectCores(), type = 'FORK')
tempRankZ <- parSapply(cl, 1:nrow(dt_inSample), function(i){
    if (i <= estimatedPeriod) {
        tempRes <- NA
    } else {
        temp <- dt_inSample[(i - estimatedPeriod) : (i)]
        if (temp[1, InstrumentID.x] != temp[.N, InstrumentID.x] |
           temp[1, InstrumentID.y] != temp[.N, InstrumentID.y] ) {
            tempRes <- NA
        } else {
            tempRes <- (temp[.N, spdRank] - temp[, mean(spdRank)]) / temp[, sd(spdRank)]
        }
    }
    return(tempRes)
})
stopCluster(cl)
## -----------------------------------------------------------------------------
})

dt_inSample[, spdRankZ := tempRankZ]

## spd：做标准化：z-score 
system.time({
## -----------------------------------------------------------------------------
cl <- makeCluster(detectCores(), type = 'FORK')
tempRankZM <- parSapply(cl, 1:nrow(dt_inSample), function(i){
    if (i <= estimatedPeriod) {
        tempRes <- NA
    } else {
        temp <- dt_inSample[(i - estimatedPeriod) : (i)]
        if (temp[1, InstrumentID.x] != temp[.N, InstrumentID.x] |
           temp[1, InstrumentID.y] != temp[.N, InstrumentID.y] ) {
            tempRes <- NA
        } else {
            tempRes <- base::rank(temp$spd, ties.method = 'max')[which(temp$spd == temp[.N, spd])][1]
            tempRes <- (tempRes - median((base::rank(temp$spd, ties.method = 'max')))) / nrow(temp)
        }
    }
    return(tempRes)
})
stopCluster(cl)
## -----------------------------------------------------------------------------
})

dt_inSample[, spdRankZM := tempRankZM]

##
## spdRtn ~ spdZ
##
dtRtn <- dt_inSample[!is.na(spdRtn) & !is.na(spdZ) & !is.na(spdRank) & !is.na(spdRankZ) & !is.na(spdRankZM)]
fit <- lm(data = dtRtn, spdRtn ~ spdZ + 0)
summary(fit)

temp <- dtRtn


fit <- lm(data = dtRtn, spdRtn ~ spdRankZ + 0)
summary(fit)

fit <- lm(data = dtRtn, spdRtn ~ spdRankZM + 0)
summary(fit)

p <- ggplot(temp) +
    geom_line(aes(x = 1:nrow(temp), y = spdZ * 3 * fit$coefficients), color = 'orange', alpha = 0.8, size = 0.5) + 
    geom_line(aes(x = 1:nrow(temp), y = spdRtn), color = 'steelblue', alpha = 0.5, size = 0.6)
print(p)
ggplotly(p)

fit <- lm(data = dtRtn, spdRtn ~ spdRankZ + 0)
summary(fit)
p <- ggplot(temp) +
    geom_line(aes(x = 1:nrow(temp), y = spdRankZ * 3 * fit$coefficients), color = 'orange', alpha = 0.8, size = 0.5) + 
    geom_line(aes(x = 1:nrow(temp), y = spdRtn), color = 'steelblue', alpha = 0.5, size = 0.6)
print(p)
ggplotly(p)

dtRtn[, rtn := -spdRtn * sign(fit$coefficients)]
dtRtn[, cumRtn := cumsum(rtn)]

ggplot(dtRtn, aes(x = 1:nrow(dtRtn), y = cumRtn)) +
    geom_line(color = 'steelblue')

tempDailyRtn <- dtRtn[, .(dailyRtn = .SD[,sum(rtn)]), by = 'TradingDay']
sharpRatio <- tempDailyRtn[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252)]
print(sharpRatio)


dtRtn[spdZ > 2, signal := 1]
dtRtn[spdZ < -2, signal := -1]
dtRtn[is.na(signal), signal := 0]
dtRtn[, forecastRtn := -spdRtn * signal]
dtRtn[, forecastRtnCum := cumsum(forecastRtn)]

tempDailyRtn <- dtRtn[, .(dailyRtn = .SD[,sum(forecastRtn)]), by = 'TradingDay']
sharpRatio <- tempDailyRtn[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252)]
print(sharpRatio)
tempDailyRtn$TradingDay <- ymd(tempDailyRtn$TradingDay)
ggplot(tempDailyRtn, aes(x = TradingDay, y = dailyRtn)) + geom_line()

ggplot(dtRtn, aes(x = 1:nrow(dtRtn), y = forecastRtnCum)) +
    geom_line(color = 'steelblue')
## -----------------------------------------------------------------------------
# dt_inSample[, ':='(
#     spdMean = sapply(1:nrow(.SD), function(i){
#         if (i <= estimatedPeriod) {
#             tempRes <- NA
#         } else {
#             temp <- .SD[(i - estimatedPeriod) : (i)]
#             if (temp[1,InstrumentID.x] != temp[.N, InstrumentID.x] |
#                 temp[1,InstrumentID.y] != temp[.N, InstrumentID.y] ){
#                 tempRes <- NA
#             } else {
#                 tempRes <- mean(temp$close.y - temp$close.x)
#             }
#         }
#         return(tempRes)
#     }),
#     spdSD = sapply(1:nrow(.SD), function(i){
#         if (i <= estimatedPeriod) {
#             tempRes <- NA
#         } else {
#             temp <- .SD[(i - estimatedPeriod) : (i)]
#             if (temp[1,InstrumentID.x] != temp[.N, InstrumentID.x] |
#                 temp[1,InstrumentID.y] != temp[.N, InstrumentID.y] ){
#                 tempRes <- NA
#             } else {
#                 tempRes <- sd(temp$close.y - temp$close.x)
#             }
#         }
#         return(tempRes)
#     })
# )]

# dt_inSample[, ':='(spdUpper = spdMean + estimatedRange * spdSD,
#                    spdLower = spdMean - estimatedRange * spdSD)]

## -----------------------------------------------------------------------------
dtRtn <- dt_inSample[!is.na(spd) & !is.na(spdMean) & !is.na(spdSD) & !is.na(pairRtn)]

dtRtn[, summary(pairRtn)]

ggplot(dtRtn, aes(x = 1:nrow(dtRtn), y = pairRtn)) + 
    geom_point(color = 'steelblue', alpha = 0.05, size = 0.5)

fit <- lm(data = dt_inSample, spdRtn ~ spd + 0)
summary(fit)


ggplot(dt_inSample) +
    geom_line(aes(x = 1:nrow(dtRtn), y = spdSD / 10), color = 'orange') +
    geom_line(aes(x = 1:nrow(dtRtn), y = pairRtn), color = 'steelblue', alpha = 0.2)


## =============================================================================
dtY <- dtRtn[spdRtn %between% c(-0.006, 0.006)][!spdRtn %between% c(-0.001, 0.001)]
dtY[, rtn := -spdRtn * sign(fit$coefficients)]
dtY[, cumRtn := cumsum(rtn)]
dtY
ggplot(dtY, aes(1:nrow(dtY), y = cumRtn)) +
    geom_line(color = 'steelblue', alpha = 0.5, size = 1.2)
tempDailyRtn <- dtY[, .(dailyRtn = .SD[,sum(rtn)]), by = 'TradingDay']
sharpRatio <- tempDailyRtn[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252)]
print(sharpRatio)
dev.new()
tempDailyRtn$TradingDay <- ymd(tempDailyRtn$TradingDay)
ggplot(tempDailyRtn, aes(x = TradingDay, y = dailyRtn)) + geom_line(color = 'steelblue')
