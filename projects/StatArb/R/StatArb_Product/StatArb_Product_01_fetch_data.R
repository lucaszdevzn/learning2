################################################################################
## StatArb_Product_01_fetch_data.R
################################################################################


## =============================================================================
productA <- 'j'
productB <- 'jm'
## =============================================================================

fetchMain <- function(product, startDate = '20110101', endDate = '20170631') {
    mysql <- mysqlFetch('china_futures_bar', host = '127.0.0.1')
    sql <- paste("
            SELECT TradingDay, Product, main
            FROM main
            WHERE TradingDay BETWEEN", startDate,
            "AND", endDate,
            "AND Product = ", paste0("'",product,"'")
        )
    res <- dbGetQuery(mysql,sql) %>% as.data.table() %>% .[nchar(main) != 0]
    dbListConnections(MySQL())
    return(res)
}

fetchMultiplier <- function(product, startDate = '20110101', endDate = '20170631') {
    mysql <- mysqlFetch('china_futures_bar', host = '127.0.0.1')
    mainContract <- fetchMain(product)
    dbWriteTable(mysql, 'tmp', mainContract, row.name = FALSE, overwrite = TRUE)

    sql <- paste("
            SELECT a.TradingDay
                  ,a.InstrumentID
                  ,a.multiplier
            FROM multiplier as a,
                 tmp as b
            WHERE a.TradingDay = b.TradingDay
            AND a.InstrumentID = b.main
            AND a.TradingDay BETWEEN", startDate,
            "AND", endDate
        )
    res <- suppressWarnings(dbGetQuery(mysql, sql)) %>% as.data.table()
    dbListConnections(MySQL())
    return(res)
}

fetchBar <- function(product, bar = 'daily', 
    startDate = '20110101', endDate = '20170631') {
    mysql <- mysqlFetch('china_futures_bar', host = '127.0.0.1')
    mainContract <- fetchMain(product)
    dbWriteTable(mysql, 'tmp', mainContract, row.name = FALSE, overwrite = TRUE)

    sql <- paste("
            SELECT a.TradingDay",
                ifelse(bar == 'daily', '', ',a.Minute as minute, a.NumericExchTime'),
                  ",a.InstrumentID
                  ,a.OpenPrice as open
                  ,a.HighPrice as high
                  ,a.LowPrice as low
                  ,a.ClosePrice as close
                  ,a.Volume as volume
                  ,a.Turnover as turnover
                  ,a.UpperLimitPrice as upper
                  ,a.LowerLimitPrice as lower
            FROM ", ifelse(bar == 'daily', 'daily', 'minute'), "as a,
                 tmp as b
            WHERE a.TradingDay = b.TradingDay
            AND a.InstrumentID = b.main
            AND a.TradingDay BETWEEN", startDate,
            "AND", endDate
        )
    res <- suppressWarnings(dbGetQuery(mysql, sql)) %>% as.data.table()
    dbListConnections(MySQL())
    return(res)
}

## =============================================================================
dtDailyA <- fetchBar(productA)
dtDailyB <- fetchBar(productB)

dtDaily <- merge(dtDailyA, dtDailyB, by = c('TradingDay'))
## =============================================================================

## =============================================================================
dtMinuteA <- fetchBar(productA, bar = 'minute')
dtMinuteB <- fetchBar(productB, bar = 'minute')
## =============================================================================

# p <- ggplot(dtMinuteA, aes(x = 1:nrow(dtMinuteA), y = close)) +
#         geom_line(color = 'steelblue') +
#         labs(title = paste('MinuteBar :==> ', productA),
#              x = 'Minute', y = 'close',
#              caption = '@william') 
# ggplotly(p)

# p <- ggplot(dtMinuteB, aes(x = 1:nrow(dtMinuteB), y = close)) +
#         geom_line(color = 'steelblue') +
#         labs(title = paste('MinuteBar :==> ', productB),
#              x = 'Minute', y = 'close',
#              caption = '@william') 
# ggplotly(p)


# temp <- rbind(dtMinute[,.(InstrumentID = InstrumentID.x, close = close.x)],
#               dtMinute[,.(InstrumentID = InstrumentID.y, close = close.y)])
# p <- ggplot(temp, aes(x = 1:nrow(temp), y = close, color = InstrumentID)) +
#         geom_line()

