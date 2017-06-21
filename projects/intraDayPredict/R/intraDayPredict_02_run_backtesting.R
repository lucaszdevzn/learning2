## =============================================================================
startTime <- c("09:00","09:00","10:00","11:00", "09:00",
               "13:00","14:00","10:00")
endTime   <- c("09:30","10:00","11:00","13:30", "11:30",
               "14:30","14:30","13:30")

bkTestInterval <- data.table(startTime, endTime) %>%
                  .[order(startTime,endTime)]
## =============================================================================


## =============================================================================
calPredictor <- function(timePeriod){
    ## -------------------------------------------------------------------------
    dtX <- inSample %>%
      .[,.(openRtn = (.SD[Minute >= timePeriod[1,endTime]][1,close] - 
                       .SD[Minute <= timePeriod[1,startTime]][.N,close]) /
                       .SD[Minute <= timePeriod[1,startTime]][.N,close]),
          by = c('TradingDay','InstrumentID')]
    ## -------------------------------------------------------------------------
    tempRes <- list(dtX[!is.na(openRtn)])
    names(tempRes) <- timePeriod[, paste0(startTime, '-', endTime)]
    return(tempRes)
    ## -------------------------------------------------------------------------
}
## =============================================================================


## =============================================================================
cl <- makeCluster(detectCores(), type = 'FORK')
dtX <- parSapply(cl, 1:nrow(bkTestInterval), function(i){    # nrow(bkTestInterval)
    timePeriod <- bkTestInterval[i]
    return(
      calPredictor(timePeriod = timePeriod)
    )
})
stopCluster(cl)
## =============================================================================


## =============================================================================

dtY[, preCloseRtn := shift(closeRtn, 1L, type = 'lag'),
    by = 'InstrumentID']
dtY <- dtY[!is.na(preCloseRtn)]
print(dtY)

print(dtX[[1]])

for (i in 1:nrow(bkTestInterval)){
    dtXY <- merge(dtX[[i]], dtY, by = c('TradingDay','InstrumentID'))
    print(names(dtX[i]))

    fit <- lm(closeRtn ~ openRtn + I(openRtn * preCloseRtn) + 0, data = dtXY)
    print(summary(fit))

    p <- ggplot(data = dtXY[openRtn %between% c(-0.01,0.01)], aes(x = openRtn, y = closeRtn)) +
        geom_point(color = 'steelblue', alpha = 0.1, size = 0.8)+
        geom_smooth()+
        labs(title = names(dtX[i]))
    print(p)
    Sys.sleep(5)
}

tempIns <- 'cu'
fit <- lm(data = dtXY[substr(InstrumentID,1,nchar(tempIns)) == tempIns], closeRtn ~ openRtn + 0)
summary(fit)

ggplot(data = dtXY[substr(InstrumentID,1,nchar(tempIns)) == tempIns], aes(x = openRtn, y = closeRtn)) +
        geom_point(color = 'steelblue', alpha = 0.3, size = 3)+
        geom_smooth()+
        geom_abline(color = 'orange')
