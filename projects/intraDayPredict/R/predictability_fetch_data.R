rm(list = ls())
setwd("~/myCodes/CTA_intraday_predictability")

suppressMessages({
  source("./conf/myInit.R");
  source("./conf/myConfig.R")
})

if(! "mainContMinuteData.csv" %in% list.files("./data",pattern='\\.csv')){
  mysql <- mysqlFetch('china_futures_bar')
  dbListTables(mysql)
  dt <- dbGetQuery(mysql,"
                 SELECT a.TradingDay,
                        a.Minute,
                        a.NumericExchTime,
                        a.InstrumentID,
                        a.OpenPrice as open,
                        a.HighPrice as high,
                        a.LowPrice  as low,
                        a.ClosePrice as close,
                        a.Volume    as volume,
                        a.Turnover  as turnover
                 From minute as a,
                      main_contract_daily as b
                 WHERE a.TradingDay = b.TradingDay
                 AND   a.InstrumentID = b.Main_contract;") %>%
    as.data.table() %>%
    .[! grep("IF|IC|IH|T|TF",InstrumentID)]
  fwrite(dt,"./data/mainContMinuteData.csv")
}

dt <- fread("./data/mainContMinuteData.csv") %>%
  .[,.SD[order(TradingDay,NumericExchTime)],by='InstrumentID']
dt[,TradingDay := as.Date(TradingDay,'%Y-%m-%d')]

# dt[grep('cu',InstrumentID)]


# ------------------------------------------------------------------------------
# In samle 
# 2011-2012,2014,2015

# Out of sample
# 2013,2016,2017

inSample <- dt[! format(TradingDay,'%Y') %in% c(2013,2016,2017)]
outSample <- dt[format(TradingDay,'%Y') %in% c(2013,2016,2017)]
fwrite(inSample,'./data/inSample.csv')
fwrite(outSample,'./data/outSample.csv')
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
inSample <- fread('./data/inSample.csv') %>%
  .[,TradingDay := as.Date(TradingDay)]

dtY <- inSample %>%
  .[,.(closeRtn = (.SD[Minute <= "15:00:00"][.N,close] -
                     .SD[Minute <= "14:30:00"][.N,close]) /
         .SD[Minute <= "14:30:00"][.N,close]),
    by = c('TradingDay','InstrumentID')]
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
outSample <- fread('./data/outSample.csv') %>%
  .[,TradingDay := as.Date(TradingDay)]
outSample
# -----------------------------------------------------------------------------


# ==============================================================================
# 13:oo - 14:30
# ==============================================================================
portRtn <- inSample[,.(portRtn = (.SD[Minute <= "15:00:00"][.N,close] -
                             .SD[Minute <= "14:31:00"][.N,close]) /
              .SD[Minute <= "14:31:00"][.N,close]),
         by = c('TradingDay','InstrumentID')]

if(FALSE){
dtX <- inSample %>%
  .[,.( rtn_09 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[Minute <= '09:00:00'][.N,close]) /
          .SD[Minute <= '09:00:00'][.N,close] ,
        rtn_10 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[Minute <= '10:00:00'][.N,close]) /
          .SD[Minute <= '10:00:00'][.N,close] ,
        rtn_11 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[Minute <= '11:00:00'][.N,close]) /
          .SD[Minute <= '11:00:00'][.N,close] ,
        rtn_13 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[Minute <= '13:00:00'][.N,close]) /
          .SD[Minute <= '13:00:00'][.N,close] ),
    by = c('TradingDay','InstrumentID')]  
}

dtX <- inSample %>%
  .[,.( rtn_09 = (.SD[Minute <= ('14:30:00')][.N,close] - 
                 .SD[NumericExchTime <= ('09:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close]) /
          .SD[NumericExchTime <= ('09:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] ,
        rtn_09_10 = (.SD[NumericExchTime <= ('10:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] - 
                 .SD[NumericExchTime <= ('09:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close]) /
          .SD[NumericExchTime <= ('09:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] ,
        rtn_10 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[NumericExchTime <= ('10:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close]) /
          .SD[NumericExchTime <= ('10:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] ,
        rtn_11 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[NumericExchTime <= ('11:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close]) /
          .SD[NumericExchTime <= ('11:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] ,
        rtn_13 = (.SD[Minute <= '14:30:00'][.N,close] - 
                 .SD[NumericExchTime <= ('13:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close]) /
          .SD[NumericExchTime <= ('13:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] ),
    by = c('TradingDay','InstrumentID')]

dtX[is.na(rtn_09),rtn_09 := 0]
dtX[is.na(rtn_09_10),rtn_09_10 := 0]
dtX[is.na(rtn_10),rtn_10 := 0]
dtX[is.na(rtn_11),rtn_11 := 0]
dtX[is.na(rtn_13),rtn_13 := 0]
# ==============================================================================

dtRes <- merge(dtX,dtY, by = c('TradingDay','InstrumentID'))
dtRes

## 
fit_09 <- lm(closeRtn ~ rtn_09 + 0, data = dtRes)
summary(fit_09)

fit_09_10 <- lm(closeRtn ~ rtn_09_10 + 0, data = dtRes)
summary(fit_09_10)

fit_10 <- lm(closeRtn ~ rtn_10 + 0, data = dtRes)
summary(fit_10)

fit_11 <- lm(closeRtn ~ rtn_11 + 0, data = dtRes)
summary(fit_11)

fit_13 <- lm(closeRtn ~ rtn_13 + 0, data = dtRes)
summary(fit_13)


# ------------------------------------------------------------------------------
# Sharp Ratio
# ------------------------------------------------------------------------------
dtRes2 <- dtRes[,.SD]
dtRes2[,":="(portWeight_09  = abs(rtn_09) / (sum(abs(rtn_09),na.rm = TRUE) + 0.0001),
             portWeight_09_10  = abs(rtn_09_10) / (sum(abs(rtn_09_10),na.rm = TRUE) + 0.0001),
             portWeight_10  = abs(rtn_10) / (sum(abs(rtn_10),na.rm = TRUE) + 0.0001),
             portWeight_11  = abs(rtn_11) / (sum(abs(rtn_11),na.rm = TRUE) + 0.0001),
             portWeight_13  = abs(rtn_13) / (sum(abs(rtn_13),na.rm = TRUE) + 0.0001)
             ),by = 'TradingDay']
if(FALSE){
dtRes2[is.na(portWeight_09), portWeight_09 := 0]
dtRes2[is.na(portWeight_09_10), portWeight_09_10 := 0]
dtRes2[is.na(portWeight_10), portWeight_10 := 0]
dtRes2[is.na(portWeight_11), portWeight_11 := 0]
dtRes2[is.na(portWeight_13), portWeight_13 := 0] 
}

dtRes2 <- merge(dtRes2,portRtn, by = c('TradingDay','InstrumentID'))

dtDailyRtn  <- dtRes2[,.SD[, .(
  dailyRtn_09 = portWeight_09 %*% diag(sign(rtn_09)) %*% portRtn * sign(fit_09$coefficients[1]),
  dailyRtn_09_10 = portWeight_09_10 %*% diag(sign(rtn_09_10)) %*% portRtn * sign(fit_09_10$coefficients[1]),
  dailyRtn_10 = portWeight_10 %*% diag(sign(rtn_10)) %*% portRtn * sign(fit_10$coefficients[1]),
  dailyRtn_11 = portWeight_11 %*% diag(sign(rtn_11)) %*% portRtn * sign(fit_11$coefficients[1]),
  dailyRtn_13 = portWeight_13 %*% diag(sign(rtn_13)) %*% portRtn * sign(fit_13$coefficients[1])
  )],by = c('TradingDay')]

dtDailyRtn[, ":="(simpleCumRtn_09 = cumsum(dailyRtn_09),
                  nav_09          = cumprod(1+dailyRtn_09),
                  simpleCumRtn_09_10 = cumsum(dailyRtn_09_10),
                  nav_09_10          = cumprod(1+dailyRtn_09_10),
                  simpleCumRtn_10 = cumsum(dailyRtn_10),
                  nav_10          = cumprod(1+dailyRtn_10),
                  simpleCumRtn_11 = cumsum(dailyRtn_11),
                  nav_11          = cumprod(1+dailyRtn_11),
                  simpleCumRtn_13 = cumsum(dailyRtn_13),
                  nav_13          = cumprod(1+dailyRtn_13)
                  )]

temp <- gather(dtDailyRtn[,.(TradingDay,nav_09,nav_09_10,nav_10,nav_11,nav_13)]
              , key, value, nav_09:nav_13)
p <- ggplot(temp, aes(x = TradingDay, y = value, color = key)) +
       geom_line()
ggplotly(p)


dtDailyRtn[,.(SR_09 = mean(dailyRtn_09)/sd(dailyRtn_09) * sqrt(250),
              SR_09_10 = mean(dailyRtn_09_10)/sd(dailyRtn_09_10) * sqrt(250),
              SR_10 = mean(dailyRtn_10)/sd(dailyRtn_10) * sqrt(250),
              SR_11 = mean(dailyRtn_11)/sd(dailyRtn_11) * sqrt(250),
              SR_13 = mean(dailyRtn_13)/sd(dailyRtn_13) * sqrt(250))]
beep(8)

# =============================================================================

dtX <- inSample %>%
        .[,.( rtn = (.SD[Minute <= ('10:00:00')][.N,close] - 
                 .SD[NumericExchTime <= ('09:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close]) /
          .SD[NumericExchTime <= ('09:00:00' %>% substr(.,1,2) %>% as.numeric(.) * 3600 )][.N,close] ),
    by = c('TradingDay','InstrumentID')]
dtRes <- merge(dtX,dtY, by = c('TradingDay','InstrumentID'))
dtRes

## 
fit_09_10 <- lm(closeRtn ~ rtn_09_10 + 0, data = dtRes)
summary(fit_09_10)

dtRes2 <- dtRes[,.SD]
dtRes2[,":="(portWeight_09_10  = abs(rtn_09_10 / (sum(abs(rtn_09_10),na.rm = TRUE)) + 0.0001)
             ),by = 'TradingDay']
dtRes2 <- merge(dtRes2,portRtn, by = c('TradingDay','InstrumentID'))

dtDailyRtn  <- dtRes2[,.SD[, .(
  dailyRtn_09_10 = portWeight_09_10 %*% diag(sign(rtn_09_10)) %*% portRtn * sign(fit_09_10$coefficients[1])
  )],by = c('TradingDay')]

dtDailyRtn[, ":="(simpleCumRtn_09_10 = cumsum(dailyRtn_09_10),
                  nav_09_10          = cumprod(1+dailyRtn_09_10)
                  )]

# =============================================================================


p <- ggplot(dtDailyRtn, aes(TradingDay, nav)) +
  geom_line(color = 'steelblue')
# p
ggplotly(p)

SR <- dtDailyRtn[,mean(dailyRtn)/sd(dailyRtn) * sqrt(250)]
SR



par(mfrow = c(2,2))
plot(fit)

layout(matrix(c(1,1,2,3),2,2,byrow=T))
plot(resid(fit))
acf(resid(fit),main='ACF')
pacf(resid(fit))

library(lmtest)
dwtest(fit,alt = 'two.sided')

hist(resid(fit))
# ==============================================================================









































dtDailyRtn <- dtPortRtn[,.SD[, .(dailyRtn = portWeight %*% portRtn * 
                                   sign(fit$coefficients[1]))],
                        by = c('TradingDay')]






library(forecast)
fit  <- auto.arima(dtRes[grep('cu',InstrumentID)]$closeRtn, seasonal = FALSE)
par(mfrow = c(1,1))
plot(fit)
str(fit)
summary(fit)

plot(forecast(fit,100))






pred <- pedict(fit, n.ahead = 100)
pred$pred

plot(pred$pred)



plot(forecast(fit))

pred <- predict(fit, n.ahead = 10)
plot(dtRes$closeRtn,type = 'l')
lines(pred$pred, col = 'red')
lines(pred$pred + 2*pred$se, col = 'red')
lines(pred$pred - 2*pred$se, col = 'red')

# ==============================================================================
# fit ARIMA
library(fArma)
closeRtnArma <- armaFit(~arma(2,2), data = dtRes$closeRtn)



WWWusage %>% forecast %>% plot
fit <- auto.arima(WWWusage)
predict(fit, n.ahead = 20)
