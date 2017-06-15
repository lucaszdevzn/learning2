

#-------------------------------------------------------------------------------
# 某个期货公司的持仓排名变化
#-------------------------------------------------------------------------------
temp <- cbind(dt,data.table(longInList = rep(0), shortInList = rep(0)))
temp

for(i in 1:nrow(temp)){
	tempID <- temp[i,InstrumentID]
	tempTD <- temp[i, TradingDay]

	tempLong  <- oiRank[TradingDay == tempTD][InstrumentID == tempID][ClassID == 'longPos']
	tempLong  <- any(grepl('永安期货',tempLong$BrokerID))

	tempShort <- oiRank[TradingDay == tempTD][InstrumentID == tempID][ClassID == 'shortPos']
	tempShort <- any(grepl('永安期货',tempShort$BrokerID))

	temp[i]$longInList   <- ifelse(tempLong,1,0)
	temp[i]$shortInList <- ifelse(tempShort,1,0)

	# pb
	# pb <- txtProgressBar(min = 1, max = nrow(temp), style = 3)
	# setTxtProgressBar(pb,i)
}

temp <- temp[TradingDay >= '2016-01-01']

fit <- lm(temp$lead_5d_Rtn ~ temp$longInList + temp$shortInList)
summary(fit)
