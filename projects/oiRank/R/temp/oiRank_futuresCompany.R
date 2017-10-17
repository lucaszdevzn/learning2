
#-------------------------------------------------------------------------------
tempLong <- mainContOI[ClassID == 'longPos'][,unique(BrokerID)]
longBroker <- lapply(tempLong, function(i){
	temp <- mainContOI[ClassID == 'longPos'][BrokerID == i] %>%
		merge(.,dt, by = 'TradingDay')
	tempRes <- data.table(BrokerID = temp[,unique(BrokerID)],
						  corLong_1d  = temp[,cor(chgAmount,lead_1d_Rtn)],
						  corLong_5d  = temp[,cor(chgAmount,lead_5d_Rtn)],
						  numObs   = nrow(temp))
}) %>% rbindlist() %>% .[!is.na(corLong_1d) & !is.na(corLong_5d)] %>% .[numObs > 30]

tempBrokerID <- longBroker[! abs(corLong_1d) %between% 
							c(quantile(corLong_1d,0.05),quantile(corLong_1d,0.95))]
print("Correlation of Change in LongPosition and nDay Return")
tempBrokerID

for(id in tempBrokerID[,unique(BrokerID)]){
	#print(id)
	temp <- mainContOI[BrokerID == id][ClassID == 'longPos'] %>%
			merge(.,dt, by = 'TradingDay') %>%
			.[,":="(lead_1d_Rtn = lead_1d_Rtn * 10,lead_5d_Rtn = lead_5d_Rtn * 10)]
	if(nrow(temp) == 0) next
	temp <- gather(temp[,.(TradingDay,chgAmount,lead_1d_Rtn,lead_5d_Rtn)], key, value, chgAmount:lead_5d_Rtn)

	p1 <- ggplot(temp,aes(x = TradingDay, y = value, color = key)) + 
			geom_line()+ 
			labs(title = paste('Percentage Change of Long vs Return',id), subtitle = 'return * 10')
	print(ggplotly(p1))
}



#-------------------------------------------------------------------------------

tempShort <- mainContOI[ClassID == 'shortPos'][,unique(BrokerID)]
shortBroker <- lapply(tempShort, function(i){
	temp <- mainContOI[ClassID == 'shortPos'][BrokerID == i] %>%
		merge(.,dt, by = 'TradingDay')
	tempRes <- data.table(BrokerID = temp[,unique(BrokerID)],
						  corShort_1d  = temp[,cor(chgAmount,lead_1d_Rtn)],
						  corShort_5d  = temp[,cor(chgAmount,lead_5d_Rtn)],
						  numObs   = nrow(temp))
}) %>% rbindlist() %>% .[!is.na(corShort_1d) & !is.na(corShort_5d)] %>% .[numObs > 30]


tempBrokerID <- shortBroker[! abs(corShort_1d) %between% 
							c(quantile(corShort_1d,0.05),quantile(corShort_1d,0.95))]
print("Correlation of Change in ShortPosition and nDay Return")
tempBrokerID

for(id in tempBrokerID[,unique(BrokerID)]){
	#print(id)
	temp <- mainContOI[BrokerID == id][ClassID == 'shortPos'] %>%
			merge(.,dt, by = 'TradingDay') %>%
			.[,":="(lead_1d_Rtn = lead_1d_Rtn * 10,lead_5d_Rtn = lead_5d_Rtn * 10)]
	if(nrow(temp) == 0) next
	temp <- gather(temp[,.(TradingDay,chgAmount,lead_1d_Rtn,lead_5d_Rtn)], key, value, chgAmount:lead_5d_Rtn)

	p1 <- ggplot(temp,aes(x = TradingDay, y = value, color = key)) + 
			geom_line()+ 
			labs(title = paste('Percentage Change of Short vs Return',id), subtitle = 'return * 10')
	print(ggplotly(p1))
}

