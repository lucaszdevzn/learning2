#-------------------------------------------------------------------------------
# AshareCompany
#-------------------------------------------------------------------------------
mysql <- mysqlFetch('test')
aShare <- dbGetQuery(mysql,paste0("
			SELECT * FROM AshareCompany
			WHERE ProductID = ","'", bkTestingProduct, "'")) %>%
		  as.data.table()
head(aShare)

#-------------------------------------------------------------------------------
aShareInfo <- lapply(1:nrow(aShare), function(i){
	# print(i)
	temp <- dt[,.(TradingDay,stockID = aShare[i,stockID],
				  stockName = aShare[i,stockName],
				  IndustryID = aShare[i,IndustryID])]
	mysql <- mysqlFetch('dev')
	dbSendQuery(mysql,"DROP TABLE IF EXISTS tempFL;")
	dbSendQuery(mysql,"CREATE TABLE tempFL(
				TradingDay    Date    NOT NULL,
				stockID       varchar(20)    NOT NULL,
				stockName     varchar(20)    NOT NULL,
				IndustryID    varchar(20)    NOT NULL);")
	dbWriteTable(mysql,"tempFL",temp, row.name = FALSE,
				append = TRUE)
	tempRes <- dbGetQuery(mysql, "
				SELECT a.TradingDay as TradingDay,
				       a.stockID    as stockID,
				       b.stockName  as stockName,
				       b.IndustryID as IndustryID,
				       a.open       as open,
				       a.high       as high,
				       a.low        as low,
				       a.close      as close,
				       a.volume     as volume,
				       a.Turnover   as turnover,
				       a.vwap       as vwap,
				       a.tradeStatus as tradeStatus
				FROM dev.wind_daily AS a,
					 dev.tempFL AS b
				WHERE a.TradingDay = b.TradingDay
				AND   a.stockID = b.stockID;") %>%
			as.data.table()

	tempRes <- tempRes[,":="(lead_1d_Close = shift(close,1L,type = 'lead'),
			   lead_5d_Close = shift(close,5L,type = 'lead'))][!is.na(lead_1d_Close) & !is.na(lead_5d_Close)] %>%
	   .[, ":="(lead_1d_Rtn  = (lead_1d_Close - close)/close,
	   			lead_5d_Rtn  = (lead_5d_Close - close)/close)]

	return(tempRes)
	}) %>% rbindlist() %>%
	.[,TradingDay := as.Date(TradingDay)]
head(aShareInfo)
#-------------------------------------------------------------------------------
p <- ggplot() +
	geom_line(data = dt, aes(x = TradingDay, y = close/2000)
			,size = 1.2, color = 'darkorange') +
	geom_line(data = aShareInfo[IndustryID == 'downStream'], 
		aes(x = TradingDay, y = close, color = stockID), size = .5) +
	labs(title = "Close Price")
ggplotly(p)


#-------------------------------------------------------------------------------
corInfo <- lapply(aShareInfo[,unique(stockID)],function(i){
	temp <- aShareInfo[stockID == i]
	temp <- merge(temp,dt, by = 'TradingDay')
	
	tempRes <- temp[,.(stockID = i,
					   stockName = aShare[stockID == i, unique(stockName)],
					   IndustryID = aShare[stockID == i, unique(IndustryID)],
					   corClose = cor(close.x, close.y),
					   corRtn_1d = cor(lead_1d_Rtn.x, lead_1d_Rtn.y),
					   corRtn_5d = cor(lead_5d_Rtn.x, lead_5d_Rtn.y))]
}) %>% rbindlist()

temp <- corInfo[! abs(corClose) %between% c(quantile(corClose,.02),quantile(corClose,.98))]
print("Correlation of Close Price")
print(temp)
p1 <- ggplot() +
	geom_line(data = dt, aes(x = TradingDay, y = close/2000)
			,size = 1.2, color = 'darkorange') +
	geom_line(data = aShareInfo[stockID %in% temp[,stockID]], 
		aes(x = TradingDay, y = close, color = stockName), size = .5) +
	labs(title = "Close Price")
ggplotly(p1)

#-------------------------------------------------------------------------------

temp <- corInfo[! abs(corRtn_1d) %between% c(quantile(corRtn_1d,.05),quantile(corRtn_1d,.95))]
print("Correlation of Return on Close Price")
print(temp)
p2 <- ggplot() +
	geom_line(data = dt, aes(x = TradingDay, y = lead_1d_Rtn)
			,size = 1.2, color = 'darkorange') +
	geom_line(data = aShareInfo[stockID %in% temp[,stockID]], 
		aes(x = TradingDay, y = lead_1d_Rtn, color = stockName), size = .5) +
	labs(title = "1day Return of Close Price")
ggplotly(p2)

#-------------------------------------------------------------------------------
