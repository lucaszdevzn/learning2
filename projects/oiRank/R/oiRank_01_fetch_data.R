################################################################################
## oiRank_01_fetch_data.R
## 提取数据
################################################################################
#
#
#-------------------------------------------------------------------------------
# 提取 oiRank 数据
#-------------------------------------------------------------------------------
mysql <- mysqlFetch('test')
# dbListTables(mysql)

oiRank <- dbGetQuery(mysql,paste0("
					 SELECT *
					 FROM oiRank
					 WHERE InstrumentID like ","'",bkTestingProduct,"%'")) %>%
		    as.data.table() %>%
		    .[grepl(bkTestingProduct,InstrumentID)] %>%
		    .[,TradingDay := as.Date(TradingDay,'%Y-%m-%d')]

oiRank
#-------------------------------------------------------------------------------
#
#
#-------------------------------------------------------------------------------
# 寻找主力合约
#-------------------------------------------------------------------------------
mysql <- mysqlFetch('china_futures_bar')
mainCont <- dbGetQuery(mysql,paste0("
					   SELECT TradingDay, main_contract as mainInstrumentID
					   FROM main_contract_daily
					   WHERE Product = ", "'", bkTestingProduct, "'")) %>%
			as.data.table() %>%
			.[!is.na(mainInstrumentID)] %>%
		    .[,TradingDay := as.Date(TradingDay,'%Y-%m-%d')]
#-------------------------------------------------------------------------------
#
#
#-------------------------------------------------------------------------------
# 主力合约的 oiRank
#-------------------------------------------------------------------------------
cl <- makeCluster(round(detectCores()*3/4),type = 'FORK')
mainContOI <- parLapply(cl, oiRank[,unique(TradingDay)], function(i){
	temp <- oiRank[TradingDay == i][InstrumentID == 
									mainCont[TradingDay == i,mainInstrumentID]]
	return(temp)
}) %>% rbindlist()
stopCluster(cl)
mainContOI[,chgAmount := DiffAmount / Amount]
mainContOI
#-------------------------------------------------------------------------------
#
#
#-------------------------------------------------------------------------------
# 主力合约的所有多空持仓汇总
# 并计算净多单
# netAmount = longAmount - shortAmount
# > 0:净多单
# < 0:净空单
#-------------------------------------------------------------------------------
dt <- mainContOI[ClassID %in% c('longPos','shortPos')] %>%
	  .[,.(sumAmount = sum(Amount)),
	  	by = c("TradingDay",'InstrumentID','ClassID')] %>%
	  .[,.(longAmount = .SD[ClassID == 'longPos',sumAmount],
	  	   shortAmount = .SD[ClassID == 'shortPos',sumAmount]),
	    by = c("TradingDay","InstrumentID")] %>%
	  .[,netAmount := longAmount - shortAmount]

dt
#-------------------------------------------------------------------------------
#
#
#-------------------------------------------------------------------------------
# 提取价格数据
#-------------------------------------------------------------------------------
mysql <- mysqlFetch('dev')
dbSendQuery(mysql,"DROP TABLE IF EXISTS tempFL;")
dbSendQuery(mysql,"
			CREATE TABLE tempFL(
			TradingDay      DATE      NOT NULL,
			InstrumentID    char(10)  NOT NULL, 
			longAmount      bigint    NULL,
			shortAmount     bigint    NULL,
			netAmount       bigint    NULL
			)")
dbWriteTable(mysql,'tempFL', dt,
			 row.name = FALSE,
			 append = TRUE)
dt <- dbGetQuery(mysql,"
				SELECT a.TradingDay as TradingDay,
					   a.InstrumentID as InstrumentID,
					   a.OpenPrice as open,
					   a.HighPrice as high,
					   a.LowPrice  as low,
					   a.ClosePrice as close,
					   a.Volume    as volume,
					   a.Turnover  as turnover,
					   a.CloseOpenInterest as oiAmount,
					   a.SettlementPrice as stlPrice,
					   b.longAmount  as longAmount,
					   b.shortAmount as shortAmount,
					   b.netAmount   as netAmount
				FROM china_futures_bar.daily as a,
					dev.tempFL as b
				WHERE a.TradingDay = b.TradingDay
				AND a.InstrumentID = b.InstrumentID
				AND a.Sector = 'allday'") %>% as.data.table() %>%
		    .[,TradingDay := as.Date(TradingDay,'%Y-%m-%d')]
#-------------------------------------------------------------------------------
#
#
dt <- dt[,":="(lead_1d_Close = shift(close,1L,type = 'lead'),
			   lead_5d_Close = shift(close,5L,type = 'lead'))][!is.na(lead_1d_Close) & !is.na(lead_5d_Close)] %>%
	   .[, ":="(lead_1d_Rtn  = (lead_1d_Close - close)/close,
	   			lead_5d_Rtn  = (lead_5d_Close - close)/close)]
head(dt,30)

if(FALSE){
source("/home/william/myCodes/myFunctions/gMeanPlot.R")
gMeanPlot(dt, x = 'netAmount', y = 'lead_5d_Rtn'
		  ,xgn = 40, ygn = 1, xtype = 'continuous',
		  title = paste('5-days leading Return vs netAmount',':==>',bkTestingProduct))	
}

#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------
p <- ggplot(dt, aes(TradingDay,close)) +
	geom_line(color = 'steelblue')
ggplotly(p)
