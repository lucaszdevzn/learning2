################################################################################
## oiRank_02_alpha_cal_product.R
## 
## 计算 productID
## 
## Input:
## @param dtOiRank
## 
## Output:
## @param 
## 
## Author: William Fang
## Created Date: 2017-07-24
################################################################################


## =============================================================================
## 获取合约品种
## =============================================================================
## 去掉股指期货
# dt <- dtOiRank[!grep('^(IF|IH|IC|TF|T)[0-9]{4}', InstrumentID)]
# tempInstrumentID <- c('cu','ru','rb','ni','zn')
tempInstrumentID <- c('rb')
dt <- dtOiRank[ProductID %in% tempInstrumentID][ClassID == 'Turnover'] %>% 
    .[TradingDay %between% c('2015-10-01','2017-07-01')]

dt <- dt[, .(total = sum(Amount))
   , by = c('TradingDay', 'BrokerID', 'ProductID')]

dt[, totalPct := total / .SD[, sum(total)]
   , by = c('TradingDay','ProductID')]

## -----------------------------------------------------------------------------
## 选择进入排名前 10 的期货公司
## 
dt[, RankNew := rank(-totalPct, ties.method = 'max')
   , by = c('TradingDay','ProductID')]
# dt[TradingDay == '2011-01-04' & ProductID == 'IF'][order(-total)]

## 做判断，如果进入前 10, 得分 1
dt[, RankScore := ifelse(RankNew <= 10, 1, 0)]

topCompany <- dt[, .(totalRankScore = sum(RankScore))
   , by = c('ProductID','BrokerID')] %>% 
   .[order(-totalRankScore)] %>% 
   .[1:10,BrokerID]
   # .[totalRankScore > 200, unique(BrokerID)]
topCompany

dt <- dt[BrokerID %in% topCompany]

## =============================================================================


## =============================================================================
## 画图看看
## =============================================================================
p <- ggplot(dt, aes(x = TradingDay, y = totalPct, color = BrokerID)) +
    geom_line(size = 0.3) +
    labs(x = 'TradingDay', y = 'totalPct', title = tempInstrumentID)
ggplotly(p)
htmlwidgets::saveWidget(ggplotly(p), paste0("~/plotly/",tempInstrumentID,"_total.html")

## 
p <- ggplot(dt, aes(x = TradingDay, y = total, color = BrokerID)) +
    geom_line(size = 0.3) +
    labs(x = 'TradingDay', y = 'total', title = tempInstrumentID)
ggplotly(p)
htmlwidgets::saveWidget(ggplotly(p), paste0("~/plotly/",tempInstrumentID,"_totalPct.html")
