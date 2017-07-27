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
# tempInstrumentID <- c('rb')

if (tempClassID == 'volume') {
  dt <- dtOiRank[ProductID %in% tempInstrumentID][ClassID == 'Turnover']
} else {
  dt <- dtOiRank[ProductID %in% tempInstrumentID][ClassID != 'Turnover']
}

dt <-  dt[TradingDay %between% c('2015-10-01', dtOiRank[,as.character(max(TradingDay))])] %>% 
        .[, .(total = sum(Amount))
          , by = c('TradingDay','ProductID','BrokerID')]

dt[, totalPct := total / .SD[, sum(total)]
   , by = c('TradingDay','ProductID')]

## -----------------------------------------------------------------------------
## 选择进入排名前 10 的期货公司
## 
dt[, RankNew := rank(-totalPct, ties.method = 'max')
   , by = c('TradingDay','ProductID')]
# dt[TradingDay == '2011-01-04' & ProductID == 'IF'][order(-total)]

## 做判断，如果进入前 10, 得分 1
dt[, RankScore := ifelse(RankNew <= 20, 1, 0)]

topCompany <- dt[, .(totalRankScore = sum(RankScore))
   , by = c('ProductID','BrokerID')] %>% 
   .[order(-totalRankScore)] %>% 
   .[1:20,BrokerID]
   # .[totalRankScore > 200, unique(BrokerID)]
topCompany

dt <- dt[BrokerID %in% topCompany]

## =============================================================================


## =============================================================================
## 画图看看
## =============================================================================
temp <- gather(dt[,.(TradingDay,BrokerID,total,totalPct)], 
              key, value, -c(TradingDay,BrokerID)) %>% as.data.table()
p <- ggplot(temp, aes(x = TradingDay, y = value, color = BrokerID)) +
    geom_line(size = 0.15, apha = 0.2) +
    geom_smooth(se = FALSE, size = 0.6, span = 0.05, alpha = 0.9) + 
    facet_grid(key ~ ., scales = "free") + 
    labs(x = 'TradingDay', y = 'Values', 
        title = paste(tempInstrumentID," :==> ", tempClassID))
ggplotly(p)
htmlwidgets::saveWidget(ggplotly(p), selfcontained = TRUE,
    paste0("/home/william/Desktop/plotly/",
           tempInstrumentID, '_', tempClassID, ".html"))
