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
tempInstrumentID <- c('IF')
dt <- dtOiRank[ProductID %in% tempInstrumentID][ClassID == 'Turnover']

dt <- dt[, .(total = sum(Amount))
   , by = c('TradingDay', 'BrokerID', 'ProductID')]

## -----------------------------------------------------------------------------
## 选择进入排名前 10 的期货公司
## 
dt[, RankNew := rank(-total, ties.method = 'max')
   , by = c('TradingDay','ProductID')]
# dt[TradingDay == '2011-01-04' & ProductID == 'IF'][order(-total)]

## 做判断，如果进入前 10, 得分 1
dt[, RankScore := ifelse(RankNew <= 10, 1, 0)]

topCompany <- dt[, .(totalRankScore = sum(RankScore))
   , by = c('ProductID','BrokerID')] %>% 
   .[totalRankScore > 500, unique(BrokerID)] %>% 
   c('东证期货','华鑫期货')

dt <- dt[BrokerID %in% topCompany]

## -----------------------------------------------------------------------------
##
dt[, TradingDay := as.Date(TradingDay)]
## =============================================================================


## =============================================================================
## 画图看看
## =============================================================================
p <- ggplot(dt, aes(x = TradingDay, y = total, color = BrokerID)) +
    geom_line(size = 0.3) +
    labs(x = 'TradingDay', y = 'total', title = 'IF: longPos + shortPos')
ggplotly(p)
# ggplot(dt[ClassID == 'Turnover'][BrokerID %in% futuresComp[,newNames]], 
#     aes(x = TradingDay, y = total, color = BrokerID)) +
#     geom_point(alpha = 0.5, size = 0.9) +
#     facet_grid(.~ProductID) +
#     labs(x = 'TradingDay', y = 'Turnover', title = 'oiRank')
     
# ggplot(dt[ClassID == 'longPos'][BrokerID %in% futuresComp[,newNames]], 
#     aes(x = TradingDay, y = total, color = BrokerID)) +
#     geom_point(alpha = 0.5, size = 0.9) +
#     facet_grid(.~ProductID) +
#     labs(x = 'longPos', y = 'Turnover', title = 'oiRank')

plotFun <- function(dt, productID, classID) {
    temp <- dt[ProductID == productID & ClassID == classID]
}




