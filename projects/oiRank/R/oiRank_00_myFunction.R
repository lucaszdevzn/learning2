## =============================================================================
## oiRank_00_myFunction.R
## 
## 存放 oiRank 项目的所有函数
## =============================================================================

## =============================================================================
## 重新计算排名
## 
## Input:
## @param dt: 数据
## @param startDate：开始日期（默认）
## @param endDate：结束日期（默认）
## @param ClassID：标签名称
## =============================================================================
cal_rank <- function(dt, startDate = '2015-10-01', 
                     endDate = dtOiRank[,as.character(max(TradingDay))],
                     ClassID) {
  tempRank <- dt[TradingDay %between% c(startDate, endDate)] %>% 
        .[, .(total = sum(Amount))
          , by = c('TradingDay','ProductID','BrokerID')]
  tempRank <- tempRank[, totalPct := total / .SD[, sum(total)]
          , by = c('TradingDay','ProductID')]
  tempRank[, RankNew := rank(-totalPct, ties.method = 'max')
       , by = c('TradingDay','ProductID')]
  tempRank[, RankScore := ifelse(RankNew <= 20, 1, 0)]
  tempRank[, ClassID := ClassID]

  return(tempRank)
}
## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


## =============================================================================
## 计算排名在前几名的期货公司
## 
## Input:
## @param dt
## @param startDate
## @param endDate
## 
## Output
## topCompany
## =============================================================================
get_top_company <- function(dt, topNo = 10, 
                            startDate = '2015-10-01', 
                            endDate = dtOiRank[,as.character(max(TradingDay))]) {
  topCompany <- dt[, .(totalRankScore = sum(RankScore))
     , by = c('ProductID','BrokerID')] %>% 
     .[order(-totalRankScore)] %>% 
     .[1:topNo,BrokerID]
  print(topCompany)

  return(topCompany)
}
## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

## =============================================================================
## 选择排名在前面几名的期货公司的数据
## 
## Input:
## @param dt
## @param topCompany
## =============================================================================
select_company <- function(dt,topCompany) {
  tempRes <- dt[BrokerID %in% topCompany]
  
  if ('国投安信' %in% topCompany){
  gtax <- tempRes[TradingDay %between% c('2015-12-20','2016-07-01')] %>% 
          .[,.(TradingDay = unique(TradingDay), ProductID = tempProductID, BrokerID = '国投安信', 
               total = 0, totalPct = 0, RankNew = 0, RankScore = 0)]
  tempRes <- rbind(tempRes, gtax)
  }

  return(tempRes)
}
## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


