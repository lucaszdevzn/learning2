################################################################################
## oiRank_02_alpha_plot.R
## 
## 合约品种 productID
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
## 计算前 10 名的公式
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

select_company <- function(dt,topCompany) {
  tempRes <- dt[BrokerID %in% topCompany]
  
  if ('国投安信' %in% topCompany){
  gtax <- tempRes[TradingDay %between% c('2015-12-20','2016-07-01')] %>% 
          .[,.(TradingDay = unique(TradingDay), ProductID = tempProductID, BrokerID = '¹úÍ¶°²ÐÅ', 
               total = 0, totalPct = 0, RankNew = 0, RankScore = 0)]
  tempRes <- rbind(tempRes, gtax)
  }

  return(tempRes)
}

top10Vol <- dtOiRank[ProductID == tempProductID][ClassID == 'Turnover'] %>% 
          cal_rank(, ClassID = 'volume') %>% 
          get_top_company()

top10Pos <- dtOiRank[ProductID == tempProductID][ClassID != 'Turnover'] %>% 
          cal_rank(, ClassID = 'position') %>% 
          get_top_company()

myPlotly <- function(dt, title, caption = '@williamfang') {
  tempRes <- gather(dt[,.(TradingDay, BrokerID, ClassID, total, totalPct)], 
                  key, value, -c(TradingDay,BrokerID,ClassID)) %>% as.data.table()
  p <- ggplot(tempRes, aes(x = TradingDay, y = value, color = BrokerID)) +
      geom_line(size = 0.15, aplha = 0.2) +
      geom_line(data= dtMain[TradingDay %in% dt[,unique(TradingDay)] & 
                              ProductID == tempProductID], 
               aes(x = TradingDay, y = priceIndex * 0.25), color = 'steelblue', size= 1.2) + 
      geom_smooth(se = FALSE, size = 0.6, span = 0.15, alpha = 0.90, na.rm = T) + 
      facet_grid(key ~ ClassID, scales = "free") + 
      labs(x = 'TradingDay', y = 'Values', 
          title = title,
          caption = caption)
  return(ggplotly(p))
  ggplotly(p)
}

## =============================================================================
## 前 10
## =============================================================================
dtVol <- dtOiRank[ProductID == tempProductID][ClassID == 'Turnover'] %>% 
          cal_rank(, ClassID = 'volume') %>% 
          .[BrokerID %in% top10Pos]

dtPos <- dtOiRank[ProductID == tempProductID][ClassID != 'Turnover'] %>% 
          cal_rank(, ClassID = 'position') %>% 
          .[BrokerID %in% top10Vol]

myPlotly(dtVol, title = paste(tempProductID,': Volume for top10Pos'))
myPlotly(dtPos, title = paste(tempProductID,': Position for top10Vol'))

## =============================================================================
## all in one figure
## =============================================================================
# dtPos$total <- dtPos$total * 3
dtAll <- list(dtVol, dtPos) %>% rbindlist()

temp <- myPlotly(dtAll, title = paste(tempProductID,
                                ': Position for top10Vol <==> Volume for top10Pos')
)

htmlwidgets::saveWidget(temp, selfcontained = TRUE,
    paste0("/home/william/Desktop/plotly/",
           tempProductID, '_top10', ".html"))



