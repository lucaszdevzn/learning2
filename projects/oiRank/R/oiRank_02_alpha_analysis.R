################################################################################
## oiRank_02_alpha_analysis.R
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

dt <- dtOiRank[ProductID == tempProductID][TradingDay >= '2015-10-01']

# y <- dt[, .(ratio = .SD[ClassID == 'Turnover', sum(Amount)] / 
#                .SD[ClassID %in% c('shortPos'), sum(Amount)])
#    , by= c('TradingDay','ProductID','BrokerID')]
# u <- y[!is.infinite(ratio)][!is.na(ratio)]

# p <- ggplot(u[BrokerID %in% c("银河期货", "永安期货", "国泰君安",'海通期货')], aes(x =TradingDay, y =ratio)) +
#     geom_line(color = 'steelblue') +
#     facet_grid(BrokerID ~ .)
# ggplotly(p)

# temp <- u[ratio %between% c(quantile(ratio,0.01), quantile(ratio, 0.99))][BrokerID %in% c("银河期货", "永安期货", "国泰君安",'海通期货','中信期货')]
# temp2 <- dtMain[ProductID == 'cu'][TradingDay %in% temp$TradingDay]
# p <- ggplot() +
#     geom_jitter(data =temp, aes(x =TradingDay, y =ratio, color =BrokerID)) +
#     geom_line(data =temp2, aes(x =TradingDay, y =priceIndex)) +
#     scale_x_continuous() +
#     scale_y_continuous(b"y_first", sec.axis=sec_axis(~.*0.025+2.75, name="y_second") ) 
# ggplotly(p)


## =============================================================================
## 我有一个想法
## 把 longPos 和 shortPos 分开处理
## 分别代表了 多方 和 空方
## =============================================================================
# dtVolume <- dt[ClassID == 'Turnover'] %>% cal_rank(.,ClassID = 'volume')
# dtLong   <- dt[ClassID == 'longPos'] %>% cal_rank(.,ClassID = 'long')
# dtShort  <- dt[ClassID == 'shortPos'] %>% cal_rank(.,ClassID = 'short')

## -----------------------------------------------------------------------------
top10Volume <- dt[ClassID == 'Turnover'] %>% 
          cal_rank(, ClassID = 'volume') %>% 
          get_top_company()
top10Long <- dt[ClassID == 'longPos'] %>% 
          cal_rank(, ClassID = 'long') %>% 
          get_top_company()
top10Short <- dt[ClassID == 'shortPos'] %>% 
          cal_rank(, ClassID = 'short') %>% 
          get_top_company()
top10Position <- dt[ClassID != 'Turnover'] %>% 
          cal_rank(, ClassID = 'position') %>% 
          get_top_company()


dtVolume   <- dt[ClassID == 'Turnover'][BrokerID %in% top10Volume] %>% cal_rank(, ClassID = 'volume')
dtLong     <- dt[ClassID == 'longPos'][BrokerID %in% top10Long] %>% cal_rank(, ClassID = 'long')
dtShort    <- dt[ClassID == 'shortPos'][BrokerID %in% top10Short] %>% cal_rank(, ClassID = 'short')
dtPosition <- dt[ClassID != 'Turnover'][BrokerID %in% top10Position] %>% cal_rank(, ClassID = 'position')

temp <- list(dtVolume, dtLong, dtShort, dtPosition) %>% rbindlist() 
tempRes <- gather(temp[,.(TradingDay,BrokerID,total,totalPct,ClassID)],
               key, value, -c(TradingDay, BrokerID, ClassID)) %>% 
            as.data.table()
p <- ggplot(tempRes, aes(x = TradingDay, y = value, color = BrokerID)) +
        geom_line(alpha = 0.3, size = 0.3) +
        geom_line(data = dtMain[TradingDay %in% dt[,unique(TradingDay)] & 
                                ProductID == tempProductID], 
                 aes(x = TradingDay, y = priceIndex * 0.5), color = 'steelblue', size = 0.7) + 
        geom_smooth(se = FALSE, size = 0.5, span = 0.15, alpha = 0.90, na.rm = T) + 
        facet_grid(key ~ ClassID, scales = "free") +
        labs(x = 'TradingDay', y = 'Values',
            title = paste0(tempProductID, ' ：==> volume vs longPos vs shortPos'))
ggplotly(p)
        
htmlwidgets::saveWidget(ggplotly(p), selfcontained = TRUE,
    paste0("/home/william/Desktop/plotly/",
           tempProductID, '_long_short', ".html"))
