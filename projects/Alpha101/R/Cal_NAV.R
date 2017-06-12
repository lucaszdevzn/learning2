Cal_NAV <- function(x){
    temp <- x

    tempDailyRtn <- temp[, pReturn := sign(predictor) *
                                      sign(fit$coefficients["predictor"]) 
                                      * pWeight 
                                      * predictRtn] %>%
                    .[,.(dailyRtn = sum(pReturn)), by = 'TradingDay'] %>%
                    .[,":="(#simpleCumRtn = cumsum(dailyRtn),
                            #nav          = cumprod(1+dailyRtn)
                             nav          = cumsum(dailyRtn)
                          )]
    return(tempDailyRtn)
}

dtNAV <- Cal_NAV(dtXY)
# ==================================================================================================
# 计算最大回撤
dtNAV[, cumMaxNAV := cummax(nav)] %>% 
  .[, drawdown := nav - cumMaxNAV]
maxDD <- dtNAV[which.min(drawdown)][.N]
# dtNAV[TradingDay <= maxDD[,TradingDay]][drawdown == 0][.N]
tempDD <- dtNAV[TradingDay %between% c(dtNAV[TradingDay <= maxDD[,TradingDay]][drawdown == 0][.N,TradingDay],
                                       maxDD[.N,TradingDay])
                ]
# ==========: 计算 NAV ==============================================================================

sharpRatio <- dtNAV[,mean(dailyRtn) / sd(dailyRtn) * sqrt(252)]
print(sharpRatio)

tempMaxMin <- rbindlist(list(dtNAV[which.max(nav)],dtNAV[which.min(nav)])) %>%
              .[, nav := round(nav,2)]

p <- ggplot() +
      geom_area(data = tempDD, aes(x = TradingDay, y = nav), fill = 'green', color = NA, alpha = 0.2) +
      geom_hline(yintercept = 0, color = 'gray80',size = 0.5) + 
      geom_line(data = dtNAV,aes(TradingDay,nav), color = 'steelblue') +
      geom_point(data = tempMaxMin ,aes(TradingDay,nav), color = c('hotpink','darkgreen')) +
      geom_point(data = dtNAV[.N] ,aes(TradingDay,nav), color = 'darkorange', size = 3) +
      geom_text(data = tempMaxMin ,aes(TradingDay,nav,label = nav),color = c('hotpink','darkgreen')
                             ,hjust = 2, vjust = 2) +
      annotate("text", x = tempDD[round(.N / 2),TradingDay], y = 0
              , label = paste("maxDD:\n",maxDD[,round(drawdown*100,2)],"%")
              , color = 'steelblue', size = 2.8) + 
      labs(title = paste0(alphaName,': ','(sharp ratio:', round(sharpRatio,2),')'
           ,"\n  //  (R-squre):",round(summary(fit)$r.squared,4)
           ," //  (t value):",round(summary(fit)$coefficients %>% as.data.table() %>% .[1,'t value'], 2)))
#ggplotly(p)

setwd('./output/')
## 
Sys.setenv(R_ZIPCMD= "/usr/bin/zip")  
openxlsx::write.xlsx(list('dtXY' = dtXY, 'dtNAV' = dtNAV)
                ,file = paste0(alphaName,'_nav.xlsx'), row.names = FALSE)

htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(alphaName,"_nav.html")
    , selfcontained = F)
setwd("..")
################################################################################
