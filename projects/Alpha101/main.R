# ==================================================================================================
# source('/home/fl/myCodes/Alpha101/main.R')
# ==================================================================================================

rm(list = ls())
# ------------------------------------------------------------------------------
setwd("/home/fl/myCodes/Alpha101/")

source("./conf/myInit.R")
source("./R/fetch_data_from_mysql.R")
# ------------------------------------------------------------------------------

options(width = 160)
# ==========: 提取dtY ===============================================================================
# ------------------------------------------------------------------------------
# dtY
# 五日收益率预测
# ------------------------------------------------------------------------------ 
dtY <- inSample[,.(TradingDay
                    ,close
                    #,preClose = log(shift(close,1L,type = 'lag'))
                    ,closeRtn   = log(shift(close,5L,type = 'lead')) - log(close)
                    #,nextClose = log(shift(close,1L,type = 'lead'))
                    ,predictRtn = log(shift(close,5L,type = 'lead')) - log(shift(open,1L,type = 'lead'))
                    )
                , by = 'InstrumentID'] %>%
        .[!is.na(closeRtn) & !is.na(predictRtn)]
dtY[,.(min(TradingDay),max(TradingDay))]
# ==================================================================================================


for(i in list.files('./R/', pattern = '*[aA]lpha')){
    print(i)
    paste0("/home/fl/myCodes/Alpha101/R/",i) %>%
        source(.)

    source("./R/Cal_dtXY.R")
    source("./R/Cal_NAV.R")
}

# ==================================================================================================
# ==================================================================================================

for(i in list.files('./R/', pattern = 'myAlpha.*')){
    print(i)
    paste0("/home/fl/myCodes/Alpha101/R/",i) %>%
        source(.)

    source("./R/Cal_dtXY.R")
    source("./R/Cal_NAV.R")
}
