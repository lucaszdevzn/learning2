################################################################################
## oiRank 策略
################################################################################

rm(list = ls())
################################################################################
## Step0: 初始参数设置
################################################################################
## temp <- dirname(parent.frame(1)$ofile)
## setwd(temp)
setwd("/home/william/Documents/myStrat/projects/oiRank")

suppressMessages(
  suppressWarnings({
    source('../../conf/Rconf/myInit.R')
    ChinaFuturesCalendar <- fread('../../data/ChinaFuturesCalendar_2011_2017.csv')
    source('./R/oiRank_00_myFunction.R')
  })
)

################################################################################
## Step1: 获取数据
################################################################################
source('./R/oiRank_01_fetch_data.R')

################################################################################
## Step2: 
################################################################################
# source('./R/oiRank_02_alpha_cal_product.R')

allProductID <- c('rb','ru','zn','cu',
                  'i','m','jm','j','v',
                  'SR','TA')
allClassID <- c('volume','position')

for (i in 1:length(allProductID)) {
    print(i)
    tempProductID <- allProductID[i]
    for (j in 1:length(allClassID)) {
      tempClassID <- allClassID[j]
      suppressWarnings({
        suppressMessages({
          source('./R/oiRank_02_alpha_plot.R')
        })
      })
    }
}



for (i in 1:length(allProductID)) {
    print(i)
    tempProductID <- allProductID[i]
    suppressWarnings({
      suppressMessages({
        # source('./R/oiRank_02_alpha_plot_3.R')
        source('./R/oiRank_02_alpha_analysis.R')
      })
    })
}

