# ==============================================================================
# Ref: "Alpha 101"
# # Alpha#33:         

# #
# 算法
# 1.
#
# 含义
# 

# 关联：
# 
# ==============================================================================

# ------------------------------------------------------------------------------
## Alpha Name
alphaName <- 'alpha101_33'

## 需要去除的日期
truncatedTD <- 0
# ------------------------------------------------------------------------------

# ==========: 提取dtX ===============================================================================
# ------------------------------------------------------------------------------
# dtX
# ------------------------------------------------------------------------------ 

# ------------------------------------------------------------------------------
# 计算 signal

# ------------------------------------------------------------------------------
dtX <- inSample[, .SD[.N > truncatedTD]
                , by = 'InstrumentID'] %>%
        .[, ":="(signal1 = 1 - open/close)
          , by = 'InstrumentID'] %>%
        .[, mySignal := base::rank(-signal1, ties.method = 'min') %>% as.numeric(),
          , by = 'TradingDay'] %>% 
        .[, mySignal := (mySignal - mean(mySignal)) / sd(mySignal) * sqrt(nrow(.SD))
          , by = 'TradingDay']
# ==================================================================================================
