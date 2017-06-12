library(RMySQL)
library(ggplot2)
library(plotly)

MySQL(max.con = 300)
for( conns in dbListConnections(MySQL()) ){
  dbDisconnect(conns)
}

mysql_user <- 'fl'
mysql_pwd  <- 'abc@123'
mysql_host <- "192.168.1.106"
mysql_port <- 3306

#---------------------------------------------------
# mysqlFetch
# 函数，主要输入为
# database
#---------------------------------------------------
mysqlFetch <- function(x){
  temp <- dbConnect(MySQL(),
                    dbname   = as.character(x),
                    user     = mysql_user,
                    password = mysql_pwd,
                    host     = mysql_host,
                    port     = mysql_port
  )
}

mysql <- mysqlFetch('china_futures_bar')
dt <- dbGetQuery(mysql, "
                 select * from main_contract_daily") %>%
  as.data.table() %>%
  .[,TradingDay := as.Date(TradingDay)] %>%
  .[!format(TradingDay, '%Y') %in% c('2013','2016','2017')] %>%
  .[, .(index = mean(Prod_index))
    , by = 'TradingDay']

p <- ggplot(data = dt, aes(x = TradingDay, y = index)) +
  geom_hline(yintercept = 1, color = 'gray50') +
  geom_line(color = 'steelblue')

ggplotly(p)
