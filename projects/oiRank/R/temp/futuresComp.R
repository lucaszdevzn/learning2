#-------------------------------------------------------------------------------
# 期货公司改名
#-------------------------------------------------------------------------------
oldNames <- c('浙江永安', '万达期货', '上海五矿', '经易期货', '成都倍特'
			, '江苏弘业', '安信期货', '珠江期货', '中证期货')

newNames <- c('永安期货', '华信万达', '五矿经易', '五矿经易', '倍特期货'
			, '弘业期货', '国投安信', '中国国际'
			, '中信期货')

futuresComp <- data.table(oldNames, newNames)
fwrite(futuresComp,paste0('../data/','futuresComp.csv'))