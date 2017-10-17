################################################################################
## 相对稳定性
################################################################################
# mainContOI[Rank %between% c(1,10)][,unique(BrokerID)]

dt[,.(totalLongPos  = mean(longAmount)/sd(longAmount),
	  totalShortPos = mean(shortAmount)/sd(shortAmount))]

#-------------------------------------------------------------------------------
cl <- makeCluster(detectCores(), type = 'FORK')
relLongPos <- parLapply(cl,mainContOI[Rank %between% c(1,10)][ClassID == 'longPos'][,unique(BrokerID)],
						function(i){
	temp <- mainContOI[Rank %between% c(1,10)][BrokerID == i][ClassID == 'longPos']
	tempDT <- temp[,mean(Amount)/sd(Amount)] / dt[,mean(longAmount)/sd(longAmount)]
	tempRes <- data.table(BrokerID = i,
						  meanAmount = temp[,mean(Amount)],
						  relVar   = tempDT)
	return(tempRes)
}) %>% rbindlist() %>% .[!is.na(relVar)]
stopCluster(cl)
#-------------------------------------------------------------------------------
relLongPos2 <- relLongPos[relVar <= quantile(relVar,0.98)]
tempAnotate <- relLongPos[meanAmount >= quantile(meanAmount,0.75)][relVar <= quantile(relVar,0.5)][order(-meanAmount)]

p <- ggplot(relLongPos2, aes(x = relVar, y = meanAmount, label = BrokerID)) + 
	geom_point(color = 'steelblue', size = 2, alpha = 0.5) + 
	geom_vline(xintercept = relLongPos2[,quantile(relVar,0.5)], color = 'hotpink', linetype = 'dashed') +
	geom_hline(yintercept = relLongPos2[,quantile(meanAmount,0.75)], color = 'hotpink', linetype = 'dashed') +
	annotate("text", x = tempAnotate$relVar, y = tempAnotate$meanAmount,
		    label = tempAnotate$BrokerID, size = 2.5, color = 'darkorange') +
	labs(title = 'LongPos')
ggplotly(p)



#-------------------------------------------------------------------------------
cl <- makeCluster(detectCores(), type = 'FORK')
relShortPos <- parLapply(cl,mainContOI[Rank %between% c(1,10)][ClassID == 'shortPos'][,unique(BrokerID)],
						function(i){
	temp <- mainContOI[Rank %between% c(1,10)][BrokerID == i][ClassID == 'shortPos']
	tempDT <- temp[,mean(Amount)/sd(Amount)] / dt[,mean(shortAmount)/sd(shortAmount)]
	tempRes <- data.table(BrokerID = i,
						  meanAmount = temp[,mean(Amount)],
						  relVar   = tempDT)
	return(tempRes)
}) %>% rbindlist() %>% .[!is.na(relVar)]
stopCluster(cl)
#-------------------------------------------------------------------------------
relShortPos2 <- relShortPos[relVar <= quantile(relVar,0.98)]
tempAnotate <- relShortPos[meanAmount >= quantile(meanAmount,0.75)][relVar <= quantile(relVar,0.5)][order(-meanAmount)]

p <- ggplot(relShortPos2, aes(x = relVar, y = meanAmount, label = BrokerID)) + 
	geom_point(color = 'steelblue', size = 2, alpha = 0.5) + 
	geom_vline(xintercept = relShortPos2[,quantile(relVar,0.5)], color = 'hotpink', linetype = 'dashed') +
	geom_hline(yintercept = relShortPos2[,quantile(meanAmount,0.75)], color = 'hotpink', linetype = 'dashed') +
	annotate("text", x = tempAnotate$relVar, y = tempAnotate$meanAmount,
		    label = tempAnotate$BrokerID, size = 2.5, color = 'darkorange') +
	labs(title = '')
ggplotly(p)


