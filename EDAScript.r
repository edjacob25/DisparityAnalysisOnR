dataset <-read.csv("Gender_StatsData.csv", stringsAsFactors=FALSE)
dim(dataset)
dataset[1]
mxset <- subset(dataset, dataset$Country.Name=='Mexico')
#unique(dataset$Country.Name=='Mexico')
#mxset <- dataset[dataset[1]=='Mexico']
#mxset <- setNames(mxset, colnames(dataset))
write_indicators <- data.frame(row.names = mxset$Indicator.Code)
write_indicators$Indicator.Name <- mxset$Indicator.Name
write.table(write_indicators, file = "indicators.txt", sep = "\t",row.names = TRUE, col.names = NA)
firstcol = which(colnames(mxset)=="X2000")
lastcol = which(colnames(mxset)=="X2018")
a_mxset<-mxset[c(firstcol:lastcol)]
row.names(a_mxset) <- mxset$Indicator.Code
head(a_mxset)
t_a_mxset <- as.data.frame(t(as.matrix(a_mxset)))
t_a_mxset$Sl.EMP.
"SL.EMP.SMGT.FE.ZS")

rm(list=ls())
