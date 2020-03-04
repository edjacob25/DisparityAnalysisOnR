dataset <-read.csv("Gender_StatsData.csv", stringsAsFactors=FALSE)

#write_countries <- data.frame(row.names = unique(dataset$Country.Code));
#write_countries$Indicator.Name <- unique(dataset$Country.Name);
#write.table(write_countries, file = "country_names.txt", sep = "\t",row.names = TRUE, col.names = NA)
mxset <- subset(dataset, dataset$Country.Code=='SGP');

#unique(dataset$Country.Name=='Mexico')
#mxset <- dataset[dataset[1]=='Mexico']
#mxset <- setNames(mxset, colnames(dataset))
firstcol = which(colnames(mxset)=="X2012");
lastcol = which(colnames(mxset)=="X2018");
a_mxset<-mxset[c(firstcol:lastcol)];
row.names(a_mxset) <- mxset$Indicator.Code;
#head(a_mxset);
t_a_mxset <- as.data.frame(t(as.matrix(a_mxset)));
#length(t_a_mxset$SE.TER.CUAT.BA.FE.ZS)
t_a_mxset$SE.TER.CUAT.BA.FE.ZS#Studied Bachelor
t_a_mxset$SE.SEC.CUAT.LO.FE.ZS#Studied Secondary
t_a_mxset$SE.PRM.CUAT.FE.ZS   #Completed Primary
t_a_mxset$SE.TER.CUAT.MS.FE.ZS#Completed Masters
t_a_mxset$SE.TER.CUAT.DO.FE.ZS#Completed Ph D

t_a_mxset$SL.UEM.ADVN.FE.ZS #Unemployment with advanced education
t_a_mxset$SL.UEM.BASC.FE.ZS #Unemployment with basic education
t_a_mxset$SL.UEM.INTM.FE.ZS #Unemployment with intermediate education
t_a_mxset$SL.UEM.TOTL.FE.ZS #Unemployment total

t_a_mxset$NY.GDP.MKTP.CD #GDP
t_a_mxset$NY.GDP.MKTP.KD.ZG #Annual growth
t_a_mxset$NY.GDP.PCAP.CD  #GDP per capita

t_a_mxset$IC.WEF.LLCO.FE  #Business Owners
t_a_mxset$IC.WEF.LLCD.FE  #Female directors
t_a_mxset$IC.WEF.SOLO.FE #Female sole propietors

t_a_mxset$SL.AGR.EMPL.FE.ZS #Employment in agriculture as percentage ILO
t_a_mxset$SL.IND.EMPL.FE.ZS #Employment in industry ILO
t_a_mxset$SL.SRV.EMPL.FE.ZS #Employment in services, ILO
t_a_mxset$SL.EMP.TOTL.SP.FE.ZS #Employment to population ratio, 15+, female modeled ILO

t_a_mxset$SL.EMP.SMGT.FE.ZS #Female share of employment in senior and middle mgmt
t_a_mxset$SE.TER.GRAD.FE.SI.ZS  #Female share of graduates from STEM.





#colnames(t_a_mxset)
rownames(t_a_mxset)
rm(list=ls())
