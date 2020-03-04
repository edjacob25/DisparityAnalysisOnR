#Mean and standrd deviation
dataset <-read.csv("Gender_StatsData.csv", stringsAsFactors=FALSE)
#Check for illiterete
#illiteracy <- subset(dataset, dataset$Indicator.Code=='UIS.LPP.AG15T24');
#order(illiteracy$X2016)
#sort_literacy<-illiteracy[order(illiteracy$X2016),]
#sort_literacy$Country.Code
#sort_literacy$X2016

#Management per countries
fe_mgmt <- subset(dataset, dataset$Indicator.Code=='SL.EMP.SMGT.FE.ZS');
fe_mgmt[is.na(fe_mgmt)] <- 0
ord_fe_mgmt<-fe_mgmt[order(-fe_mgmt$X2018),]
which(ord_fe_mgmt$Country.Code=='MEX')
ord_fe_mgmt$Country.Code
ord_fe_mgmt$X2018[1:17]

barplot(ord_fe_mgmt$X2018[1:17],
        names.arg = ord_fe_mgmt$Country.Code[1:17],
        main = 'Percentage of managers that are woman. Top 17 countries in 2018.'
        )

#Check for females in stem

fe_stem <- subset(dataset, dataset$Indicator.Code=='SE.TER.GRAD.FE.SI.ZS');
fe_stem[is.na(fe_stem)] <- 0
#ord_fe_stem<-fe_stem[order(-fe_stem$X2017),]
ord_fe_stem[is.na(ord_fe_stem)] <- 0
ord_fe_stem <- ord_fe_stem[order(-ord_fe_stem),]
which(ord_fe_stem$Country.Code=='MEX')
#barplot(c(ord_fe_stem$X2017[1:10],ord_fe_stem$X2017[27]),
#        names.arg = c(ord_fe_stem$Country.Code[1:10],ord_fe_stem$Country.Code[27]),
#        main="Percentage of females graduated in STEM by top countries country")
barplot(ord_fe_stem$X2017[1:27],
        names.arg = ord_fe_stem$Country.Code[1:27],
        main="Percentage of females graduated in STEM. Top 27 countries in 2017")
head(ord_fe_stem$Country.Code)

timeframe = 2010:2017;
begin = 'X2010';
ending = 'X2017';
plot(timeframe,c(fe_stem[which(fe_stem$Country.Code=='MEX'),][which(colnames(fe_stem)==begin) : which(colnames(fe_stem)==ending)]),
     ylab = '',xlab = '', xaxt='n', yaxt='n', type='b',col ='black', pch=19)
par(new=T)
plot(timeframe,c(fe_stem[which(fe_stem$Country.Code=='TUN'),][which(colnames(fe_stem)==begin) : which(colnames(fe_stem)==ending)]),
     ylab = '',xlab = '', xaxt='n', yaxt='n', pch=18, col="blue", type="b", lty=2)
par(new=T)
plot(timeframe,c(fe_stem[which(fe_stem$Country.Code=='MRT'),][which(colnames(fe_stem)==begin) : which(colnames(fe_stem)==ending)]),
     ylab = '',xlab = '', xaxt='n', yaxt='n',type='b',col ='red', pch=19)
par(new=T)
plot(timeframe,c(fe_stem[which(fe_stem$Country.Code=='IND'),][which(colnames(fe_stem)==begin) : which(colnames(fe_stem)==ending)]),
     ylab = '',xlab = '', xaxt='n', yaxt='n',type='b',col ='green', pch=19)
par(new=T)
plot(timeframe,c(fe_stem[which(fe_stem$Country.Code=='OMN'),][which(colnames(fe_stem)==begin) : which(colnames(fe_stem)==ending)]),
     ylab = '',xlab = 'Years',type='b',col ='yellow', pch=19,main="Percentage of females graduated in stem by year. 
     Top performing countries in 2017 and Mexico.")

legend("topleft", legend=c("Mexico", "Tunisia", 'Mauritania', 'India', 'Oman'),
       col=c("black", "blue",'red','green', 'yellow'), lty=1:2, cex=0.8)




mxset <- subset(dataset, dataset$Country.Name=='Mexico');
nwset <- subset(dataset, dataset$Country.Code=='NOR');
swset <- subset(dataset, dataset$Country.Code=='SWE');
dkset <- subset(dataset, dataset$Country.Code=='DNK');
#unique(dataset$Country.Name=='Mexico')
#mxset <- dataset[dataset[1]=='Mexico']
#mxset <- setNames(mxset, colnames(dataset))
##write_indicators <- data.frame(row.names = mxset$Indicator.Code);
##write_indicators$Indicator.Name <- mxset$Indicator.Name;
##write.table(write_indicators, file = "indicators.txt", sep = "\t",row.names = TRUE, col.names = NA)
firstcol = which(colnames(mxset)=="X2013");
lastcol = which(colnames(mxset)=="X2013");

a_mxset <- mxset[c(firstcol:lastcol)];
a_nwset <- nwset[c(firstcol:lastcol)]
a_swset <- swset[c(firstcol:lastcol)]
a_dkset <- dkset[c(firstcol:lastcol)]


row.names(a_mxset) <- mxset$Indicator.Code;
row.names(a_nwset) <- nwset$Indicator.Code;
row.names(a_swset) <- swset$Indicator.Code;
row.names(a_dkset) <- dkset$Indicator.Code;
#head(a_mxset);
t_a_mxset <- as.data.frame(t(as.matrix(a_mxset)));
t_a_nwset <- as.data.frame(t(as.matrix(a_nwset)));
t_a_swset <- as.data.frame(t(as.matrix(a_swset)));
t_a_dkset <- as.data.frame(t(as.matrix(a_dkset)));
#length(t_a_mxset$SE.TER.CUAT.BA.FE.ZS)

t_a_dkset$SL.EMP.SMGT.FE.ZS #Female share of employment in senior and middle mgmt
t_a_mxset$SE.TER.GRAD.FE.SI.ZS  #Female share of graduates from STEM.

t_a_mxset$SE.TER.CUAT.BA.FE.ZS#Studied Bachelor   % 25+
t_a_mxset$SE.SEC.CUAT.LO.FE.ZS#Studied Secondary
t_a_mxset$SE.PRM.CUAT.FE.ZS   #Completed Primary
t_a_mxset$SE.TER.CUAT.MS.FE.ZS#Completed Masters
t_a_mxset$SE.TER.CUAT.DO.FE.ZS#Completed Ph D

t_a_mxset$SL.UEM.ADVN.FE.ZS #Unemployment with advanced education   %productive population
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



#mean of the employment in industry
mean(t_a_mxset$SL.IND.EMPL.FE.ZS)
#Standard Deviation for the employment in industry
sd(t_a_mxset$SL.IND.EMPL.FE.ZS)

#mean SD for the unemployment with advanced education
mean(t_a_mxset$SL.UEM.ADVN.FE.ZS)
sd(t_a_mxset$SL.UEM.ADVN.FE.ZS)

#mean SD for the unemployment with basic education
mean(t_a_mxset$SL.UEM.BASC.FE.ZS)
sd(t_a_mxset$SL.UEM.BASC.FE.ZS)

#mean SD for the unemployment with intermediate education
mean(t_a_mxset$SL.UEM.INTM.FE.ZS)
sd(t_a_mxset$SL.UEM.INTM.FE.ZS)

#ANOVA, employment in senior and mid mgmt as a function of woman that have studied

res.aov <- aov(SL.EMP.SMGT.FE.ZS ~  SE.TER.CUAT.BA.FE.ZS + SE.TER.CUAT.MS.FE.ZS + SE.TER.CUAT.DO.FE.ZS, data = t_a_mxset)
summary(res.aov)      

#ANOVA, the M Sc and the Ph Ds as a function of the graduated from bachelors

res.aov2 <- aov(SE.TER.CUAT.BA.FE.ZS ~ SE.TER.CUAT.MS.FE.ZS + SE.TER.CUAT.DO.FE.ZS, data = t_a_mxset)
summary(res.aov2)

#ANOVA, seniod and mig mgmt as function of graduated from stem 

res.aov3 <- aov(SL.EMP.SMGT.FE.ZS ~ SE.TER.GRAD.FE.SI.ZS, data = t_a_mxset)
summary(res.aov3)

#Let us do the plots:

boxplot(t_a_mxset$SL.EMP.SMGT.FE.ZS, t_a_mxset$SE.TER.CUAT.BA.FE.ZS, t_a_mxset$SE.TER.CUAT.MS.FE.ZS, t_a_mxset$SE.TER.CUAT.DO.FE.ZS, 
        main= 'Comparing female share of employment in management positions with the percentage 
        of women that are over 25 years and have a degree',
        at = c(1,3,4,5),
        names = c('% Females in MGMT', 'Bachelors degree', 'Masters degree', 'Ph D degree'))

boxplot(t_a_mxset$SE.TER.GRAD.FE.SI.ZS,t_a_mxset$SE.TER.CUAT.BA.FE.ZS, t_a_mxset$SE.TER.CUAT.MS.FE.ZS, t_a_mxset$SE.TER.CUAT.DO.FE.ZS, 
        main= 'Share of females graduated from STEM comparted with the percentage 
        of women that are over 25 years and have a degree',
        at = c(1,3,4,5),
        names = c('STEM graduates','Bachelors degree', 'Masters degree', 'Ph D degree' ))

boxplot(t_a_mxset$SL.EMP.SMGT.FE.ZS,t_a_mxset$SE.TER.GRAD.FE.SI.ZS, 
        main= 'Share of females in management compared to females with STEM background',
        at = c(1,3),
        names = c('Females in MGMT','Females STEM graduates'))

boxplot(t_a_mxset$SL.EMP.SMGT.FE.ZS, t_a_dkset$SL.EMP.SMGT.FE.ZS, t_a_nwset$SL.EMP.SMGT.FE.ZS, t_a_swset$SL.EMP.SMGT.FE.ZS, 
        main = 'Share of females with management positions by country',
        names = c('Mexico', 'Denmark', 'Norway', 'Sweeden')
        )

boxplot(t_a_mxset$SE.TER.GRAD.FE.SI.ZS, t_a_dkset$SE.TER.GRAD.FE.SI.ZS, t_a_nwset$SE.TER.GRAD.FE.SI.ZS, t_a_swset$SE.TER.GRAD.FE.SI.ZS, 
        main = 'Percentage of 25+ year old females with STEM degrees',
        names = c('Mexico', 'Denmark', 'Norway', 'Sweeden')
        )
t_a_dkset$SL.EMP.SMGT.FE.ZS
t_a_dkset$SE.TER.GRAD.FE.SI.ZS


plot()


#colnames(t_a_mxset)
rownames(t_a_mxset)
rm(list=ls())
