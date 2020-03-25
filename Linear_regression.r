library(ggplot2)

  dataset <-read.csv("Gender_StatsData.csv", stringsAsFactors=FALSE)
  gdp <- subset(dataset, dataset$Indicator.Code=='NY.GDP.PCAP.CD');
  #gdp[is.na(gdp)] <- 0
  gdp$X2017
  fe_bachelors <- subset(dataset, dataset$Indicator.Code=='SE.TER.CUAT.BA.FE.ZS');
  #fe_bachelors[is.na(fe_bachelors)] <- 0
  fe_bachelors$X2018
  linearMod <- lm(fe_bachelors$X2017 ~ gdp$X2017)
  summary(linearMod)
  ggplot(fe_bachelors$X2017, gdp$X2017)
  plot(predicted_df$fe_bachelors,predicted_df$GDP)
  
  dfram1 <- data.frame(GDP = gdp$X2017, Fe_bachelors = fe_bachelors$X2017)
  row.names(dfram1) <- dfram1$Indicator.Code;
  dfram1 <- subset(dfram1, !is.na(Fe_bachelors))
  dfram1 <- subset(dfram1, !is.na(GDP))
  #let us check what is the greatest GDP adn why they do not show in the following stuff
  data2017 <- subset(dataset, dataset$Indicator.Code=='NY.GDP.PCAP.CD')
  data2017$Country.Code[order(-data2017$X2017)]
  data2017$X2017[order(-data2017$X2017)]
  
  datafe <- subset(dataset, dataset$Indicator.Code== 'SE.TER.CUAT.BA.FE.ZS' )
  which(datafe$Country.Code =='LUX')
  datafe$X2017[163]
  #finished the checking, create the model
  
  linearMod2 <- lm(dfram1$Fe_bachelors ~ dfram1$GDP)
  summary(linearMod2)
  
  
  #make the histogram with the residues
  resm2 <- resid(linearMod2)
  plot(resm2)
  hist(resm2, xlab="Residuals", ylab="Frequency of residuals",  border="black", col="gray33", xlim=c(-25,25), ylim=c(0,15))
  hist(resm2, xlab="Residuals", ylab="Frequency of residuals", main = 'Probability density for residuals of the linear model' ,  border="black", col="gray33", xlim=c(-25,25), ylim=c(0,0.07), prob = TRUE)
  lines(density(resm2))
  legend(-24,0.07, legend = 'Density curve for the residuals', col = 'black',lty=1:2)
  grid(col = 'firebrick4')
  
  predicted_df <- data.frame(fe_bachelors = predict(linearMod2, dfram1), GDP=dfram1$GDP)
  plot(predicted_df$GDP, predicted_df$fe_bachelors )
  plot(dfram1$GDP,dfram1$Fe_bachelors)
  
  
  plot(dfram1$GDP,dfram1$Fe_bachelors)
  
  
  # this is the predicted line of multiple linear regression
  ggplot(data = dfram1, aes(x = GDP, y = Fe_bachelors)) + 
    geom_point(color = 'gray33') +
    geom_line(color='firebrick4',data = predicted_df, aes(x=GDP, y=fe_bachelors))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
    labs(x = "GDP per capita", y = "Percentage of adult females with at least bachelor's degree", title = "Scatterplot and linear regression comparing adult females with at least bachelor's 
         degree within a country against the GDP per capita of that country")
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          #panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
#Homework 6: Create models for the males
  
  ma_bachelors <- subset(dataset, dataset$Indicator.Code=='SE.TER.CUAT.BA.MA.ZS');
  dfram2 <- data.frame(GDP = gdp$X2017, Ma_bachelors = ma_bachelors$X2017) #tells over the male population
  row.names(dfram2) <- dfram2$Indicator.Code;
  dfram2 <- subset(dfram2, !is.na(Ma_bachelors))
  dfram2 <- subset(dfram2, !is.na(GDP))
  
  linearMod3 <- lm(dfram2$Ma_bachelors ~ dfram2$GDP)
  summary(linearMod3)
  
  resm3 <- resid(linearMod3)
  plot(resm3)
  hist(resm3, xlab="Residuals", ylab="Frequency of residuals", main = "Probability density for residuals, male bachelor's attainment/GDP" ,  border="black", col="gray33", xlim=c(-25,25), ylim=c(0,0.07), prob = TRUE)
  lines(density(resm3))
  legend(-24,0.07, legend = 'Density curve for the residuals', col = 'black',lty=1:2)
  grid(col = 'firebrick4')
  predicted_df2 <- data.frame(Ma_bachelors = predict(linearMod3, dfram2), GDP=dfram2$GDP)
  ggplot(data = dfram2, aes(x = GDP, y = Ma_bachelors)) + 
    geom_point(color = 'gray33') +
    geom_line(color='firebrick4',data = predicted_df2, aes(x=GDP, y=Ma_bachelors))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
    labs(x = "GDP per capita", y = "Percentage of adult males with at least bachelor's degree", title = "Scatterplot and linear regression comparing adult males with at least bachelor's 
         degree within a country against the GDP per capita of that country")
  
  summary(linearMod3)
  summary(linearMod2)
  
  #make a multiple linear regression model, this one turned to be a very bad model
  
  #fe_own_house <- subset(dataset, dataset$Indicator.Code=='SG.OWN.HSAL.FE.ZS'); #too few information
  w_industry <- subset(dataset, dataset$Indicator.Code=='SL.IND.EMPL.FE.ZS'); #GDP per capita
  gdp <- subset(dataset, dataset$Indicator.Code=='NY.GDP.PCAP.CD'); #GDP per capita
  fe_bachelors <- subset(dataset, dataset$Indicator.Code=='SE.TER.CUAT.BA.FE.ZS'); #women that have at least bachelors degree
  
  
  
  dfram_house <- data.frame(GDP = gdp$X2017, w_industry = w_industry$X2017, fe_bachelors = fe_bachelors$X2017) #tells over the male population
  row.names(dfram_house) <- dfram_house$Indicator.Code;
  #dfram_house <- subset(dfram_house, !is.na(fe_own_house))
  dfram_house <- subset(dfram_house, !is.na(w_industry))
  dfram_house <- subset(dfram_house, !is.na(fe_bachelors))
  dfram_house <- subset(dfram_house, !is.na(GDP))
  head(dfram_house)
  
  linearMod_couse_bachelors <- lm(dfram_house$fe_bachelors ~ dfram_house$GDP + dfram_house$w_industry)
  summary(linearMod_couse_bachelors)
  
  
  #make a model for the STEM graduates
  
  stem_grad <- subset(dataset, dataset$Indicator.Code=='SE.TER.GRAD.FE.SI.ZS'); #GDP per capita
  gdp <- subset(dataset, dataset$Indicator.Code=='NY.GDP.PCAP.CD'); #GDP per capita
  fe_bachelors <- subset(dataset, dataset$Indicator.Code=='SE.TER.CUAT.BA.FE.ZS'); #women that have at least bachelors degree
  
  
  
  dfram_stem <- data.frame(GDP = gdp$X2017, stem_grad = stem_grad$X2017, fe_bachelors = fe_bachelors$X2017) #tells over the male population
  row.names(dfram_stem) <- dfram_stem$Indicator.Code;
  #dfram_house <- subset(dfram_house, !is.na(fe_own_house))
  dfram_stem <- subset(dfram_stem, !is.na(stem_grad))
  #dfram_stem <- subset(dfram_stem, !is.na(fe_bachelors))
  dfram_stem <- subset(dfram_stem, !is.na(GDP))
  head(dfram_stem)
  
  linearMod_couse_stem <- lm(dfram_stem$fe_bachelors ~ dfram_stem$GDP + dfram_stem$stem_grad)
  summary(linearMod_couse_stem)
  
  linearMod_stem <- lm(dfram_stem$stem_grad ~ dfram_stem$GDP)
  summary(linearMod_stem)
  
  
  resm_stem  <- resid(linearMod_stem)
  plot(resm_stem)
  hist(resm_stem, xlab="Residuals", ylab="Frequency of residuals", main = 'Probability density for residuals of the second linear model' ,  border="black", col="gray33", xlim=c(-25,25), ylim=c(0,0.13), prob = TRUE)
  lines(density(resm_stem))
  legend(-24,0.13, legend = 'Density curve for the residuals', col = 'black',lty=1:2)
  grid(col = 'firebrick4')
  
  predicted_df_stem <- data.frame(stem_grad = predict(linearMod_stem, dfram_stem), GDP=dfram_stem$GDP)
  
  ggplot(data = dfram_stem, aes(x = GDP, y = stem_grad)) + 
    geom_point(color = 'gray33') +
    geom_line(color='firebrick4',data = predicted_df_stem, aes(x=GDP, y=stem_grad))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))+
    theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
    labs(x = "GDP per capita", y = "Percentage of adult females with at least bachelor's degree", title = "Scatterplot and linear regression comparing share of female graduates from STEM 
         programs against the GDP per capita of that country")
  