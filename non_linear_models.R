dataset <-read.csv("Gender_StatsData.csv", stringsAsFactors=FALSE)
gdp <- subset(dataset, dataset$Indicator.Code=='NY.GDP.PCAP.CD');
fe_bachelors <- subset(dataset, dataset$Indicator.Code=='SE.TER.CUAT.BA.FE.ZS');
dfram1 <- data.frame(GDP = gdp$X2017, Fe_bachelors = fe_bachelors$X2017)
row.names(dfram1) <- dfram1$Indicator.Code;
dfram1 <- subset(dfram1, !is.na(Fe_bachelors))
dfram1 <- subset(dfram1, !is.na(GDP))
linearMod2 <- lm(dfram1$Fe_bachelors ~ log(dfram1$GDP))
summary(linearMod2)

resm2 <- resid(linearMod2)
plot(resm2)
hist(resm2, xlab="Residuals", ylab="Frequency of residuals",  border="black", col="gray33", xlim=c(-25,25), ylim=c(0,15))
hist(resm2, xlab="Residuals", ylab="Frequency of residuals", main = 'Probability density for residuals of the logarithmic model' ,  border="black", col="gray33", xlim=c(-25,25), ylim=c(0,0.08), prob = TRUE)
lines(density(resm2))
legend(-24,0.08, legend = 'Density curve for the residuals', col = 'black',lty=1:2)
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
  labs(x = "GDP per capita", y = "Percentage of adult females with at least bachelor's degree", title = "Scatterplot and logarithmic regression comparing adult females with at least bachelor's 
         degree within a country against the GDP per capita of that country")

linearMod2 <- lm(dfram1$Fe_bachelors ~ log(dfram1$GDP))
summary(linearMod2)
linearMod2 <- lm(dfram1$Fe_bachelors ~ dfram1$GDP)
summary(linearMod2)
