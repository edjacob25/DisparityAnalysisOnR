dataset <- read.csv("Gender_StatsData.csv", stringsAsFactors=FALSE)
countries <- unique(dataset[,1]) # The first column contains the name of the countries and the second is an indicator for it 
head(countries)
columns = unique(dataset[,3]) # The third column contains the indicator name
head(columns)
#head(dataset[40])
new_dat <- data.frame(row.names = countries)
for (col_name in columns) {
  col = subset(x = dataset, subset = Indicator.Name == col_name, select=X2017)
  new_dat[col_name] <- col[, "X2017"]
}
new_dat <- new_dat[!sapply(new_dat, function (x) all(is.na(x) | x == ""))]
summary(new_dat)

dim(dataset)
dim(new_dat)
