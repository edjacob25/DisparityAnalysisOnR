my_data <- PlantGrowth
set.seed(1234)
dplyr::sample_n(my_data, 10)
#levels(my_data$group)
#my_data$group <- ordered(my_data$group,levels = c("ctrl", "trt1", "trt2"))
group_by(my_data, group)
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")  


rm(list=ls())
