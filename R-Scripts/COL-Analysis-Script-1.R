library(plotrix)
library(ggplot2)
library(reshape2)

# plot 3-D pie chart of the Cost of Living Index components
COLIndex <- data.frame(group=c("Housing", "Misc Goods & Svcs", "Grocery", "Transportation", "Utilities", "Healthcare"), FR=c(29,32,13,12, 10, 4))
pie3D(COLIndex$FR, labels = COLIndex$group, main = "Cost of Living Index - Breakdown", explode=0.07, radius=.9, labelcex = 1.1,  start=0.7, theta=pi/3)

COL_Cities <- read.csv("../Datasets/COL_Details_SelectCities.csv")

COL_Plot <- ggplot(COL_Cities)
COL_Plot <- COL_Plot + geom_bar(aes(City_County, COLIndex, group=1, fill=COLIndex), alpha=0.3, stat = "identity") 
#COL_Plot <- COL_Plot + geom_bar(aes(City_County, Housing, group=1), alpha=0.3, stat = "identity", color="red")

COL_Plot <- COL_Plot + theme(axis.text.x = element_text(angle=45, hjust=1))

COL_Plot

