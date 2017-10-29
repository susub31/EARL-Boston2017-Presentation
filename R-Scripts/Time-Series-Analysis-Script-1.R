library(dplyr)
library(forecast)
library(tseries)

# hdata2016 <- read.csv("../Datasets/PIT_CountsByState_2016.csv", stringsAsFactors = FALSE)

# Using the code below multiple datasets are created in a dynamic fashion
years = c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
for (i in years) {
  inputfilename = paste0("../Datasets/PIT_CountsByState_", i, ".csv")
  assign(paste0("hdata", i), read.csv(paste0("../Datasets/PIT_CountsByState_", i, ".csv"), stringsAsFactors = FALSE))
}

hdata2007$Year <- 2007
hdata2008$Year <- 2008
hdata2009$Year <- 2009
hdata2010$Year <- 2010
hdata2011$Year <- 2011
hdata2012$Year <- 2012
hdata2013$Year <- 2013
hdata2014$Year <- 2014
hdata2015$Year <- 2015
hdata2016$Year <- 2016

combineddata <- merge(hdata2007, hdata2008, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008)

combineddata <- merge(combineddata, hdata2009, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009)

combineddata <- merge(combineddata, hdata2010, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010)

combineddata <- merge(combineddata, hdata2011, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010, TotalHomeless2011)

combineddata <- merge(combineddata, hdata2012, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010, TotalHomeless2011, TotalHomeless2012)

combineddata <- merge(combineddata, hdata2013, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010, TotalHomeless2011, TotalHomeless2012, TotalHomeless2013)

combineddata <- merge(combineddata, hdata2014, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010, TotalHomeless2011, TotalHomeless2012, TotalHomeless2013, TotalHomeless2014)

combineddata <- merge(combineddata, hdata2015, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010, TotalHomeless2011, TotalHomeless2012, TotalHomeless2013, 
                       TotalHomeless2014, TotalHomeless2015)

combineddata <- merge(combineddata, hdata2016, by.x ="State", by.y="State")
combineddata <- select(combineddata, State, TotalHomeless2007, TotalHomeless2008, TotalHomeless2009,
                       TotalHomeless2010, TotalHomeless2011, TotalHomeless2012, TotalHomeless2013, 
                       TotalHomeless2014, TotalHomeless2015, TotalHomeless2016)

head(combineddata)

AK_Data_TS <- tidyr::gather(combineddata[1,], "Year", "Counts", 2:11)
AK_Data_TS$Year <- gsub("TotalHomeless", "", AK_Data_TS$Year)
AK_Data_TS$State = NULL

AK_Data_TS

plot.ts(AK_Data_TS$Counts)
acf(diff(log(AK_Data_TS$Counts)))

fit <- arima(log(AK_Data_TS$Counts))
pred <- predict(fit, n.ahead = 1)
new_TS <- rbind(AK_Data_TS, c(2017, as.integer(2.718^pred$pred)))

# 2017 for Alaska: 1128 + 717 (https://www.alaskahousing-homeless.org/data/)

p1 <- ggplot(AK, aes(Year, Counts, group=1)) +
  geom_point() + 
  geom_line() +
  labs(x="Year", y="Counts")

p1 + stat_smooth(aes(y=Counts, x=Year), method='loess')

ggplot(AK, aes(Year, Counts, group=1)) +
  geom_point() +
  geom_line() +
  geom_smooth() 

ggplot(new_TS, aes(Year, Counts, group=1)) +
  geom_point() +
  geom_line() +
  geom_smooth() 

