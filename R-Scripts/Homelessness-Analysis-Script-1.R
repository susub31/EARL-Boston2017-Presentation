library(dplyr)
library(ggmap)
library(caret)
library(stringr)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

hdata <- read.csv("../Datasets/HomelessData2016.csv", stringsAsFactors = FALSE)
modhdata <- read.csv("../Datasets/HomelessData2016_With_LatLon.csv", stringsAsFactors = FALSE)
modhdata$Total.Homeless..2016 = NULL
modhdata$CoC.Name=NULL
alldata <- merge(hdata, modhdata, by.x ="CoC.Number", by.y="CoC.Number")
alldata <- select(alldata, CoC.Number, CoC.Name, cityname, lat, lon, Total.Homeless..2016, 
                  Homeless.Veterans..2016, Homeless.Unaccompanied.Youth..Under.25...2016) %>%
  rename(TotalHomeless = Total.Homeless..2016, HomelessVeterans = Homeless.Veterans..2016, 
         HomelessYouth = Homeless.Unaccompanied.Youth..Under.25...2016)

alldata$TotalHomeless <- as.numeric(gsub(",","", alldata$TotalHomeless))
alldata$HomelessVeterans <- as.numeric(gsub(",", "", alldata$HomelessVeterans))
alldata$HomelessYouth <- as.numeric(gsub(",", "", alldata$HomelessYouth))


# http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
top = 49.3457868 # north lat
bottom =  24.7433195 # south lat
left = -124.7844079 # west long
right = -66.9513812 # east long

#select for states in continous United States
contusdata <- alldata %>% 
  filter(lat > 24 & lat < 50) %>% 
  filter(lon < -66 & lon > -124)

#Include a column to capture "State" from CoCNumber
contusdata$State = substr(contusdata$CoC.Number,1,2)

us <- map_data("state")
usstates <- read.csv("../Datasets/StateNames.csv")
usstates <- usstates %>%
  add_rownames("region") %>%
  mutate(region=tolower(StateName))

# Merge US States and Cost of Living by cities datasets for color-coding the US Map
usstates$StateName=NULL
usstates <- merge(usstates, contusdata, by.x="State", by.y="State")
#usstates$TotalHomeless <- as.numeric(gsub(",","", usstates$TotalHomeless))
#usstates$HomelessVeterans <- as.numeric(gsub(",", "", usstates$HomelessVeterans))
#usstates$HomelessYouth <- as.numeric(gsub(",", "", usstates$HomelessYouth))


CountsByState <- usstates %>% 
  group_by(State, region) %>% 
  summarize(TotalHomeless = sum(TotalHomeless), HomelessVeterans = sum(HomelessVeterans), HomelessYouth = sum(HomelessYouth)) 

CountsByState$Count.Category <- ifelse(CountsByState$TotalHomeless > 20000, "HIGH", ifelse(CountsByState$TotalHomeless > 10000, "MEDIUM", "LOW"))

# Plot a blank US map (for the presentation - EARL Boston)
BlankUSMap <- BlankUSMap + geom_map(data=us, map=us,
                                    aes(x=long, y=lat, map_id=region), 
                                    fill="darkseagreen2", color="black")

BlankUSMap <- BlankUSMap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank()) +
  theme(legend.position = "none")

BlankUSMap 

#Plot a blank US Map
BlankUSMap <- ggplot()
BlankUSMap <- BlankUSMap + geom_map(data=us, map=us,
                                    aes(x=long, y=lat, map_id=region), 
                                    fill="white", color="black")

#US Map, color coded based on the COL Index in the state
HomelessMap <- BlankUSMap + geom_map(data=CountsByState, map=us, 
                                aes(fill=TotalHomeless, map_id=region), color="#ffffff", size=0.15) 

#HomelessMap <- HomelessMap + scale_fill_continuous(low='lightblue', high='darkblue', guide='colorbar')
HomelessMap <- HomelessMap + scale_fill_continuous(low='lightgrey', high='lightblue', guide='colorbar')

HomelessMap <- HomelessMap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank()) +
  theme(legend.position = "none")

#Display the homeless map (color-coded by homeless counts)
HomelessMap

ggplot(CountsByState, aes(x=TotalHomeless)) +
  geom_histogram() +
  facet_wrap(~ Count.Category)


ggplot(CountsByState, aes(x=TotalHomeless)) +
  geom_density() 

ggplot(CountsByState, aes(x=TotalHomeless)) +
  geom_density() +
  facet_wrap(~ Count.Category)

# select top 100 based on homeless counts - adults
hdatatop100 <- tbl_df(subset(alldata, !((substr(alldata$CoC.Number,1,2) =="HI")) & !((substr(alldata$CoC.Number,1,2) =="PR")) )) %>%
  top_n(100, TotalHomeless)

Top100HomelessAdultsMap <- HomelessMap +
  geom_point(aes(x=lon, y=lat, size=TotalHomeless, colour="red", alpha=0.8), data=hdatatop100) + 
  #labs(x='Longitude', y='Latitude') +
  #ggtitle("Homeless Counts (Top 35)") +
  scale_size_continuous(name="Homeless Counts", range = c(2,12), guide = FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE) 
  
Top100HomelessAdultsMap

# select top 100 based on homeless counts - veterans
hvetsdatatop100 <- tbl_df(subset(alldata, !((substr(alldata$CoC.Number,1,2) =="HI")) & !((substr(alldata$CoC.Number,1,2) =="PR")) )) %>%
  top_n(100, HomelessVeterans)

Top100HomelessVeteransMap <- HomelessMap +
  geom_point(aes(x=lon, y=lat, size=HomelessVeterans, colour="red", alpha=0.8), data=hvetsdatatop100) + 
  scale_size_continuous(name="Homeless Counts", range = c(2,12), guide = FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE) 

Top100HomelessVeteransMap

# select top 100 based on homeless counts - Youth
hyouthdatatop100 <- tbl_df(subset(alldata, !((substr(alldata$CoC.Number,1,2) =="HI")) & !((substr(alldata$CoC.Number,1,2) =="AK")) & !((substr(alldata$CoC.Number,1,2) =="PR")) )) %>%
  top_n(100, HomelessYouth)

Top100HomelessYouthMap <- HomelessMap +
  geom_point(aes(x=lon, y=lat, size=HomelessYouth, colour="red", alpha=0.8), data=hyouthdatatop100) + 
  scale_size_continuous(name="Homeless Counts", range = c(2,12), guide = FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE) 

Top100HomelessYouthMap



#Read Grants dataset
GrantsDS <- read.csv("../Datasets/ESGGrantDSWithGeoCodes.csv") 
GrantsDStop35 <- tbl_df(GrantsDS) %>% 
  top_n(35, TOT_AMT)
GrantsDStop35 <- subset(GrantsDStop35, !(STUSAB == "PR"))

AllGrantsDStop35 <- group_by(GrantsDS, round(lat, 2)) %>% mutate(sum(TOT_AMT))

AllHomelessTop35 <- tbl_df(subset(alldata, !((substr(alldata$CoC.Number,1,2) =="HI")) & !((substr(alldata$CoC.Number,1,2) =="PR")) )) %>%
  top_n(25, TotalHomeless)
GrantsDStop35 <- tbl_df(subset(GrantsDS, !((substr(GrantsDS$STUSAB,1,2) =="HI")) & !((substr(GrantsDS$STUSAB,1,2) =="PR")) )) %>% 
  top_n(25, TOT_AMT)


us <- map_data("state")
usstates <- read.csv("../Datasets/StateNames.csv")
usstates <- usstates %>%
  add_rownames("region") %>%
  mutate(region=tolower(StateName))

COLByCities <- read.csv("../Datasets/COL_ByCities.csv")
COLByCities$UrbanArea <- gsub(", ", " ", COLByCities$UrbanArea)
COLByCities$State <- COLByCities$UrbanArea
COLByCities$State <- substrRight(COLByCities$State, 2)

usstates$StateName=NULL
usstates <- merge(usstates, COLByCities, by.x="State", by.y="State")

COLByCities <- group_by(COLByCities, State)
COLByCities <- summarise(COLByCities, COLStateAvg=mean(COLIndex), HousingIndex=mean(Housing))

COLMap <- BlankUSMap + geom_map(data=usstates, map=us, 
                                aes(fill=Housing, map_id=region), color="#ffffff", size=0.15) 

#COLMap <- COLMap + scale_fill_continuous(low='gray94', high='thistle3', guide='colorbar')
COLMap <- COLMap + scale_fill_continuous(low='lightgray', high='lightblue', guide='colorbar')

COLMap <- COLMap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())
#COLMap


GrantsAndCountsmap <- COLMap + 
  #geom_point(aes(x=lon, y=lat, size=TOT_AMT, colour="Total Grants", alpha=0.8), data=newGrantsDS) +
  geom_point(aes(x=lon, y=lat, size=TOT_AMT, colour="Total Grants", alpha=0.8), data=GrantsDStop35) + 
  geom_point(aes(x=lon, y=lat, size=TotalHomeless, colour="Homeless Counts", alpha=0.8), data=AllHomelessTop35) + 
  #geom_point(aes(x=lon, y=lat, size=TotalHomeless, colour="Homeless Counts", alpha=0.8), data=newHomelessDS) + 
  labs(x='Longitude', y='Latitude') +
  #ggtitle("Homeless Counts vs. Grants") +
  #scale_size_continuous(name="Total Grants", range = c(2,12), guide = FALSE) +
  scale_alpha(guide=FALSE) +
  #scale_colour_discrete(name="Category")
# use this to remove all legends
#  theme(legend.position="none")

GrantsAndCountsmap <- GrantsAndCountsmap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank()) +
  theme(legend.position = "none")

GrantsAndCountsmap
