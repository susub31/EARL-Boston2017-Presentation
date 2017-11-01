library(dplyr)
library(ggmap)

hdata <- read.csv("../Datasets/HomelessData2016.csv")
modhdata <- select(hdata, 1:3)
rm(hdata)

modhdata <- as.data.frame(modhdata)

for (i in 1: nrow(modhdata)) {
  cityname = gsub(",.*$", "", toString(modhdata[i,2]))
  cityname = gsub("/.*$", "", cityname)
  cityname = paste(cityname, ", ", substr(modhdata[i,1],1,2))
  modhdata$cityname[i] = cityname
  
  #latlon = geocode(toString(modhdata[i,2]))
  latlon = geocode(cityname)
  modhdata$lon[i] = as.numeric(latlon[1])
  modhdata$lat[i] = as.numeric(latlon[2])
}


# data clean-up 
# update the lat/lon for the counties that are not getting recognized in geocode function call
modhdata$lon[which(modhdata$CoC.Number=='NJ-515')] = as.numeric((geocode("Elizabeth, NJ"))[1])
modhdata$lat[which(modhdata$CoC.Number=='NJ-515')] = as.numeric((geocode("Elizabeth, NJ"))[2])

modhdata$lon[which(modhdata$CoC.Number=='PR-502')] = as.numeric((geocode("Puerto Rico"))[1])
modhdata$lat[which(modhdata$CoC.Number=='PR-502')] = as.numeric((geocode("Puerto Rico"))[2])

modhdata$lon[which(modhdata$CoC.Number=='GA-508')] = as.numeric((geocode("DeKalb County"))[1])
modhdata$lat[which(modhdata$CoC.Number=='GA-508')] = as.numeric((geocode("DeKalb County"))[2])

modhdata$lon[which(modhdata$CoC.Number=='GA-502')] = as.numeric((geocode("Fulton County"))[1])
modhdata$lat[which(modhdata$CoC.Number=='GA-502')] = as.numeric((geocode("Fulton County"))[2])

modhdata$lon[which(modhdata$CoC.Number=='CO-503')] = as.numeric((geocode("Metropolitan Denver"))[1])
modhdata$lat[which(modhdata$CoC.Number=='CO-503')] = as.numeric((geocode("Metropolitan Denver"))[2])

#make a copy of the dataset
homelessdatawithlatlon = modhdata

#write dataset to file (saved with lat/lon information)
write.csv(homelessdatawithlatlon, "../Datasets/HomelessData2016_With_LatLon.csv")


#Read Grants dataset
GrantsDS <- read.csv("../Datasets/GrantsDataset.csv") #, header = T)
#GrantsDS <- filter(GrantsDS, !str_detect(NAME, 'Nonentitle'))

GrantsDSWithoutNAs <- GrantsDS[!is.na(GrantsDS$lat),]
GrantsDSWithNAs <- GrantsDS[is.na(GrantsDS$lat),]
for (i in 1:nrow(GrantsDSWithNAs)) {
  latlon = geocode(gsub('Nonentitlement', '', (gsub('County', '', gsub('Counties', '', gsub('CoC', '', toString(GrantsDSWithNAs[i,3])))))))
  GrantsDSWithNAs$lon[i] = as.numeric(latlon[1])
  GrantsDSWithNAs$lat[i] = as.numeric(latlon[2])
}

GrantsDS <- rbind(GrantsDSWithoutNAs, GrantsDSWithNAs)
write.csv(GrantsDS, "../Datasets/GrantDSWithGeoCodes.csv")


