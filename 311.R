library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(ggthemes)

bos.df <- read.csv("311.csv")
# boston <- fread("311.csv")
graffiti <- filter(bos.df,REASON == "Graffiti")

write.csv(graffiti,file="graffiti.csv")

#sample.row <- sample(nrow(bos.df),replace=F,size=50000)
#sample.df <- bos.df[sample.row,]

set.seed(1)
sm.df <- bos.df[sample(nrow(bos.df),replace=F,size=50000),]

ggplot(graffiti)+
  geom_bar(mapping=aes(x=neighborhood))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Histogram of 311 Calls for Graffiti by Neighborhood")+
  xlab("Neighborhood")+
  ylab("Count")

bos311 <- fread("bos311.csv")
graffiti.df <- filter(bos311,REASON == "Graffiti")

write.csv(graffiti.df,file="311graffiti.csv")

sum(graffiti.df$neighborhood=="") # 32 rows with blank neighborhood
blanks <- subset(graffiti.df,graffiti.df$neighborhood=="")
graffiti.df <- graffiti.df[!graffiti.df$neighborhood=="",]

ggplot(graffiti.df)+
  geom_bar(mapping=aes(x=neighborhood))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Histogram of 311 Calls for Graffiti by Neighborhood")+
  xlab("Neighborhood")+
  ylab("Count")

sort.df <- as.data.frame(sort(table(graffiti.df$neighborhood),decreasing=TRUE))

graffiti.df$LOCATION_ZIPCODE <- as.factor(graffiti.df$LOCATION_ZIPCODE)
ggplot(graffiti.df)+
  geom_bar(mapping=aes(x=LOCATION_ZIPCODE))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

library(leaflet)

zip2108 <- filter(graffiti.df,LOCATION_ZIPCODE==2108)

zip2115 <- filter(graffiti.df,LOCATION_ZIPCODE==2115)

map <- leaflet(data=graffiti.df) %>%
  addTiles() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>%
  addMarkers(clusterOptions = markerClusterOptions(freezeAtZoom=14),popup=~SubmittedPhoto, 
             label=~Location)

map

######


graffiti.df$Year <- year(graffiti.df$open_dt)

crime <- fread("crime.csv")

library(stringr)
split <- as.data.frame(str_split_fixed(crime$OCCURRED_ON_DATE," ",2))

library(lubridate)

crime$Date <- split$V1
count <- count(crime,Date)
count$Date <- as.Date(count$Date)
count$Year <- year(count$Date)

count <- count %>%
  separate(Date, sep="-")

ggplot(count)+
  geom_point(mapping=aes(x=Date,y=n))





# MCA
library(FactoMineR)
library(ggplot2)
library(explor)
mca.graffiti <- graffiti.df[,c("police_district","neighborhood","LOCATION_ZIPCODE",
                               "pwd_district")]
mca.graf <- MCA(mca.graffiti,graph=FALSE)
explor(mca.graf)

## Graffiti time series

graffiti.df$Open <- as.POSIXct(graffiti.df$open_dt, format="%m/%d/%Y %H:%M")
graffiti.df$Close <- as.POSIXct(graffiti.df$closed_dt, format="%m/%d/%Y %H:%M")

# Lag time (open to closed cases) in minutes
graffiti.df$diff <- difftime(graffiti.df$Close,graffiti.df$Open) / 86400
graf.closed <- filter(graffiti.df,CASE_STATUS == "Closed")

graf.closed$Year <- as.factor(year(graf.closed$Open))
graf.closed$Day <- format(graf.closed$Open,"%A")
graf.closed$Month <- month(graf.closed$Open,label = TRUE)
graf.closed$Day <- ordered(graf.closed$Day,levels=c("Monday","Tuesday","Wednesday",
                                                    "Thursday","Friday","Saturday",
                                                    "Sunday"))

## Graphing frequency of calls by day of week
summary <- as.data.frame(table(graf.closed$Day,graf.closed$Year))
colnames(summary) <- c("Day","Year","Freq")

filter <- c("2015","2016","2017","2018")
summary1 <- summary[summary$Year %in% filter,]

ggplot(summary1)+
  geom_bar(mapping=aes(x=Day,y=Freq,fill=Year),stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Dark2")

## Graphing frequency of calls by month

sum.month <- as.data.frame(table(graf.closed$Month,graf.closed$Year))
colnames(sum.month) <- c("Month","Year","Freq")
sum.month1 <- sum.month[sum.month$Year %in% filter,]

ggplot(sum.month1,aes(x=Month,y=Freq,color=Year,group=Year))+
  geom_smooth(method=loess,se=FALSE)+
  scale_fill_brewer(palette = "Dark2")

ggplot(sum.month1,aes(x=Month,y=Freq,color=Year,group=Year))+
  geom_line()+
  scale_fill_brewer(palette = "Dark2")

ggplot(sum.month1)+
  geom_bar(mapping=aes(x=Month,y=Freq,fill=Year),stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Dark2")

## From officeR presentation
bymonth <- as.data.frame(table(graf.closed$Year,graf.closed$Month))
colnames(bymonth) <- c("Year","Month","Freq")

avebymo <- aggregate(Freq ~ Month, data=bymonth, mean)
avebymo$Freq <- round(avebymo$Freq,2)

ggplot(avebymo)+
  geom_bar(aes(x=Month,y=Freq),stat="identity")+
  ggtitle("Average Frequency by Month")+
  theme_economist()

byday <- as.data.frame(table(graf.closed$Year,graf.closed$Day))
colnames(byday) <- c("Year","Day","Freq")

avebyday <- aggregate(Freq ~ Day, data = byday, mean)
avebyday$Freq <- round(avebyday$Freq,2)

ggplot(avebyday)+
  geom_bar(aes(x=Day,y=Freq),stat="identity")+
  ggtitle("Average Frequency by Day of Week")+
  theme_economist()

### Response Times in Days
rtbymo <- aggregate(diff ~ Year + Month, data = graf.closed, mean)
avertbymo <- aggregate(diff ~ Month, data=rtbymo, mean)

ggplot(rtbymo)+
  geom_bar(aes(x=Month,y=diff, fill=Year), stat="identity", position = "dodge")

ggplot(avertbymo)+
  geom_bar(aes(x=Month, y=diff),stat="identity")

rtbyday <- aggregate(diff ~ Year + Day, data = graf.closed, mean)
avertbyday <- aggregate(diff ~ Day, data = rtbyday, mean)

ggplot(rtbyday)+
  geom_bar(aes(x=Day,y=diff, fill=Year), stat="identity", position = "dodge")

ggplot(avertbyday)+
  geom_bar(aes(x=Day, y=diff),stat="identity")

## Facet grid

url.df <- bos.graf[,c("CASE_ENQUIRY_ID","SubmittedPhoto")]
url.df <- url.df[!url.df$SubmittedPhoto==""]
url.df$CASE_ENQUIRY_ID <- NULL
