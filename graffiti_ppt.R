library(officer)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(htmltools)
library(rgdal)
library(Rmisc)
library(tidytext)
library(wordcloud)
library(data.table)
library(ggthemes)
library(scales)
library(geojsonio)

graffiti <- read_pptx("graffiti.pptx")

# Title Slide
graffiti <- graffiti %>%
  add_slide(layout="Title Slide",master="Ion Boardroom") %>%
  ph_with_text(type="ctrTitle",str="Boston 311: Investigating Graffiti Calls") %>%
  ph_with_text(type="subTitle",str="Data + Narrative with R Workshop")

# Slide 2: Introduction
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Introduction") %>%
  ph_with_ul(type="body",index = 1,
             str_list=c("Hanan Alsalamah", "Arvindh Raghavan", "Ian Riaf"),
             level_list=c(1,1,1)) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

#+++++++++++
boston311 <- read.csv("311graffiti.csv")
boston311$open_dt <- as.POSIXlt(boston311$open_dt,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
boston311$target_dt <- as.POSIXlt(boston311$target_dt,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
boston311$closed_dt <- as.POSIXlt(boston311$closed_dt,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
#++++++++++


## Slide 3: Top 15 Reasons for 311 Call
#++++++++++

data.311 <- fread("bos311.csv")

info <- data.311 %>% 
  select(REASON, TYPE, neighborhood, Source)
reason <- info %>% 
  group_by(REASON) %>%
  count(REASON) %>% 
  arrange(desc(n))

reason_u <- reason %>% 
  ungroup() %>% 
  top_n(15,n)
reason_u$REASON <- factor(reason_u$REASON, levels = fct_infreq(reason_u$REASON))

neighborhood <- info %>% 
  group_by(neighborhood) %>% 
  count(neighborhood) %>% 
  arrange(desc(n))
neighborhood_u <- neighborhood %>% 
  ungroup()
neighborhood_u$neighborhood <- factor(neighborhood_u$neighborhood, 
                                      levels = fct_infreq(neighborhood_u$neighborhood))
neighborhood_d <- neighborhood %>% 
  ungroup()
neighborhood_d$neighborhood <- factor(neighborhood_d$neighborhood, 
                                      levels = fct_infreq(neighborhood_d$neighborhood))
type <- info %>% 
  group_by(REASON) %>% 
  count(TYPE) %>% 
  arrange(desc(n))
type_u <- type %>% 
  ungroup() %>% 
  top_n(10,n)
source <- info %>% 
  group_by(Source) %>% 
  count(Source) %>% 
  arrange(desc(n))
source$Source <- factor(source$Source, levels = fct_infreq(source$Source))
area.color <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,"violetred3", NA, NA)

#+++++++++++

topreason <- ggplot(reason_u, aes(x = REASON, y = n)) +
  geom_bar(stat = "identity", fill = c("deepskyblue4","deepskyblue4","deepskyblue4","deepskyblue4",
                                       "deepskyblue4","deepskyblue4","deepskyblue4","deepskyblue4",
                                       "deepskyblue4","deepskyblue4","deepskyblue4", "deepskyblue4",
                                       "salmon2","deepskyblue4","deepskyblue4")) +
  labs(x = "Reason", y = "Cases") +
  scale_y_continuous(labels = comma) +
  theme_economist() +
  scale_color_economist(stata = TRUE) +
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Top 15 Reasons for 311 Calls") %>%
  ph_with_gg(type="body", index=1, value=topreason) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 4: Neighborhoods with Most Frequent 311 Calls
neigh <- ggplot(neighborhood_u, aes (x = neighborhood, y = n, fill="deepskyblue4")) +
  geom_bar(fill="deepskyblue4",stat = "identity") +
  labs(x = "Neighborhood", y = "Cases") +
  theme_economist() +
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_y_continuous(labels = comma)

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Frequency of 311 Calls by Neighborhood") %>%
  ph_with_gg(type="body", index=1, value=neigh) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")


### Slide 5: Scatterplot of Population Density
#++++++++
boston_social<- read_csv("Climate_Ready_Boston_Social_Vulnerability.csv")
population <- aggregate(boston_social$POP100_RE, by=list(Category=boston_social$Name), FUN=sum)
population[1,2] <- sum(population[1,2],population[4,2])
population <- population %>% filter(Category != "Brighton")
population[1,1] <- "Allston / Brighton" 
population[7,2] <-  sum(population[7,2], population[12,2])
population[7,1] <- "Fenway / Longwood" 
population <- population %>% filter(Category != "Longwood Medical Area")
population[17,2] <-  sum(population[17,2],population[18,2])
population[17,1] <- "South Boston / South Boston Waterfront" 
population <- population %>% filter(Category != "South Boston Waterfront")
population <- population %>% filter(Category != "Harbor Islands")  
population <- population %>% filter(Category != "West End")
population <- population %>% filter(Category != "Leather District")  
population[3,1] <- "Downtown / Financial District"
names(population)[2]<-paste("population")
info <- readRDS("info.rds")
names(population)[1] <- paste("neighborhood")
total <- merge(population, info, by="neighborhood")
names(total)[3] <- paste("call")
total$population <- total$population/sum(total$population)
total$call <- total$call/sum(total$call)
total$neighborhood[total$neighborhood=="Downtown / Financial District"] <- "Downtown"
#++++++++
scatter <- ggplot(total, mapping = aes(x=population, y=call))+
  geom_text(aes(label=neighborhood), size=5)+
  geom_abline(intercept = 0, slope = 1,colour = "deepskyblue4",size=1)+
  geom_jitter(size=0.000001)+
  theme_economist()+
  xlab("Proportion of Total Population")+
  ylab("Proportion of Total Calls")

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Plotting Population Density Versus Frequency of Calls") %>%
  ph_with_gg(type="body", index=1, value=scatter) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")


## Slide 6: Collage of Graffiti Images
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Looking at Graffiti in Boston") %>%
  ph_with_img(type="body", index=1, src="boston_graf_gallery.png",width=5) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 7: Graffiti images (Before and After) 
graffiti <- graffiti %>%
  add_slide(layout = "Comparison",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Looking at Graffiti in Boston") %>%
  ph_with_text(type="body",index=3,str="Before") %>%
  ph_with_img(type="body",index=1,src="img1b.jpg",width=4) %>%
  ph_with_text(type="body",index=2,str="After") %>%
  ph_with_img(type="body",index=4,src="img1a.jpg",width=4) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 8: Word Cloud
#++++++++
graffiti.word <- read.csv("graffiti_word.csv")[,-1]

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
graffiti_text_sentiment_stat <- graffiti.word %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)
bing_word_counts <- graffiti.word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  filter(sentiment=="positive")
sentiment <- bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
#+++++++++

graffiti.word.freq <- graffiti.word %>%
  count(word,sort=T)

gg.sent <- graffiti.word.freq %>%
  top_n(10) %>%
  ggplot(aes(reorder(word,n), n)) +
  geom_col(show.legend = FALSE,fill="deepskyblue4") +
  labs(y = "Counts",
       x = NULL) +
  ggtitle("Words Associated with Graffiti")+
  coord_flip()+
  theme_economist()

graffiti <- graffiti %>%
  add_slide(layout="Two Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Text Analysis of Tweets about Graffiti") %>%
  ph_with_img(type="body", index=1, src = "wordcloud.png") %>%
  ph_with_gg(type="body",index=2,value=gg.sent) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 9: Map of Call Frequency by Neighborhood
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Mapping the Boston Neighborhoods") %>%
  ph_with_img(type="body", index=1, src = "map_neighborhood.png",width=5) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 10: Map of Response Time by Neighborhood
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Mapping Response Times by Neighborhood") %>%
  ph_with_img(type="body", index=1, src = "map_responsetime.png",width=5) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 11: Average Graffiti Calls by Month
#+++++++
boston311$Year <- year(boston311$open_dt)
boston311$Month <- month(boston311$open_dt,label=TRUE)
boston311$Day <- format(boston311$open_dt, "%A")
boston311$Day <- ordered(boston311$Day,levels=c("Monday","Tuesday","Wednesday",
                                              "Thursday","Friday","Saturday",
                                              "Sunday"))

bymonth <- as.data.frame(table(boston311$Year,boston311$Month))
colnames(bymonth) <- c("Year","Month","Freq")
avebymo <- aggregate(Freq ~ Month, data=bymonth, mean)
avebymo$Freq <- round(avebymo$Freq,2)
Filter <- c("2015","2016","2017")
bymonth1 <- bymonth[bymonth$Year %in% Filter,]
#+++++++

bymo <- ggplot(avebymo,mapping=aes(x=Month, y=Freq))+
  geom_bar(fill="deepskyblue4", stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Average Graffiti Calls by Month")+
  xlab("Months")+
  ylab("Frequency")+
  theme_economist()

sp <- ggplot(bymonth1, aes(x=Month, y=Freq)) + 
  geom_point(size=3,color="deepskyblue4") + 
  facet_grid(Year ~ .)+
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="Pink", fill="#CCFFFF"),
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey",size = 0.5, 
                                        linetype = "solid"),
        axis.line = element_line(colour = "black"))+
  theme_economist(dkpanel=TRUE)+
  theme(panel.border = element_rect(color="black",fill=NA))+
  theme(legend.position="right")+
  ggtitle("Average Graffiti Calls by Year, 2015-2017")+
  xlab("Months")+
  ylab("Frequency")

graffiti <- graffiti %>%
  add_slide(layout = "Two Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Average Call Frequency, Month-by-Month") %>%
  ph_with_gg(type="body",index=2,value=bymo) %>%
  ph_with_gg(type="body",index=1,value=sp) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 12: Average Response Time by Month
#++++++++
rtbymo <- aggregate(diffopenclosed ~ Year + Month, data = boston311, mean)
avertbymo <- aggregate(diffopenclosed ~ Month, data=boston311, mean)
rtmo1 <- rtbymo[rtbymo$Year %in% Filter,]
rtmo1$diffopenclosed <- round(rtmo1$diffopenclosed,2)
#++++++++

graf.resp <- ggplot(avertbymo)+
  geom_bar(aes(x=Month, y=diffopenclosed),stat="identity",fill="deepskyblue4")+
  ggtitle("Average Response Time by Month")+
  xlab("Month")+
  ylab("Response Time (Days)")+
  theme_economist()

graf.rt <- ggplot(rtmo1, aes(x=Month, y=diffopenclosed)) + 
  geom_point(size=3,color="deepskyblue4") + 
  facet_grid(Year ~ .)+
  theme_economist(dkpanel=TRUE)+
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        panel.border = element_rect(color="black",fill=NA),
        axis.line = element_line(colour = "black"))+
  ggtitle("Average Response Time by Year, 2015-2017")+
  xlab("Month")+
  ylab("Response Time (Days)")

graffiti <- graffiti %>%
  add_slide(layout = "Two Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Average Response Time, Month-by-Month") %>%
  ph_with_gg(type="body",index=2,value=graf.resp) %>%
  ph_with_gg(type="body",index=1,value=graf.rt) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Final Slide
graffiti <- graffiti %>%
  add_slide(layout="Title Slide",master="Ion Boardroom") %>%
  ph_with_text(type="ctrTitle",str="Thank You!")


###

print(graffiti,target="graffiti.pptx")
