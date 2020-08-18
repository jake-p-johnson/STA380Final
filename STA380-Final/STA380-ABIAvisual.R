#################
# ABIA Plotting #
#################
rm(list=ls())

library(mosaic)
library(tidyverse)
library(ggplot2)

abia = read.csv("~/MSBAsummer20/STA380-master/data/ABIA.csv", stringsAsFactors=FALSE)
attach(abia)
airport_codes <- read.csv("~/MSBAsummer20/airportcodes.csv", strip.white=TRUE)

summary(abia)

par(mfrow=c(1,3))
hist(DepTime)
hist(DepDelay)
hist(Distance)

model1 = lm(DepDelay ~ DepTime + Distance + DayOfWeek + AirTime, data=abia)
confint(model1, level=0.95)
summary(model1)
table(Origin)
# have to get rid of missing values to calculate avgs
depDelay_clean = abia[!is.na(abia$DepDelay), ]
arrDelay_clean = abia[!is.na(abia$ArrDelay), ]

#########################

#by departure time
by_hour <-aggregate(depDelay_clean$DepDelay,by=list(depDelay_clean$DepTime), FUN=mean, na.rm=TRUE)
#names(by_DOW_hour)
names(by_hour)[1] <- "Departure.Hour"
names(by_hour)[2] <- "Avg.Delay"

by_hour <- by_hour[order(by_hour$Departure.Hour),]

#Remove departure hour 1
by_hour <- subset(by_hour,by_hour$Departure.Hour != 1)

ggplot(data= by_hour, aes(x=Departure.Hour, y=Avg.Delay)) + 
  geom_line() + geom_point( size=4, shape=21, fill="white") + 
  ggtitle("Average delay with respect to scheduled departure time") + 
  theme(plot.title = element_text(lineheight=1.2, face="bold"))

#arrival v departure delay by airline
ggplot(data = abia) + 
  geom_point(mapping = aes(x = ArrDelay, y = DepDelay)) + 
  facet_wrap(~ UniqueCarrier, nrow = 4)

# arrival delay vs flight time by airline
ggplot(data = abia) + 
  geom_point(mapping = aes(x = ArrDelay, y = ActualElapsedTime)) + 
  facet_wrap(~ UniqueCarrier, nrow = 4)

#flight delays by airline
airline_delay = depDelay_clean %>%
  group_by(UniqueCarrier)  %>%
  summarize(avg_delay_mins = mean(DepDelay))

ggplot(airline_delay, aes(x=reorder(UniqueCarrier, avg_delay_mins), y=avg_delay_mins)) + 
  geom_bar(stat='identity', position='dodge') +
  labs(title = "Average departure delay by airline")

#flight delays by month

airline_list = c('AA', 'WN', 'CO', 'YV')

flights_per_month = depDelay_clean %>%
  filter(UniqueCarrier %in% airline_list) %>%
  group_by(UniqueCarrier, Month) %>%
  summarize(avg_delay_mins = mean(DepDelay))

ggplot(flights_per_month, aes(x=Month, y=avg_delay_mins)) + 
  geom_line( color='blue') +
  geom_point( size=4, shape=21, fill="white") +
  facet_wrap(~ UniqueCarrier, nrow = 2) +
  theme_bw(base_size=18) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "Average departure delay by month")

#flight delays by day of month
flights_per_day = depDelay_clean %>%
  filter(UniqueCarrier %in% airline_list) %>%
  group_by(UniqueCarrier, DayofMonth) %>%
  summarize(avg_delay_mins = mean(DepDelay))

ggplot(flights_per_day, aes(x=DayofMonth, y=avg_delay_mins)) + 
  geom_line( color='blue') +
  geom_point( size=4, shape=21, fill="white") +
  facet_wrap(~ UniqueCarrier, nrow = 2) +
  theme_bw(base_size=18) +
  scale_x_continuous(breaks = 1:31) +
  labs(title = "Average departure delay by day of month")

#flight delays by day of week
flights_per_week = depDelay_clean %>%
  filter(UniqueCarrier %in% airline_list) %>%
  group_by(UniqueCarrier, DayOfWeek) %>%
  summarize(avg_delay_mins = mean(DepDelay))

ggplot(flights_per_week, aes(x=DayOfWeek, y=avg_delay_mins)) + 
  geom_line( color='blue') +
  geom_point( size=4, shape=21, fill="white") +
  facet_wrap(~ UniqueCarrier, nrow = 2) +
  theme_bw(base_size=18) +
  scale_x_continuous(breaks = 1:31) +
  labs(title = "Average departure delay by day of week")

#flight delays by dep time
flights_delay_deptime = depDelay_clean %>%
  group_by(DepTime) %>%
  summarize(avg_delay_mins = mean(DepDelay))

ggplot(flights_delay_deptime) + 
  geom_line(aes(x=DayOfWeek, y=avg_delay_mins), color='blue') +
  theme_bw(base_size=18) +
  scale_x_continuous(breaks = 1:24) +
  labs(title = "Average departure delay by scheduled departure time - Month level")

#arrival delay by destination
arr_delay_dest = arrDelay_clean %>%
  group_by(Dest) %>%
  summarize(avg_delay_mins = mean(ArrDelay))

ggplot(arr_delay_dest, aes(x=reorder(Dest, avg_delay_mins), y=avg_delay_mins)) + 
  geom_bar(stat='identity') +
  labs(title = "Average arrival delay by Dest") +
  coord_flip()

#departure delay by origin
dep_delay_org = depDelay_clean %>%
  group_by(Origin) %>%
  summarize(avg_delay_mins = mean(DepDelay))

ggplot(dep_delay_org, aes(x=reorder(Origin, avg_delay_mins), y=avg_delay_mins)) + 
  geom_bar(stat='identity') +
  labs(title = "Average departure delay by Origin") +
  coord_flip()


#######################
# map

library(usmap)
library(ggplot2)

Origin = airport_codes$iata_code

df_tmp = data.frame(Origin,airport_codes$coordinates)

df_tmp = df_tmp[!(df_tmp$Origin=="" | df_tmp$Origin=="0" | df_tmp$Origin=="-"),]

df_tmp = df_tmp[!duplicated(df_tmp[ , "Origin"]),]

abia_coor = merge(abia, df_tmp, by = "Origin")

abia_lat_lon = separate(abia_coor,airport_codes.coordinates, c('lat','lon'), ",", remove=TRUE)

usa = us_map(regions = "states")

dep_delay_org = depDelay_clean %>%
  group_by(Origin) %>%
  summarize(avg_delay_mins = mean(DepDelay))

plot_usmap() +
   geom_point(data = abia_lat_lon, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = sqrt(mean(abia_lat_lon$DepDelay)), shape = 21) + 
   guides(fill=FALSE, alpha=FALSE, size=FALSE) +
   labs(title="Average departure delay time by all destination Airports", x="Longitude", y="Latitude") + 
   theme(plot.title = element_text(hjust = 0, vjust = 1, face = c("bold")))

###############