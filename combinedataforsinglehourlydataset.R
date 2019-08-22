library(data.table) # for faster loading on large dataset
library(tidyverse) # for ggplot, dplyr
library(tis) # for holiday function

busystation<-NULL
timeall<-NULL
hourlist<-seq(0,23,by=1)

#load raw weather data, scraping by website weather underground by python
weather<- as.data.frame(fread("C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\Weather.csv"))
weather$Time <- format(strptime(weather$Time, "%I:%M %p"), "%H:%M")
weather$Time<-as.integer(substr(weather$Time,1,2))+round(as.numeric(substr(weather$Time,4,5))/60)
weather$Time<-replace(weather$Time, weather$Time==24, 0)
weather<-weather[4:(nrow(weather)-20),]
weather$Date[(nrow(weather)-2):nrow(weather)]<-rep("2019-03-31",3)
rownames(weather) <- NULL
newweather<-NULL


for (y in c(2017,2018,2019)){
        for (m in seq(1,12,by=1)){
                if (y == 2019 & m == 4) { return() }
                ## load blue bike rental data from https://s3.amazonaws.com/hubway-data/index.html
                path<-paste0("C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\hubway\\",y,sprintf("%02i",m),"-bluebikes-tripdata.csv")
                bikedata <- as.data.frame(fread(path))
                
                # dataset for busy station definition and visulization
                bikedata$hour<-as.factor(substr(bikedata$starttime,12,13))
                NS<-bikedata$`end station latitude`-bikedata$`start station latitude`
                bikedata$direction_NS<-"S"
                bikedata$direction_NS[NS>0]<-"N"
                bikedata$direction_NS[NS==0]<-"0"
                EW<-bikedata$`end station longitude`-bikedata$`start station longitude`
                bikedata$direction_EW<-"W"
                bikedata$direction_EW[EW>0]<-"E"
                bikedata$direction_EW[EW==0]<-"0"
                bikedata$duration_min <- round(bikedata$tripduration/60, 0)
                bikedata1<-bikedata[c("starttime","stoptime","start station id","start station latitude","start station longitude",
                                      "end station id","end station latitude","end station longitude",
                                      "direction_NS","direction_EW","duration_min","usertype")]
                busystation<-rbind(busystation,bikedata1)
                
                # # dataset for time series and ML analysis
                # bikedata2<-bikedata[c("starttime","usertype")]
                # bikedataall<-rbind(bikedataall,bikedata2)
                
                
                for(d in seq(1,31,by=1)){
                        # Check if leap year
                        if (y%%400 == 0){
                                leap = TRUE
                        } else if (y%%100 == 0){
                                leap = FALSE
                        } else if (y%%4 == 0){
                                leap = TRUE
                        } else leap = FALSE
                        
                        # check date of every month
                        if (m == 2 & leap & d > 29){
                                next
                        } else if (m == 2 & d > 28){
                                next
                        } else if (m %in% c(4, 6, 9, 11) & d > 30){
                                next
                        }
                        
                        #make a complete time list to storage dates and hours
                        timelist<-cbind(rep(paste(y,m,d,sep="-"),24),hourlist)
                        timeall<-rbind(timeall,timelist)
                        
                        
                        # data cleasing for weather data
                        part=weather[weather$Date==paste(y,sprintf("%02d",m),sprintf("%02d",d),sep="-"),]
                        if (nrow(part)>10){
                                part1<-part[1:10,]
                                part2<-part[11:nrow(part),]
                                part1$Date[part1$Time>19]<-format(as.Date(part1$Date[part1$Time>19])-1,"%Y-%m-%d")
                                part<-rbind(part1,part2)
                                
                        } else message(paste0(y,sprintf("%02d",m),sprintf("%02d",d)," less than 10 rows"))
                        part<-part[!duplicated(part[,c("Time","Date")]),]
                        newweather<-rbind(newweather,part)
                        
                }
                
        }
}

# remove daylight savings hours
timeall<-as.data.frame(timeall)
colnames(timeall) <- c("date","hour")
timeall<-timeall[!(timeall$date %in% c("2017-3-12","2018-3-11","2019-3-10") & timeall$hour==2),]
winterdaylight<-data.frame(date=c("2017-11-5","2018-11-4"),hour=c(2,2))
timeall<-rbind(timeall,winterdaylight)

# dataset for time series and ML analysis
bikedataall<-busystation[,c("starttime","usertype")]
bikedataall$newtime<-format(round(as.POSIXct(bikedataall$starttime, format="%Y-%m-%d %H:%M:%S"), units="hours"))
#combine bike usage in function of hour
bikedata3<-bikedataall %>%
        group_by(newtime,usertype) %>%
        summarize(count = n())


bikedata3$hour<-substr(bikedata3$newtime,12,13)
bikedata3 %>% ggplot(aes(x = hour, y = count,color=usertype)) +
        geom_point(alpha = 0.2) +
        labs(title = "BlueBikes Usage in each hour",
             x = "Hour",  y = "BlueBikes Used") +
        theme_bw()
bikedataall<-bikedata3[,-4]

newweather$Temperature = as.numeric(substr(newweather$Temperature,1,nchar(newweather$Temperature)-1))
newweather$`Dew Point` = as.numeric(substr(newweather$`Dew Point`,1,nchar(newweather$`Dew Point`)-1))
newweather$Humidity = as.numeric(substr(newweather$Humidity,1,nchar(newweather$Humidity)-1))
newweather$`Wind Speed` = as.numeric(substr(newweather$`Wind Speed`,1,nchar(newweather$`Wind Speed`)-3))
newweather$Pressure = as.numeric(substr(newweather$Pressure,1,nchar(newweather$Pressure)-2))
newweather$Precip.=as.numeric(substr(newweather$Precip.,1,nchar(newweather$Precip.)-2))

library(GGally)
# strong correlation between "Temperature" and "Dew Point", drop Dew point
ggpairs(newweather[, c("Temperature","Dew Point","Humidity","Wind Speed","Pressure","Precip.")])

newweather<-newweather[,c("Time","Temperature", "Humidity","Wind Speed","Pressure","Precip.","Condition","Date" )]


write.csv(bikedataall[bikedataall$usertype=='Customer',],"C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\bikedata_customer.csv", row.names = FALSE)
write.csv(bikedataall[bikedataall$usertype=='Subscriber',],"C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\bikedata_subscriber.csv", row.names = FALSE)
write.csv(timeall,"C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\timelist.csv", row.names = FALSE)
write.csv(newweather,"C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\weather2.csv", row.names = FALSE)
rm(bikedataall)
rm(timeall)
rm(newweather)



## busiest start and return bike station visulization and data analysis
#handling busystation start
busystation$newtime<-format(round(as.POSIXct(busystation$starttime, format="%Y-%m-%d %H:%M:%S"), units="hours"))
busystation$date=substr(busystation$newtime,1,10)
busystation$hour<-substr(busystation$newtime,12,13)
busystation<-busystation[,-1]
busystation$year<-substr(busystation$newtime,1,4)
busystation$month<-substr(busystation$newtime,6,7)
busystation<-busystation[!busystation$newtime=="2019-04-01 00:00:00",]

# visualization of monthly bike data
yearmonth1<-busystation%>%
        filter(usertype=="Subscriber") %>%
        group_by(year,month) %>%
        summarize(total=n())
yearmonth1$type<-"Subscriber"
yearmonth2<-busystation%>%
        filter(usertype=="Customer") %>%
        group_by(year,month) %>%
        summarize(total=n())
yearmonth2$type<-"Customer"
yearmonth1<-rbind(yearmonth1,yearmonth2)

#stacked barchart
ggplot(yearmonth1, aes(fill=type, y=total, x=paste(year,month,sep=''))) + 
        geom_bar( stat="identity") +    
        scale_fill_brewer(palette = "Set1") +
        labs(x = "TIme", y = NULL, fill = NULL ,title = "Stacked barplot of bike rental")
# Faceting
ggplot(yearmonth1, aes(fill=type, y=total, x=paste(year,month,sep=''))) + 
        geom_bar( stat="identity") +    
        facet_wrap(~type)+
        labs(x = "TIme", y = NULL, fill = NULL ,title = "Stacked barplot of bike rental")

# exclude super long duration use history
# visulize duration column
# stats for duration_min 1  7 11 19 37
duration_normal<-busystation$duration_min[(busystation$duration_min<=boxplot.stats(busystation$duration_min)$stats[5])&
                                                  (busystation$duration_min>=boxplot.stats(busystation$duration_min)$stats[1])]
hist(duration_normal, prob=TRUE, # prob=TRUE for probabilities not counts
     main="Desntiy Plot of rental time", 
     xlab="Duration", 
     border="red", 
     col="grey")
lines(density(duration_normal), col="orange", lwd=2) # add a density estimate with defaults

# visulize duration distribution of different directions 
duration_dire <- busystation[,c('direction_NS','direction_EW','duration_min')]
duration_dire$to_dire<-paste0(duration_dire$direction_NS,duration_dire$direction_EW)
direction_min <- duration_dire %>%
        group_by(to_dire, duration_min) %>%
        summarise(total = n())  %>%
        filter(to_dire!= "00")
NE<-direction_min[direction_min$to_dire=='NE',]
NE<-NE[NE$duration_min %in% boxplot.stats(NE$duration_min)$stats[1]:boxplot.stats(NE$duration_min)$stats[5],]

NW<-direction_min[direction_min$to_dire=='NW',]
NW<-NW[NW$duration_min %in% boxplot.stats(NW$duration_min)$stats[1]:boxplot.stats(NW$duration_min)$stats[5],]

SE<-direction_min[direction_min$to_dire=='SE',]
SE<-SE[SE$duration_min %in% boxplot.stats(SE$duration_min)$stats[1]:boxplot.stats(SE$duration_min)$stats[5],]

SW<-direction_min[direction_min$to_dire=='SW',]
SW<-SW[SW$duration_min %in% boxplot.stats(SW$duration_min)$stats[1]:boxplot.stats(SW$duration_min)$stats[5],]

direction_min2<-rbind(NE,NW,SE,SW)

direction_min2 %>% ggplot(aes(x = duration_min)) +
        geom_density(alpha = 0.3, fill = "black") +
        guides(fill = FALSE) +
        labs(title = "Density Plot of Distance Based on direction (to which)" ,
             x = "Duration (min)",
             y = "Density") +
        facet_wrap(~ to_dire) 


daysum_s<-busystation %>%
        group_by(date,usertype) %>%
        summarise(total=n())
# Multiple line plot for daily bike usage
ggplot(daysum_s, aes(x = date, y = total, group=usertype)) + 
        geom_line(aes(color = usertype), size = 1) +
        scale_color_manual(values = c("#00AFBB", "#E7B800"))+
        theme_minimal() +
        ggtitle("BlueBikes usage by day")


daysum<-busystation %>%
        group_by(date) %>%
        summarise(total=n())

daysum<-daysum[order(-daysum$total,daysum$date), ]

busiestday<-busystation %>%
        filter(date==unlist(daysum[1,1])) %>%
        group_by(hour,`start station id`,`start station latitude`,`start station longitude`) %>%
        summarize(total=n())

busiestday %>% ggplot(aes(x = hour, y = total,group=`start station id`)) +
        geom_line(aes(color = `start station id`)) +
        theme_minimal()+
        labs(title = "Total of Blue Bikes depart from each station in each hour, 2018-09-14",
             x = "Hour",  y = "Total of Blue Bikes leave")

sort1.start<-busiestday[order(-busiestday$total,busiestday$hour),]
head(sort1.start)
#18h stationid 80 "MIT Stata Center at Vassar St / Main St"

#count each direction number 
direction1<-busystation %>%
        filter(date==unlist(daysum[1,1])&hour==unlist(sort1.start$hour[1])
               &`start station id`==unlist(sort1.start$`start station id`[1])) %>%
        group_by(direction_NS,direction_EW) %>%
        summarize(total=n())
#count each direction rental time
usetime1<-busystation %>%
        filter(date==unlist(daysum[1,1])&hour==unlist(sort1.start$hour[1])
               &`start station id`==unlist(sort1.start$`start station id`[1])) %>%
        group_by(direction_NS,direction_EW) %>%
        summarize(mean_time = mean(duration_min, na.rm = TRUE))

direction1$meantime<-usetime1$mean_time
direction1$to_dire<-paste0(direction1$direction_NS,direction1$direction_EW)
direction1<-direction1[,c(-1,-2)]


ggplot(direction1,aes(x = to_dire,y = total,fill = to_dire)) +
        geom_col(width = 1, color = "white")+
        coord_polar()+ 
        labs(x = "", y = "", title = "Direction count pie chart for busiest departure station in busiest day")+
        theme_minimal()

ggplot(direction1,aes(x = to_dire,y = meantime,fill = to_dire)) +
        geom_col(width = 1, color = "white")+
        coord_polar()+ 
        labs(x = "", y = "", title = "Direction duration pie chart for busiest departure station in busiest day")+
        theme_minimal()

library(leaflet) # interactive mapping 
library(leaflet.extras) #extra mapping for leaflet
# 2018-09-14 18:00, how busy the stations are to start bike rental
busiestday[busiestday$hour==unlist(sort1.start[1,1]),] %>% 
        leaflet() %>%
        setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>% 
        addTiles( ) %>%
        addHeatmap(lng = busiestday$`start station longitude`, lat = busiestday$`start station latitude`, 
                   max = 2, radius = 15) %>%
        addMarkers(lng = busiestday$`start station longitude`[busiestday$`start station id`
                                                              == unlist(sort1.start$`start station id`[1])],
                   lat = busiestday$`start station latitude`[busiestday$`start station id`
                                                             == unlist(sort1.start$`start station id`[1])], 
                   popup =busiestday$`start station id`[ busiestday$`start station id`
                                                         ==unlist(sort1.start$`start station id`[1])])


#handling busystation end
busystation$newtime<-format(round(as.POSIXct(busystation$stoptime, format="%Y-%m-%d %H:%M:%S"), units="hours"))
busystation$date=substr(busystation$newtime,1,10)
busystation$hour<-substr(busystation$newtime,12,13)
busystation<-busystation[,-1]

daysum2<-busystation %>%
        group_by(date) %>%
        summarise(total=n())
daysum2<-daysum2[order(-daysum2$total,daysum2$date), ]

busiestday2<-busystation %>%
        filter(date==unlist(daysum2[1,1])) %>%
        group_by(hour,`end station id`,`end station latitude`,`end station longitude`) %>%
        summarize(total=n())

busiestday2 %>% ggplot(aes(x = hour, y = total,group=`end station id`)) +
        geom_line(aes(color = `end station id`))  +
        theme_minimal()+
        labs(title = "Total of Blue Bikes return to each station in each hour, 2018-09-14",
             x = "Hour",  y = "Total of Blue Bikes return")

sort2.end<-busiestday2[order(-busiestday2$total,busiestday2$hour),]
head(sort2.end)
#17h stationid 190 "Nashua Street at Red Auerbach Way"
# to see what kind of user at that time
busystation %>%
        filter(date=="2018-09-14"&`end station id`==190 & hour=='17') %>%
        group_by(usertype) %>%
        summarize(total=n())
# Customer=3,Subscriber=80 
# count each direction number 
direction2<-busystation %>%
        filter(date==unlist(daysum2[1,1])&hour==unlist(sort2.end$hour[1])
               &`end station id`==unlist(sort2.end$`end station id`[1])) %>%
        group_by(direction_NS,direction_EW) %>%
        summarize(total=n())
#count each direction rental time
usetime2<-busystation %>%
        filter(date==unlist(daysum2[1,1])&hour==unlist(sort2.end$hour[1])
               &`end station id`==unlist(sort2.end$`end station id`[1])) %>%
        group_by(direction_NS,direction_EW) %>%
        summarize(mean_time = mean(duration_min, na.rm = TRUE))

direction2$meantime<-usetime2$mean_time
direction2$from_dire<-paste0(direction2$direction_NS,direction2$direction_EW)
direction2<-direction2[,c(-1,-2)]

ggplot(direction2,aes(x = from_dire,y = total,fill = from_dire)) +
        geom_col(width = 1, color = "white")+
        coord_polar()+ 
        labs(x = "", y = "", title = "Direction count pie chart for busiest return station in busiest day")+
        theme_minimal()

ggplot(direction2,aes(x = from_dire,y = meantime,fill = from_dire)) +
        geom_col(width = 1, color = "white")+
        coord_polar()+ 
        labs(x = "", y = "", title = "Direction duration pie chart for busiest return station in busiest day")+
        theme_minimal()


# 2018-09-14 17:00, how busy the stations are to return bikes
busiestday2[busiestday2$hour==unlist(sort2.end[1,1]),] %>% 
        leaflet() %>%
        setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>% 
        addTiles( ) %>%
        addHeatmap(lng = busiestday2$`end station longitude`, lat = busiestday2$`end station latitude`, 
                   max = 2, radius = 15) %>%
        addMarkers(lng = busiestday2$`end station longitude`[busiestday2$`end station id`
                                                              == unlist(sort2.end$`end station id`[1])],
                   lat = busiestday2$`end station latitude`[busiestday2$`end station id`
                                                             == unlist(sort2.end$`end station id`[1])], 
                   popup =busiestday2$`end station id`[ busiestday2$`end station id`
                                                         ==unlist(sort2.end$`end station id`[1])])
rm(list=ls())

## main part of time series forecasting and machine learning prediction
# function used to define the season for bikedata and bikedata2
getSeason <- function(DATES) {
        WS <- as.Date("2016-12-15", format = "%Y-%m-%d") # Winter Solstice 4
        SE <- as.Date("2016-3-15",  format = "%Y-%m-%d") # Spring Equinox 1
        SS <- as.Date("2016-6-15",  format = "%Y-%m-%d") # Summer Solstice 2
        FE <- as.Date("2016-9-15",  format = "%Y-%m-%d") # Fall Equinox 3
        
        # Convert dates from any year to 2016 dates
        d <- as.Date(strftime(DATES, format="2016-%m-%d"))
        
        ifelse (d >= WS | d < SE, 4,
                ifelse (d >= SE & d < SS, 1,
                        ifelse (d >= SS & d < FE, 2, 3)))
}

# data cleasing for subscriber bike rentale data
bikedata <- as.data.frame(fread("C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\bikedata_subscriber.csv"))
bikedata$newtime<-as.POSIXct(bikedata$newtime,format="%Y-%m-%d %H")
bikedata<-bikedata[,-2]
timeall<- as.data.frame(fread("C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\timelist.csv"))
timeall$newtime<-as.POSIXct(paste(timeall$date, timeall$hour), format="%Y-%m-%d %H")
bikedata<-merge(x = bikedata, y = timeall, by = "newtime", all= TRUE)
bikedata<-bikedata[!is.na(bikedata$date),]
bikedata$count[is.na(bikedata$count)]<-0

bikedata$season<-getSeason(as.Date(bikedata$date))
bikedata$season<-as.factor(bikedata$season)
bikedata$day<-weekdays(as.Date(bikedata$date))
bikedata$day<- factor(bikedata$day, levels = c("Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)
bikedata$day<-as.factor(as.integer(bikedata$day))
bikedata$holidays<-isHoliday(as.Date(bikedata$date))
bikedata$holidays<-as.factor(as.numeric(bikedata$holidays))
bikedata$hour<-as.factor(bikedata$hour)
bikedata$month<-as.factor(as.integer(substr(bikedata$newtime,6,7)))
bikedata<-bikedata[,-3]
boxplot(bikedata$count~bikedata$hour)
# transformation count, considering 0 therefore use log(x+1)
hist(bikedata$count)
hist(log(bikedata$count+1))

# data cleasing for casual bike rentale data
bikedata2 <- as.data.frame(fread("C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\bikedata_customer.csv"))
bikedata2$newtime<-as.POSIXct(bikedata2$newtime,format="%Y-%m-%d %H")
bikedata2<-bikedata2[,-2]
bikedata2<-merge(x = bikedata2, y = timeall, by = "newtime", all= TRUE)
bikedata2<-bikedata2[!is.na(bikedata2$date),]
bikedata2$count[is.na(bikedata2$count)]<-0

bikedata2$season<-getSeason(as.Date(bikedata2$date))
bikedata2$season<-as.factor(bikedata2$season)
bikedata2$day<-weekdays(as.Date(bikedata2$date))
bikedata2$day<- factor(bikedata2$day, levels = c("Monday", "Tuesday", "Wednesday",                                               "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)
bikedata2$day<-as.factor(as.integer(bikedata2$day))
bikedata2$holidays<-isHoliday(as.Date(bikedata2$date))
bikedata2$holidays<-as.factor(as.numeric(bikedata2$holidays))
bikedata2$hour<-as.factor(bikedata2$hour)
bikedata2$month<-as.factor(as.integer(substr(bikedata2$newtime,6,7)))
bikedata2<-bikedata2[,-3]
boxplot(bikedata2$count~bikedata2$hour)# too much outliers
 # transformation count, considering 0 therefore use log(x+1)
hist(bikedata2$count)
hist(log(bikedata2$count+1))
## visualization for week
boxplot(bikedata$count~bikedata$day)
boxplot(bikedata2$count~bikedata2$day)

# data cleasing for weather data
weather<-as.data.frame(fread("C:\\Users\\Minjie\\Documents\\2018 Fall Master\\Data Mining\\project\\weather2.csv"))
weather[(weather$Date %in% c("2017-03-12","2018-03-11","2019-03-10") & weather$Time==2),"Time"]<-3

hist(weather$Temperature)
boxplot(weather$Temperature)
hist(weather$Humidity)
boxplot(weather$Humidity)
hist(weather$`Wind Speed`)
boxplot(weather$`Wind Speed`)# right skewed
hist(log(weather$`Wind Speed`+1))
boxplot(log(weather$`Wind Speed`+1))
weather$`Wind Speed`<- log(weather$`Wind Speed`+1)
weather$Pressure[weather$Pressure==0]<-NA # remove outliers
hist(weather$Pressure)
hist(weather$Precip.)
boxplot(weather$Precip.)
table(weather$Precip.) # majority is 0
# to visulize precipitation
precipitation<-weather %>%
        group_by(Date) %>%
        summarize(sum_precip=sum(Precip.))
ggplot(data = precipitation,  aes(x = Date, y = sum_precip)) +
        geom_point(alpha = .5, color = "blue")+
        labs(x = NULL, y = "Daily_Precipitation_in", title = "Daily precipitation in Boston")

# to visulize wind speed
weather$month<-as.factor(as.integer(substr(weather$Date,6,7)))
qplot(`Wind Speed`,data=weather,fill=month,xlab="wind speed in mph", ylab ="density",geom="density")
weather<-weather[,1:8]

## word cloud for weather condition
weather$Condition<-gsub("T-Storm", "TStorm",weather$Condition)
#install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML"))
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
# Load the data as a corpus
docs <- Corpus(VectorSource(weather$Condition))
#remove special characters from the text.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "-")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# Build a term-document matrix
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=Inf, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))


conditionlist<-as.data.frame(table(weather$Condition))
index1<-c(3,4,7,8,29,30,31,32,33) # weather condition category 1
index2<-c(6,9,10,11,28,34,39) #weather condition category 2
index3<-c(5,18,19,20,21,22,23,24,25,26,27,35,36,37,38,47) #weather condition category 3
index4<-c(1,2,12,13,14,15,16,17,40,41,42,43,44,45,46,48,49) #weather condition category 4
conditionlist$category<-NA
conditionlist$category[index1]<-1
conditionlist$category[index2]<-2
conditionlist$category[index3]<-3
conditionlist$category[index4]<-4
for (name in conditionlist$Var1)
        weather$Condition[weather$Condition==name]<-conditionlist$category[conditionlist$Var1==name]

weather$newtime<-as.POSIXct(paste(weather$Date, weather$Time), format="%Y-%m-%d %H")
weather<-merge(x = weather, y = timeall, by = "newtime", all.y= TRUE)
#remove duplicated row
weather<-weather[!duplicated(weather),c(-2,-9,-10,-11)]

# one hot encoding for bikedata
library(mltools)
bikedata <- one_hot(as.data.table(bikedata))
# drop the last categorical column as dummy variable (linear depandency)
bikedata<-bikedata[,c(-26,-30,-37,-38,-51)]

# one hot encoding for bikedata2
bikedata2 <- one_hot(as.data.table(bikedata2))
bikedata2<-bikedata2[,c(-26,-30,-37,-38,-51)]
# drop the last categorical column as dummy variable (linear depandency)

#join bikedata and weather on newtime
bikedata<-merge(x = bikedata, y = weather, by = "newtime", all = FALSE)
bikedata<-bikedata[!duplicated(bikedata[,-47:-52]),]
# check how many NA
table(is.na(bikedata))
# FALSE    TRUE 
# 1021887    1317 

# fill NA values by interpolation
library(zoo)
bikedata$Temperature <- na.approx(bikedata$Temperature)
bikedata$Humidity <- na.approx(bikedata$Humidity)
bikedata$`Wind Speed`<-na.approx(bikedata$`Wind Speed`)
bikedata$Pressure<-na.approx(bikedata$Pressure)

bikedata$Precip.<-na.approx(bikedata$Precip.)
bikedata$Precip.[bikedata$Precip.>0]<-1
bikedata$Precip.=as.factor(bikedata$Precip.)

#fill NA by value of last row
bikedata$Condition<-na.locf(bikedata$Condition)
bikedata$Condition=factor(bikedata$Condition,levels = c("1", "2", "3","4"),ordered = TRUE)

colnames(bikedata)[49]<-"WindSpeed"


#join bikedata2 and weather on newtime
bikedata2<-merge(x = bikedata2, y = weather, by = "newtime", all = FALSE)
bikedata2<-bikedata2[!duplicated(bikedata2[,-47:-52]),]
# fill NA values by interpolation
bikedata2$Temperature <- na.approx(bikedata2$Temperature)
bikedata2$Humidity <- na.approx(bikedata2$Humidity)
bikedata2$`Wind Speed`<-na.approx(bikedata2$`Wind Speed`)
bikedata2$Pressure<-na.approx(bikedata2$Pressure)

bikedata2$Precip.<-na.approx(bikedata2$Precip.)
bikedata2$Precip.[bikedata2$Precip.>0]<-1
bikedata2$Precip.=as.factor(bikedata2$Precip.)

#fill NA by value of last row
bikedata2$Condition<-na.locf(bikedata2$Condition)
bikedata2$Condition=factor(bikedata2$Condition,levels = c("1", "2", "3","4"),ordered = TRUE)

colnames(bikedata2)[49]<-"WindSpeed"
# summary for time and date related variables
summary(bikedata[,1:46])
# summary for weather related variables
summary(bikedata[,47:52])


## split tain and test dataset (4 weeks data trained to predict next 2 days)
bikedata$date<-substr(bikedata$newtime,1,10)
bikedata$ddiff <- as.Date(bikedata$date, format="%Y-%m-%d")-
        as.Date("2017-01-01", format="%Y-%m-%d")+1
bikedata<-bikedata[,-53]

#time series forcasting
library(forecast)
## subscribed user part
#------------------------------------------
library(randomForest)
#install.packages("rpart",repos = "http://cran.r-project.org", dependencies = c("Depends", "Imports", "Suggests")) 
library(rpart)
#install.packages("caret",repos = "http://cran.r-project.org", dependencies = c("Depends", "Imports", "Suggests"))
library(caret) # for parameters turning
#install.packages("rpart.plot")
library(rpart.plot)

# 10-fold cross-validation for random forest
mtry_list<-seq(1, 31, by = 3)
ntree_list<-c(100,500,1000,1500)
rf_error<-c()
param_mtry<-c()
param_ntree<-c()
for (i in 1:10){
        ml_train<-bikedata[bikedata$ddiff %in% (((i-1)*30)+1):(i*30-2),c(-1,-53)]
        ml_xtest<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),c(-1,-2,-53)]
        ml_ytest<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),2]
        
        ts_train<-bikedata[bikedata$ddiff %in% (((i-1)*30)+1):(i*30-2),c(1,2)]
        ts_train$count<-ts_train$count+1
        
        #decomposition part
        rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
        decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
        
        # extract and predict trend
        trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
        trend_fit <- auto.arima(trend_part)
        trend_for <- forecast(trend_fit, 48)$mean
        
        #with trend
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        print(i)
        for (mt in mtry_list){
                for (n in ntree_list){
                        fit_sub <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=n,mtry=mt)
                        ml_pred<-exp(predict(fit_sub,ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
                        rf_error<-c(rf_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
                        param_mtry<-c(param_mtry,mt)
                        param_ntree<-c(param_ntree,n)
                        
                }
        }
}
df <- data.frame(mtry=param_mtry, error=rf_error, ntree=as.character(param_ntree))
# find the best mtry=31, ntree=1000,mean_rmse=7.09
sub_error_rf<-df %>%
        group_by(mtry,ntree) %>%
        summarize(mean_rmse = mean(error, na.rm = TRUE))
sub_error_rf[sub_error_rf$mean_rmse==min(sub_error_rf$mean_rmse),]
#plot
ggplot(sub_error_rf, aes(x =mtry, y = mean_rmse, colour =ntree)) + geom_line()

#with trend forecast
# 10-fold cross-validation for SVM
library(e1071)
epsilon_list<-seq(0,1,0.1)
cost_list<-c(2^(2:9))
svm_error<-c()
param_epsilon<-c()
param_cost<-c()
for (i in 1:10){ 
        ml_train<-bikedata[bikedata$ddiff %in% (((i-1)*30)+1):(i*30-2),c(-1,-53)]
        ml_xtest<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),c(-1,-2,-53)]
        ml_ytest<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),2]
        
        ts_train<-bikedata[bikedata$ddiff %in% (((i-1)*30)+1):(i*30-2),c(1,2)]
        ts_train$count<-ts_train$count+1
        
        #decomposition part
        rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
        decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
        
        # extract and predict trend
        trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
        trend_fit <- auto.arima(trend_part)
        trend_for <- forecast(trend_fit, 48)$mean
        
        # with trend
        ml_train<-bikedata[bikedata$ddiff %in% ((i-1)*30+1):(i*30-2),c(-1,-53)]
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        ml_train$Temperature<-scale(ml_train$Temperature)
        ml_train$Humidity <-scale(ml_train$Humidity)
        ml_train$WindSpeed<-scale(ml_train$WindSpeed)
        ml_train$Pressure<-scale(ml_train$Pressure)
        print(i)
        for (e in epsilon_list){
                for (c in cost_list){
                        fit_sub<-svm(count~., data=ml_train,epsilon=e,cost=c)
                        ml_pred <- exp(predict(fit_sub, ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
                        svm_error<-c(svm_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
                        param_epsilon<-c(param_epsilon,e)
                        param_cost<-c(param_cost,c)
                }
        }
}
df <- data.frame(epsilon=param_epsilon, error=svm_error, cost=as.character(param_cost))
# find the best epsilon=0.1, cost=8,mean_rmse=14.4
sub_error_svm<-df %>%
        group_by(epsilon,cost) %>%
        summarize(mean_rmse = mean(error, na.rm = TRUE))
sub_error_svm[sub_error_svm$mean_rmse==min(sub_error_svm$mean_rmse),]
#plot
ggplot(sub_error_svm, aes(x =epsilon, y = mean_rmse, colour =cost)) + geom_line()

# i=22 select 2018-9-23 2018-10-20 to predict 10-21 and 10-22 for illustration
ml_train<-bikedata[bikedata$ddiff %in% 631:658,c(-1,-53)]
ml_xtest<-bikedata[bikedata$ddiff %in% c(658+1,658+2),c(-1,-2,-53)]
ml_ytest<-bikedata[bikedata$ddiff %in% c(658+1,658+2),2]

ts_train<-bikedata[bikedata$ddiff %in% 631:658,c(1,2)]
ts_train$count<-ts_train$count+1
ts_test<-bikedata[bikedata$ddiff %in% c(658+1,658+2),c(1,2)]
#time series forcasting
## subscribed user part
#------------------------------------------
#mstl part
rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
decompo_stl <- data.table(Usage = as.numeric(decompo),
                          Date = rep(ts_train$newtime, ncol(decompo)),
                          Type = factor(rep(c("original data", colnames(decompo)[-1]),
                                            each = nrow(decompo)),
                                        levels = c("original data", colnames(decompo)[-1])))
ggplot(decompo_stl, aes(x = Date, y = Usage)) +
        geom_line() + 
        facet_grid(Type ~ ., scales = "free_y", switch = "y") +
        labs(x = "Date", y = NULL,
             title = "Time Series Decomposition by STL")
stlm_model<-stlf(rental_sub,lambda=0,method="ets",h=48)
plot(stlm_model)
stlm_pred<-stlm_model$mean-1
mean(sqrt((stlm_pred-ts_test$count)^2/length(stlm_pred)))

# tbats part
rental_sub2<-msts(log(ts_train$count),seasonal.period=c(24,7*24))
decompo_tbats<-tbats(rental_sub2,use.box.cox = FALSE, 
                     use.trend = TRUE, 
                     use.damped.trend = TRUE)
tbats_model<-forecast(rental_sub2,h=48)
plot(tbats_model)
tbats_pred<-exp(tbats_model$mean)-1
mean(sqrt((tbats_pred-ts_test$count)^2/length(tbats_pred)))
# mstl and tbats has same result

# extract and predict trend
trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
trend_fit <- auto.arima(trend_part)
trend_for <- forecast(trend_fit, 48)$mean
trend_data <- data.table(Usage = c(decompo[,2], trend_for),
                         Time = c(ts_train$newtime, ts_test$newtime),
                         Type = c(rep("Real", nrow(ts_train)), rep("Forecast",nrow(ts_test))))
ggplot(trend_data, aes(Time, Usage, color = Type)) +
        geom_line(size = 1.2) +
        labs(title = paste(trend_fit))

# mL algorithm part---------------------------------
#with trend
ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
# randomforest
# grid search for better parameters
# customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), label = c("mtry", "ntree"))
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#         randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#         predict(modelFit, newdata)
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#         predict(modelFit, newdata, type = "prob")
# customRF$sort <- function(x) x[order(x[,1]),]
# control <- trainControl(method="cv", number=10, search="grid")
# set.seed(100)
# tunegrid <- expand.grid(.mtry = seq(1, 31, by = 3),.ntree = c(100,500,1000,1500))
# rf_gridsearch <- train(count~., data=ml_train, method=customRF, metric="RMSE", tuneGrid=tunegrid, trControl=control)
# rf_gridsearch
# plot(rf_gridsearch)

# random forest model (of best model parameters) with trend forecast
fit_sub <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=1000,mtry=31)
rf_pred<-exp(predict(fit_sub,ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
mean(sqrt(unlist((as.numeric(rf_pred)-ml_ytest)^2/length(rf_pred))))
# show training error
plot(fit_sub)
varImpPlot(fit_sub,type=2)
# to visualize tree
tree_1 <- rpart(count ~ ., data = ml_train)
rpart.plot(tree_1, cex=0.75,digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")

# without trend
ml_train<-bikedata[bikedata$ddiff %in% 631:658,c(-1,-53)]
ml_train$count<-as.double(log(ml_train$count+1))
fit_sub_0 <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=1000,mtry=31)
rf_pred_0<-exp(predict(fit_sub,ml_xtest))-1
mean(sqrt(unlist((as.numeric(rf_pred_0)-ml_ytest)^2/length(rf_pred_0))))
varImpPlot(fit_sub_0,type=2)
# to visualize tree
tree_1 <- rpart(count ~ ., data = ml_train)
rpart.plot(tree_1, cex=0.75,digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")

#svm part
#without trend forecast
ml_train$Temperature<-scale(ml_train$Temperature)
ml_train$Humidity <-scale(ml_train$Humidity)
ml_train$WindSpeed<-scale(ml_train$WindSpeed)
ml_train$Pressure<-scale(ml_train$Pressure)

ml_xtest$Temperature<-scale(ml_xtest$Temperature)
ml_xtest$Humidity <-scale(ml_xtest$Humidity)
ml_xtest$WindSpeed<-scale(ml_xtest$WindSpeed)
ml_xtest$Pressure<-scale(ml_xtest$Pressure)

fit_sub_svm<-svm(count~., data=ml_train,epsilon=0.1,cost=8)
svm_pred_0<- exp(predict(fit_sub_svm, ml_xtest))-1
mean(sqrt(unlist((as.numeric(svm_pred_0)-ml_ytest)^2/length(svm_pred_0))))
# visualization
library(plotly)
Humidity = ml_xtest$Humidity
Temperature=ml_xtest$Temperature
plot_ly(x=as.vector(Humidity),y=as.vector(Temperature),z=unlist(ml_ytest), type="scatter3d", mode="markers", name = "Obs", marker = list(size = 3)) %>%
        add_trace(x=as.vector(Humidity),y=as.vector(Temperature),z=svm_pred_0, type = "mesh3d", name = "Preds")

# with trend forecast
ml_train<-bikedata[bikedata$ddiff %in% 631:658,c(-1,-53)]
ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
ml_train$Temperature<-scale(ml_train$Temperature)
ml_train$Humidity <-scale(ml_train$Humidity)
ml_train$WindSpeed<-scale(ml_train$WindSpeed)
ml_train$Pressure<-scale(ml_train$Pressure)

fit_sub_svm<-svm(count~., data=ml_train,epsilon=0.1,cost=8)
svm_pred <- exp(predict(fit_sub_svm, ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
mean(sqrt(unlist((as.numeric(svm_pred)-ml_ytest)^2/length(svm_pred))))
# visualization of svm model versus humidity
ggplot() +
        geom_point(aes(x = ml_xtest$Humidity, y = unlist(ml_ytest)), colour = "red") +
        geom_line(aes(x = ml_xtest$Humidity , y = svm_pred_0), colour = "blue")+
        xlab("scaled humidity") +
        ylab("hourly bike flow count")
# visulization of comparision of time series forecasts
datas <- data.table(Usage = c(ts_test$count,stlm_pred,rf_pred,rf_pred_0,svm_pred,svm_pred_0),
                    Time = rep(1:length(ts_test$newtime), 6),
                    Type = rep(c("Real", "stlm","rf","rf_no_trend","svm","svm_no_trend"), each = length(ts_test$count)))
ggplot(datas, aes(Time, Usage, color = Type)) +
        geom_line(size = 0.8, alpha = 0.75) +
        labs(y = "Usage", title = "Comparison of forecasts for subscribed user's bike usage")

## loop to see  model performance in whole period
## split tain and test dataset (4 weeks data trained to predict next 2 days)
stlm_error<-c()
tbats_error<-c()
rf_error<-c()
rf0_error<-c()
svm_error<-c()
svm0_error<-c()
for (i in 11:27) {
        ml_train<-bikedata[bikedata$ddiff %in% (((i-1)*30)+1):(i*30-2),c(-1,-53)]
        ml_xtest<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),c(-1,-2,-53)]
        ml_ytest<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),2]
        
        ts_train<-bikedata[bikedata$ddiff %in% (((i-1)*30)+1):(i*30-2),c(1,2)]
        ts_train$count<-ts_train$count+1
        ts_test<-bikedata[bikedata$ddiff %in% c(i*30-1,i*30),c(1,2)]
        #time series forcasting
        ## subscribed user part
        #------------------------------------------
        #mstl part
        rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
        decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
        stlm_model<-stlf(rental_sub,lambda=0,method="ets",h=48)
        stlm_pred<-stlm_model$mean-1
        stlm_error<-c(stlm_error,mean(sqrt((stlm_pred-ts_test$count)^2/length(stlm_pred))))
        
        # tbats part
        rental_sub2<-msts(log(ts_train$count),seasonal.period=c(24,7*24))
        decompo_tbats<-tbats(rental_sub2,use.box.cox = FALSE, use.trend = TRUE, use.damped.trend = TRUE)
        tbats_model<-forecast(rental_sub2,h=48)
        tbats_pred<-exp(tbats_model$mean)-1
        tbats_error<-c(tbats_error,mean(sqrt((tbats_pred-ts_test$count)^2/length(tbats_pred))))
        
        # extract and predict trend
        trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
        trend_fit <- auto.arima(trend_part)
        trend_for <- forecast(trend_fit, 48)$mean
        
        # mL algorithm part---------------------------------
        #random forest
        #with trend
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        fit_sub <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=1000,mtry=31)
        ml_pred<-exp(predict(fit_sub,ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
        rf_error<-c(rf_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
        # without trend
        ml_train<-bikedata[bikedata$ddiff %in% ((i-1)*30+1):(i*30-2),c(-1,-53)]
        ml_train$count<-as.double(log(ml_train$count+1))
        fit_sub_0 <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=1000,mtry=31)
        ml_pred_0<-exp(predict(fit_sub,ml_xtest))-1
        rf0_error<-c(rf0_error,mean(sqrt(unlist((as.numeric(ml_pred_0)-ml_ytest)^2/length(ml_pred_0)))))
        
        # svm
        # without trend
        ml_train$Temperature<-scale(ml_train$Temperature)
        ml_train$Humidity <-scale(ml_train$Humidity)
        ml_train$WindSpeed<-scale(ml_train$WindSpeed)
        ml_train$Pressure<-scale(ml_train$Pressure)
        
        ml_xtest$Temperature<-scale(ml_xtest$Temperature)
        ml_xtest$Humidity <-scale(ml_xtest$Humidity)
        ml_xtest$WindSpeed<-scale(ml_xtest$WindSpeed)
        ml_xtest$Pressure<-scale(ml_xtest$Pressure)
        fit_sub_0<-svm(count~., data=ml_train,epsilon=0.1,cost=8)
        ml_pred_0 <- exp(predict(fit_sub_0, ml_xtest))-1
        svm0_error<-c(svm0_error,mean(sqrt(unlist((as.numeric(ml_pred_0)-ml_ytest)^2/length(ml_pred_0)))))
        # with trend
        ml_train<-bikedata[bikedata$ddiff %in% ((i-1)*30+1):(i*30-2),c(-1,-53)]
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        ml_train$Temperature<-scale(ml_train$Temperature)
        ml_train$Humidity <-scale(ml_train$Humidity)
        ml_train$WindSpeed<-scale(ml_train$WindSpeed)
        ml_train$Pressure<-scale(ml_train$Pressure)
        fit_sub<-svm(count~., data=ml_train,epsilon=0.1,cost=8)
        ml_pred <- exp(predict(fit_sub, ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
        svm_error<-c(svm_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
}
mean_error1<-as.data.frame(cbind(stlm_error,tbats_error,rf_error,rf0_error,svm_error,svm0_error))
colMeans(mean_error1)
apply(mean_error1,2,sd)
boxplot(x = as.list(mean_error1),col=rainbow(length(unique(as.list(mean_error1)))))

# ## casual user part ------------------------
## split tain and test dataset (4 weeks data trained to predict next 2 days)
bikedata2$date<-substr(bikedata2$newtime,1,10)
bikedata2$ddiff <- as.Date(bikedata2$date, format="%Y-%m-%d")-
        as.Date("2017-01-01", format="%Y-%m-%d")+1
bikedata2<-bikedata2[,-53]
# turning parameters
# 10-fold cross-validation for random forest
mtry_list<-seq(1, 31, by = 3)
ntree_list<-c(100,500,1000,1500)
rf_error<-c()
param_mtry<-c()
param_ntree<-c()
for (i in 1:10){
        ml_train<-bikedata2[bikedata2$ddiff %in% (((i-1)*30)+1):(i*30-2),c(-1,-53)]
        ml_xtest<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),c(-1,-2,-53)]
        ml_ytest<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),2]
        ts_train<-bikedata2[bikedata2$ddiff %in% (((i-1)*30)+1):(i*30-2),c(1,2)]
        ts_train$count<-ts_train$count+1
        #decomposition part
        rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
        decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
        # extract and predict trend
        trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
        trend_fit <- auto.arima(trend_part)
        trend_for <- forecast(trend_fit, 48)$mean
        #with trend
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        for (mt in mtry_list){
                for (n in ntree_list){
                        fit_sub <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=n,mtry=mt)
                        ml_pred<-exp(predict(fit_sub,ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
                        rf_error<-c(rf_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
                        param_mtry<-c(param_mtry,mt)
                        param_ntree<-c(param_ntree,n)
                }
        }
}
df <- data.frame(mtry=param_mtry, error=rf_error, ntree=as.character(param_ntree))
# find the best mtry=28, ntree=100, mean_rmse=2.08
cus_error_rf<-df %>%
        group_by(mtry,ntree) %>%
        summarize(mean_rmse = mean(error, na.rm = TRUE))
cus_error_rf[cus_error_rf$mean_rmse==min(cus_error_rf$mean_rmse),]
#plot
ggplot(cus_error_rf, aes(x =mtry, y = mean_rmse, colour =ntree)) + geom_line()

# 10-fold cross-validation
epsilon_list<-seq(0,1,0.1)
cost_list<-2^(2:9)
svm_error<-c()
param_epsilon<-c()
param_cost<-c()
for (i in 1:10){
        ml_train<-bikedata2[bikedata2$ddiff %in% (((i-1)*30)+1):(i*30-2),c(-1,-53)]
        ml_xtest<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),c(-1,-2,-53)]
        ml_ytest<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),2]
        ts_train<-bikedata2[bikedata2$ddiff %in% (((i-1)*30)+1):(i*30-2),c(1,2)]
        ts_train$count<-ts_train$count+1
        #decomposition part
        rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
        decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
        # extract and predict trend
        trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
        trend_fit <- auto.arima(trend_part)
        trend_for <- forecast(trend_fit, 48)$mean
        # with trend
        ml_train<-bikedata2[bikedata2$ddiff %in% ((i-1)*30+1):(i*30-2),c(-1,-53)]
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        ml_train$Temperature<-scale(ml_train$Temperature)
        ml_train$Humidity <-scale(ml_train$Humidity)
        ml_train$WindSpeed<-scale(ml_train$WindSpeed)
        ml_train$Pressure<-scale(ml_train$Pressure)
        for (e in epsilon_list){
                for (c in cost_list){
                        fit_sub<-svm(count~., data=ml_train,epsilon=e,cost=c)
                        ml_pred <- exp(predict(fit_sub, ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
                        svm_error<-c(svm_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
                        param_epsilon<-c(param_epsilon,e)
                        param_cost<-c(param_cost,c)
                }
        }
}
df <- data.frame(epsilon=param_epsilon, error=svm_error, cost=as.character(param_cost))
# find the best epsilon=0.8, cost=4, mean_rmse=3.53
cus_error_svm<-df %>%
        group_by(epsilon,cost) %>%
        summarize(mean_rmse = mean(error, na.rm = TRUE))
cus_error_svm[cus_error_svm$mean_rmse==min(cus_error_svm$mean_rmse),]
#plot
ggplot(cus_error_svm, aes(x =epsilon, y = mean_rmse, colour =cost)) + geom_line()

# i=22 select 2018-9-23 2018-10-20 to predict 10-21 and 10-22
ml_train<-bikedata2[bikedata2$ddiff %in% 631:658,c(-1,-53)]
ml_xtest<-bikedata2[bikedata2$ddiff %in% c(658+1,658+2),c(-1,-2,-53)]
ml_ytest<-bikedata2[bikedata2$ddiff %in% c(658+1,658+2),2]

ts_train<-bikedata2[bikedata2$ddiff %in% 631:658,c(1,2)]
ts_train$count<-ts_train$count+1
ts_test<-bikedata2[bikedata2$ddiff %in% c(658+1,658+2),c(1,2)]

#time series forcasting
## casual user part
#------------------------------------------
#mstl part
rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
decompo_stl <- data.table(Usage = as.numeric(decompo),
                          Date = rep(ts_train$newtime, ncol(decompo)),
                          Type = factor(rep(c("original data", colnames(decompo)[-1]),
                                            each = nrow(decompo)),
                                        levels = c("original data", colnames(decompo)[-1])))
ggplot(decompo_stl, aes(x = Date, y = Usage)) +
        geom_line() + 
        facet_grid(Type ~ ., scales = "free_y", switch = "y") +
        labs(x = "Date", y = NULL,
             title = "Time Series Decomposition by STL")
stlm_model<-stlf(rental_sub,lambda=0,method="ets",h=48)
plot(stlm_model)
stlm_pred<-stlm_model$mean-1
mean(sqrt((stlm_pred-ts_test$count)^2/length(stlm_pred)))

# tbats part
rental_sub2<-msts(log(ts_train$count),seasonal.period=c(24,7*24))
decompo_tbats<-tbats(rental_sub2,use.box.cox = FALSE, 
                     use.trend = TRUE, 
                     use.damped.trend = TRUE)
tbats_model<-forecast(rental_sub2,h=48)
plot(tbats_model)
tbats_pred<-exp(tbats_model$mean)-1
mean(sqrt((tbats_pred-ts_test$count)^2/length(tbats_pred)))
# mstl and tbats has same result

# extract and predict trend
trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
trend_fit <- auto.arima(trend_part)
trend_for <- forecast(trend_fit, 48)$mean
trend_data <- data.table(Usage = c(decompo[,2], trend_for),
                         Time = c(ts_train$newtime, ts_test$newtime),
                         Type = c(rep("Real", nrow(ts_train)), rep("Forecast",nrow(ts_test))))
ggplot(trend_data, aes(Time, Usage, color = Type)) +
        geom_line(size = 1.2) +
        labs(title = paste(trend_fit))

# mL algorithm part---------------------------------
#with trend
ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
# randomforest
# # grid search for better parameters
# customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), label = c("mtry", "ntree"))
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#         randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#         predict(modelFit, newdata)
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#         predict(modelFit, newdata, type = "prob")
# customRF$sort <- function(x) x[order(x[,1]),]
# control <- trainControl(method="cv", number=10, search="grid")
# set.seed(100)
# tunegrid <- expand.grid(.mtry = seq(1, 31, by = 3),.ntree = c(100,500,1000,1500))
# rf_gridsearch <- train(count~., data=ml_train, method=customRF, metric="RMSE", tuneGrid=tunegrid, trControl=control)
# rf_gridsearch
# plot(rf_gridsearch)

# random forest model (of best model parameters) with trend forecast
fit_sub <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=100,mtry=28)
rf_pred<-exp(predict(fit_sub,ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
mean(sqrt(unlist((as.numeric(rf_pred)-ml_ytest)^2/length(rf_pred))))
# show training error
plot(fit_sub)
varImpPlot(fit_sub,type=2)
# to visualize tree
tree_1 <- rpart(count ~ ., data = ml_train)
rpart.plot(tree_1, cex=0.75,digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")

# without trend
ml_train<-bikedata2[bikedata2$ddiff %in% 631:658,c(-1,-53)]
ml_train$count<-as.double(log(ml_train$count+1))
fit_sub_0 <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=100,mtry=28)
rf_pred_0<-exp(predict(fit_sub,ml_xtest))-1
mean(sqrt(unlist((as.numeric(rf_pred_0)-ml_ytest)^2/length(rf_pred_0))))
varImpPlot(fit_sub_0,type=2)
# to visualize tree
tree_1 <- rpart(count ~ ., data = ml_train)
rpart.plot(tree_1, cex=0.75,digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")

#svm part
#without trend forecast
ml_train$Temperature<-scale(ml_train$Temperature)
ml_train$Humidity <-scale(ml_train$Humidity)
ml_train$WindSpeed<-scale(ml_train$WindSpeed)
ml_train$Pressure<-scale(ml_train$Pressure)

ml_xtest$Temperature<-scale(ml_xtest$Temperature)
ml_xtest$Humidity <-scale(ml_xtest$Humidity)
ml_xtest$WindSpeed<-scale(ml_xtest$WindSpeed)
ml_xtest$Pressure<-scale(ml_xtest$Pressure)

fit_sub_svm<-svm(count~., data=ml_train,epsilon=0.8,cost=4)
svm_pred_0<- exp(predict(fit_sub_svm, ml_xtest))-1
mean(sqrt(unlist((as.numeric(svm_pred_0)-ml_ytest)^2/length(svm_pred_0))))
# 3d visualization
Humidity = ml_xtest$Humidity
Temperature=ml_xtest$Temperature
plot_ly(x=as.vector(Humidity),y=as.vector(Temperature),z=unlist(ml_ytest), type="scatter3d", mode="markers", name = "Obs", marker = list(size = 3)) %>%
        add_trace(x=as.vector(Humidity),y=as.vector(Temperature),z=svm_pred_0, type = "mesh3d", name = "Preds")

#with trend forecast
ml_train<-bikedata2[bikedata2$ddiff %in% 631:658,c(-1,-53)]
ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
ml_train$Temperature<-scale(ml_train$Temperature)
ml_train$Humidity <-scale(ml_train$Humidity)
ml_train$WindSpeed<-scale(ml_train$WindSpeed)
ml_train$Pressure<-scale(ml_train$Pressure)

fit_sub_svm<-svm(count~., data=ml_train,epsilon=0.8,cost=4 )
svm_pred <- exp(predict(fit_sub_svm, ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
mean(sqrt(unlist((as.numeric(svm_pred)-ml_ytest)^2/length(svm_pred))))
# visualization of svm model versus humidity
ggplot() +
        geom_point(aes(x = ml_xtest$Humidity, y = unlist(ml_ytest)), colour = "red") +
        geom_line(aes(x = ml_xtest$Humidity , y = svm_pred), colour = "blue")+
        xlab("scaled humidity") +
        ylab("hourly bike flow count")

# visulization of comparision of time series forecasts
datas <- data.table(Usage = c(ts_test$count,stlm_pred,rf_pred,rf_pred_0,svm_pred,svm_pred_0),
                    Time = rep(1:length(ts_test$newtime), 6),
                    Type = rep(c("Real", "stlm","rf","rf_no_trend","svm","svm_no_trend"), each = length(ts_test$count)))
ggplot(datas, aes(Time, Usage, color = Type)) +
        geom_line(size = 0.8, alpha = 0.75) +
        labs(y = "Usage", title = "Comparison of forecasts for casual user's bike usage")

## loop to see  model performance in whole period
## split tain and test dataset (4 weeks data trained to predict next 2 days)
stlm_error<-c()
tbats_error<-c()
rf_error<-c()
rf0_error<-c()
svm_error<-c()
svm0_error<-c()
for (i in 11:27) {
        ml_train<-bikedata2[bikedata2$ddiff %in% (((i-1)*30)+1):(i*30-2),c(-1,-53)]
        ml_xtest<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),c(-1,-2,-53)]
        ml_ytest<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),2]
        
        ts_train<-bikedata2[bikedata2$ddiff %in% (((i-1)*30)+1):(i*30-2),c(1,2)]
        ts_train$count<-ts_train$count+1
        ts_test<-bikedata2[bikedata2$ddiff %in% c(i*30-1,i*30),c(1,2)]
        
        #time series forcasting
        ## casual user part
        #------------------------------------------
        #mstl part
        rental_sub<-msts(ts_train$count,seasonal.period=c(24,7*24))
        decompo<-mstl(rental_sub,s.window = "periodic", robust = TRUE)
        stlm_model<-stlf(rental_sub,lambda=0,method="ets",h=48)
        stlm_pred<-stlm_model$mean-1
        stlm_error<-c(stlm_error,mean(sqrt((stlm_pred-ts_test$count)^2/length(stlm_pred))))
        
        # tbats part
        rental_sub2<-msts(log(ts_train$count),seasonal.period=c(24,7*24))
        decompo_tbats<-tbats(rental_sub2,use.box.cox = FALSE, use.trend = TRUE, use.damped.trend = TRUE)
        tbats_model<-forecast(rental_sub2,h=48)
        tbats_pred<-exp(tbats_model$mean)-1
        tbats_error<-c(tbats_error,mean(sqrt((tbats_pred-ts_test$count)^2/length(tbats_pred))))
        
        # extract and predict trend
        trend_part <- msts(decompo[,2],seasonal.period=c(24,7*24))
        trend_fit <- auto.arima(trend_part)
        trend_for <- forecast(trend_fit, 48)$mean
        
        # mL algorithm part---------------------------------
        #random forest
        #with trend
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        fit_sub <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=100,mtry=28)
        ml_pred<-exp(predict(fit_sub,ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
        rf_error<-c(rf_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
        # without trend
        ml_train<-bikedata2[bikedata2$ddiff %in% ((i-1)*30+1):(i*30-2),c(-1,-53)]
        ml_train$count<-as.double(log(ml_train$count+1))
        fit_sub_0 <- randomForest(count ~ ., data=ml_train,importance=TRUE, ntree=100,mtry=28)
        ml_pred_0<-exp(predict(fit_sub,ml_xtest))-1
        rf0_error<-c(rf0_error,mean(sqrt(unlist((as.numeric(ml_pred_0)-ml_ytest)^2/length(ml_pred_0)))))
        
        # svm
        # without trend
        ml_train$Temperature<-scale(ml_train$Temperature)
        ml_train$Humidity <-scale(ml_train$Humidity)
        ml_train$WindSpeed<-scale(ml_train$WindSpeed)
        ml_train$Pressure<-scale(ml_train$Pressure)
        
        ml_xtest$Temperature<-scale(ml_xtest$Temperature)
        ml_xtest$Humidity <-scale(ml_xtest$Humidity)
        ml_xtest$WindSpeed<-scale(ml_xtest$WindSpeed)
        ml_xtest$Pressure<-scale(ml_xtest$Pressure)
        fit_sub_0<-svm(count~., data=ml_train,epsilon=0.8,cost=4)
        ml_pred_0 <- exp(predict(fit_sub_0, ml_xtest))-1
        svm0_error<-c(svm0_error,mean(sqrt(unlist((as.numeric(ml_pred_0)-ml_ytest)^2/length(ml_pred_0)))))
        # with trend
        ml_train<-bikedata2[bikedata2$ddiff %in% ((i-1)*30+1):(i*30-2),c(-1,-53)]
        ml_train$count<-as.double(log(ml_train$count-decompo[,2]-min(ml_train$count-decompo[,2])+1))
        ml_train$Temperature<-scale(ml_train$Temperature)
        ml_train$Humidity <-scale(ml_train$Humidity)
        ml_train$WindSpeed<-scale(ml_train$WindSpeed)
        ml_train$Pressure<-scale(ml_train$Pressure)
        fit_sub<-svm(count~., data=ml_train,epsilon=0.8,cost=4)
        ml_pred <- exp(predict(fit_sub, ml_xtest))-1+min(ml_train$count-decompo[,2]) +trend_for
        svm_error<-c(svm_error,mean(sqrt(unlist((as.numeric(ml_pred)-ml_ytest)^2/length(ml_pred)))))
}
mean_error2<-as.data.frame(cbind(stlm_error,tbats_error,rf_error,rf0_error,svm_error,svm0_error))
colMeans(mean_error2)
apply(mean_error2,2,sd)
boxplot(x = as.list(mean_error2),col=rainbow(length(unique(as.list(mean_error2)))))