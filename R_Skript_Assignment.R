



## Exploratory Data Analysis - Final Programming Assignment

setwd("C:/Users/Tobias/Documents/coursera/Exploratory Data Analysis/Explo_D_a")
#setwd("O:/R\Coursera/Reproducible Research/Course Project 1")

# Define necessary packages
packages<- c("dplyr", "openxlsx", "ggplot2","xlsx", "lubridate","tidyr")

# Load selected packages
lapply(packages, require, character.only = TRUE)




# Initial Load of Data


download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
              ,destfile = "summarySCC_PM25.zip")

unzip(zipfile = "summarySCC_PM25.zip")

NEI <- readRDS("summarySCC_PM25.rds")


# fact table
# file contains data frame with all PM2.5 emissions for 1999,2992,2995 and 2008.
# number of tons of PM2.5 per year from a source

# fips: five digit number for the US county

# SCC: the name of the source - see mapping in SCC dataset

# Pollutant: the pollutant always is PM2.5

# type: The type of source

# year: year of the emission



SCC <- readRDS("Source_Classification_Code.rds")
# Source classification code table
# provides mapping from scc digit strings
# its up on you to choose the detail level to explore 



#Questions
#You must address the following questions and tasks in your exploratory analysis. 
#For each question/task you will need to make a single plot. 
#Unless specified, you can use any plotting system in R to make your plot.

# 1.
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources
#for each of the years 1999, 2002, 2005, and 2008.



YearAgg<-NEI%>%group_by(year,Pollutant)%>%summarise(sumPoll = sum(Emissions))

png('plot1.jpg')
plot(x = YearAgg$year ,y = YearAgg$sumPoll, main = "PM 2.5 Pollution", xlab = "Year", ylab = "Pollution"
     , col = "darkblue", lwd = 2, pch = 1)
dev.off()

lm(data = YearAgg,formula = sumPoll~year)
# Negative Coefficient for year --> negative trend as shown in base plot



# 2.
#Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

BaltimoreAgg<-NEI%>%filter(fips == 24510)%>%group_by(year,Pollutant)%>%summarise(sumPoll = sum(Emissions))

png('plot2.png')
plot(x = BaltimoreAgg$year ,y = BaltimoreAgg$sumPoll, main = "Baltimore PM 2.5 Pollution", xlab = "Year", ylab = "Pollution"
     , col = "darkblue", lwd = 2, pch = 1)
dev.off()

lm(data = BaltimoreAgg,formula = sumPoll~year)
# negative general trend


# 3. Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 

BaltimoreAgg_2<-NEI%>%filter(fips == 24510)%>%group_by(year,Pollutant,type)%>%summarise(sumPoll = sum(Emissions))

png('plot3.jpg')

ggplot(data = BaltimoreAgg_2,aes(x = year, y= sumPoll, color = type))+
  geom_line()+
  theme(axis.title.y = element_blank()
        ,axis.text.x = element_text(angle=90, size=10, face="bold")
        ,axis.text.y = element_text(size= 10, face= "bold")
        ,title = element_text(size=25)
        ,legend.text = element_text(size= 15))+
  ggtitle("Baltimore Pollution per year by Type")+ xlab("Year")
dev.off()


BaltimoreAgg_3<-BaltimoreAgg_2%>%filter(year %in% c(1999, 2008))
ggplot(data = BaltimoreAgg_3, aes(x = year, y = sumPoll, color = type))+
  geom_bar(stat = "identity", position = "dodge")

ggplot(data = BaltimoreAgg_3, aes(x = type, y = sumPoll, color = as.factor(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.title.y = element_blank()
        ,axis.text.x = element_text(angle=90, size=10, face="bold")
        ,axis.text.y = element_text(size= 10, face= "bold")
        ,title = element_text(size=25)
        ,legend.text = element_text(size= 15))+
  ggtitle("Baltimore Pollution per year by Type")+ xlab("Year")

# --> Decreaes have been seen in the NON-Road, NON-Point and On-road category. 

# 4.
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

ggplot(data = BaltimoreAgg_3, aes(x = type, y = sumPoll, color = as.factor(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle=90, size=10, face="bold")
        ,axis.text.y = element_text(size= 10, face= "bold")
        ,title = element_text(size=25)
        ,legend.text = element_text(size= 15))+
  ggtitle("Baltimore Pollution per year by Type")+ xlab("Year")+ylab("Amount of Pollution")

# -->Increases have been seen in the PONT-Category.


# 5. 
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?



joined_Rawdata<-left_join(x = NEI,y = SCC, by = c("SCC"))

head(joined_Rawdata)

names(joined_Rawdata)

USA_coal<-filter(joined_Rawdata, grepl("coal",SCC.Level.Three))%>%group_by(year,type)%>%summarise(sumPoll = sum(Emissions))

ggplot(data = USA_coal, aes(x = type, y = sumPoll, color = as.character(year)))+
  geom_bar(stat = "identity", position = "dodge", fill = TRUE)+
  theme(axis.text.x = element_text(angle=90, size=10, face="bold")
        ,axis.text.y = element_text(size= 10, face= "bold")
        ,title = element_text(size=25)
        ,legend.text = element_text(size= 15))+
  ggtitle("USA Coal Combustion Pollution per year by Type")+ xlab("Year")+ylab("Amount of Pollution")



# 6.
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#  Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
# in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips=="06037").

Baltimore_motor_vehicle<-joined_Rawdata%>%filter(fips == 24510)%>%filter(grepl("Motor", SCC.Level.Three ))

Baltimore_mv_agg<-Baltimore_motor_vehicle%>%group_by(fips,year,type)%>%summarise(sumPoll = sum(Emissions))

ggplot(data = Baltimore_mv_agg, aes(x = type, y = sumPoll, color = as.character(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle=90, size=10, face="bold")
        ,axis.text.y = element_text(size= 10, face= "bold")
        ,title = element_text(size=25)
        ,legend.text = element_text(size= 15))+
  ggtitle("Baltimore Motor vehicle  Pollution per year by Type")+ xlab("Year")+ylab("Amount of Pollution")

# Nonpoint emissions have stayed constant. On-road have decreased at first and than increased in the last time span.

LAC_motor_vehicle<-joined_Rawdata%>%filter(fips == "06037")%>%filter(grepl("Motor", SCC.Level.Three ))

LAC_mv_agg<-LAC_motor_vehicle%>%group_by(fips,year,type)%>%summarise(sumPoll = sum(Emissions))

Compared_emissions<-rbind(Baltimore_mv_agg, LAC_mv_agg)

ggplot(data = Compared_emissions, aes(x = fips, y = sumPoll, color = as.character(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle=90, size=10, face="bold")
        ,axis.text.y = element_text(size= 10, face= "bold")
        ,title = element_text(size=25)
        ,legend.text = element_text(size= 15))+
  ggtitle("Baltimore Motor vehicle  Pollution per year by Type")+ xlab("Year")+ylab("Amount of Pollution")



# 7. Which city has seen greater changes over time in motor vehicle emissions?

# Baltimore has seen greater changes over time in both relative and absolute values but remains at around 
# 1/3 of the pollution level of Los Angeles





