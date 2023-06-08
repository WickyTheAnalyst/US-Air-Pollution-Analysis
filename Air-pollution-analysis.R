#The overall goal of this assignment is to explore the National Emissions Inventory 
#database and see what it say about fine particulate matter pollution in the United 
#states over the 10-year period 1999-2008. R package will be used to support the 
#analysis.

setwd("C:\Users\Hp\Documents\R\US-Air-Pollution-Analysis")
getwd()

# Call Packages dplyr, bindrcpp & ggplot2
library(dplyr)
library(bindrcpp)
library(ggplot2) 

#Downloaded zip file contains 2 distinct RDS files Source_Classification_Code.rds
#and summarySCC_PM25.rds

# reading and exploring NEI data - National Emmissions Data
NEI_data <- readRDS("summarySCC_PM25.rds")

head(NEI_data)

# string data
str(NEI_data)

# dimension data
dim(NEI_data)

# reading and exploring SCC data - Source Classification Code Data
SCC_data <- readRDS("Source_Classification_Code.rds")

# head data
head(SCC_data) 

# string data
str(SCC_data)

# dimension data
dim(SCC_data)


# aggregating NEI emmissions by year
yearly_emmissions <- aggregate(Emissions ~ year, NEI_data, sum)


# plot1.ng
cols <- c("maroon", "orange", "yellow", "Aquamarine")
barplot(height=yearly_emmissions$Emissions/1000, names.arg=yearly_emmissions$year, 
        xlab="Year", ylab=expression('Aggregated Emission'),
        main=expression('Aggregated PM'[2.5]*' Emmissions by Year'), col = cols)

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == “24510”) from 1999 to 2008? Use the base plotting system to make
#a plot answering this question.

# forming Baltimore data which will be NEI_data subset
baltdata <- NEI_data[NEI_data$fips=="24510", ]
# Baltimore yearly emmisisons data
baltYrEmm <- aggregate(Emissions ~ year, baltdata, sum)


# plot2.png
cols1 <- c("maroon", "yellow", "orange", "Aquamarine")
barplot(height=baltYrEmm$Emissions/1000, names.arg=baltYrEmm$year, 
        xlab="Year", ylab=expression('Aggregated Emission'),
        main=expression('Baltimore Aggregated PM'[2.5]*' Emmissions by Year'), 
        col = cols1)

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#variable, which of these four sources have seen decreases in emissions from 1999-2008 
#for Baltimore City? Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

# Baltimore yearly emmisisons data
baltYrTypEmm <- aggregate(Emissions ~ year+ type, baltdata, sum)
# plot3.png
chart <- ggplot(baltYrTypEmm, aes(year, Emissions, color = type))
chart <- chart + geom_line() +
  xlab("year") +
  ylab(expression('Total Emissions')) +
  ggtitle('Total Baltimore Emissions [2.5]* From 1999 to 2008')
print(chart)



#Emissions from coal combustion-related sources changed from 1999-2008?
  
  # plot4.png
  chart1 <- ggplot(baltYrTypEmm, aes(factor(year), Emissions))
chart1 <- chart + geom_bar(stat="identity") +
  xlab("year") +  
  ylab(expression('Total Emissions')) +
  ggtitle('Total [2.5]* Coal Emissions From 1999 to 2008')
print(chart1)


#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
  
  # plot5.png
  # Type: ON-ROAD, Fips = "24510" Baltimore Motor Vehicle PM[2.5]* Emissions from 1999 to 2008
  chart <- ggplot(baltYrTypEmm, aes(factor(year), Emissions))
chart <- chart + geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression('Total Emissions')) +
  ggtitle('Baltimore Motor Vehicle PM[2.5] Emissions From 1999 to 2008')
print(chart)



#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == “06037”). Which city has seen greater changes over time in motor vehicle emissions?
  
  # Comparing Baltimore, MD-24510 and Los Angeles, CA-06037
  baltYrTypEmmFips <- summarise(group_by(filter(NEI_data, NEI_data$fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
laYrTypEmmFips <- summarise(group_by(filter(NEI_data, NEI_data$fips == "06037"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))

baltYrTypEmmFips$County <- "Baltimore City, MD"
laYrTypEmmFips$County <- "Los Angeles County, CA"

baltLaEmissions <- rbind(baltYrTypEmmFips, laYrTypEmmFips)
# plot6.png
# Type: ON-ROAD, Fips = 24510 for Baltimore, MD Motor Vehicle PM[2.5]* Emissions Against Los Angeles, CA Fips = 06037  from 1999 to 2008
ggplot(baltLaEmissions, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions,2))) +
  geom_bar(stat="identity") + 
  facet_grid(County~., scales="free") +
  ylab(expression("total PM"[2.5]*" emissions in tons")) + 
  xlab("year") +
  ggtitle(expression("Baltimore City vs Los Angeles County Motor vehicle emission in tons"))+
  geom_label(aes(fill = County),colour = "yellow", fontface = "bold")


knitr::opts_chunk$set(echo = TRUE)
