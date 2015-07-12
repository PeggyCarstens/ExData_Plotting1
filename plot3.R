#Plot3 - line chart of the 3 submetering fields over time
#       date range is Feb 1, 2007 to Feb 2, 2007

####################################################

# install needed packages and environment
install.packages("dplyr")
install.packages("lubridate")
library(lubridate)
library(dplyr)

options(max.print=99999) 

###################################################

# Read in the data "household_power_consumption.txt"
# Won't read the data if variable hps2 already exists
# Assumes the household_power_consumption.txt" file is in your working directory

if (!exists("hps2")) {
  
  classes = c("character","character",rep("numeric",7))
  hps <- read.table("household_power_consumption.txt", header=TRUE, 
                    nrow=2075270, sep=";", stringsAsFactors=FALSE,
                    na.strings="?", colClasses=classes)
  
  # filter to select dates -- format is dd/mm/yyyy
  hps1 <- filter(hps, Date=="1/2/2007"| Date=="2/2/2007")
  
  # convert date and time and create a Date/Time column
  hps2 <- hps1 %>%
    transform(Date=dmy(Date)) %>%
    transform(Time=hms(Time)) %>%
    mutate(DateTime = Date + Time) #create a date/time column
  
  # cleanup so don't have 2 files taking up resources
  rm(hps, hps1, classes) 
}

###################################################

#combine the 3 metering values into one table
stacked <- with(hps2, 
                data.frame(value=c(Sub_metering_1, Sub_metering_2, Sub_metering_3),
                           meter_nbr=factor(rep(c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                                                each=NROW(hps2))),
                           DateTime=rep(DateTime, 3)))

#create plot using subsets of the stacked data.frame
par(mfrow=c(1,1)) #just in case this is run after another page that has multiple plots

with(stacked, plot(DateTime, value, ylab="Energy sub metering", xlab="", type="n"))
with(subset(stacked, meter_nbr=="Sub_metering_1"), points(DateTime, value, type="l", col="black"))
with(subset(stacked, meter_nbr=="Sub_metering_2"), points(DateTime, value, type="l", col="red"))
with(subset(stacked, meter_nbr=="Sub_metering_3"), points(DateTime, value, type="l", col="blue"))
legend("topright", lty=1, cex=0.8, col=c("blue","red","gray40"), 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

###################################################
#copy file to png
dev.copy(png, filename="plot3.png", width=480, height=480, units="px")
dev.off()
