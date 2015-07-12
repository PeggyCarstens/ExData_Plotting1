#Plot 4 - 4 graphs on one page
#         date range for each chart is Feb 1, 2007 to Feb 2, 2007

####################################################

# install needed packages and environment
install.packages("dplyr")
install.packages("lubridate")
library(lubridate)
library(dplyr)

options(max.print=99999) 

#################################################### 

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

#combine the 3 metering values into one table (if stacked var. doesn't already exist)
if (!exists("stacked")) {
  stacked <- with(hps2, 
                  data.frame(value=c(Sub_metering_1, Sub_metering_2, Sub_metering_3),
                             meter_nbr=factor(rep(c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                                                  each=NROW(hps2))),
                             DateTime=rep(DateTime, 3)))
}

#plot the graphs
par(mfrow=c(2,2)) #4 on one page

with(hps2, {
  #1 - plot global active power over time
  plot(DateTime, Global_active_power, type="l",
       xlab="", ylab="Global Active Power")
  #2 - plot voltage over time
  plot(DateTime, Voltage, type="l")
  #plot energy sub metering
})
#3 - plot energy sub metering over time
with(stacked, plot(DateTime, value, xlab="", ylab="Energy sub metering", type="n"))
with(subset(stacked, meter_nbr=="Sub_metering_1"), points(DateTime, value, type="l", col="black"))
with(subset(stacked, meter_nbr=="Sub_metering_2"), points(DateTime, value, type="l", col="red"))
with(subset(stacked, meter_nbr=="Sub_metering_3"), points(DateTime, value, type="l", col="blue"))
legend("topright", lty=1, cex=0.5, bty="n", col=c("black","red","blue"), 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#4- plot global reactive power over time
with(hps2, plot(DateTime, Global_reactive_power, type="l"))

###################################################
#copy file to png
dev.copy(png, filename="plot4.png", width=480, height=480, units="px")
dev.off()
