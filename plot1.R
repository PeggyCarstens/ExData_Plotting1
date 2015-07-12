#Plot1 - histogram of Global Active Power
#       date range is Feb 1, 2007 to Feb 2, 2007

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
                    na.strings="?", colClasses=classes,skipNul=TRUE)
  
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
#Create the histogram for global active power

par(mfrow=c(1,1)) #just in case this is run after another page that has multiple plots

with(hps2, hist(hps2$Global_active_power, 
                main="Global Active Power",
                col="red", 
                xlab="Global Active Power (kilowatts)")
    )

###################################################

#copy file to png
dev.copy(png, filename="plot1.png", width=480, height=480, units="px")
dev.off()


