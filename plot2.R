#=========================================================
#
#	File:  plot2.R
#
#	This file reads the data given for the project
#	and creates plot2.png file. 
#
#	Author: Prasanna Limaye ( prasanna.limaye@hotmail.com)
#
#	Project: Coursera Assignment 1 of Exploratory Data
#
#
#
#
#----------------------------------------------------------

#
#  Function loadData()
#
#     Assumes the houshold_power_consumption.txt in project directory.
#
#       this function loads the data
#       the data in Posix format is created by combining two columns: Date and time. 
#       the DateTime column is added in the original Data.
#        data is then filtered getting data  for the two days worth of data
#       dated  1 feb 2007 and 2 feb 2007
#     
#       the key data fields are then properly converted to numeric   
#
#       the final data frame is returned for plotting
#
#

loadData <- function() {
  
  # read the ; seperated tet file
  
  fullData <-  read.table("./household_power_consumption.txt",sep=";",as.is = TRUE , header=TRUE,na.strings="NA")
  
  # paste the date and time columns and format it into standard date format. 
  
  fullData$DateTime <- strptime(paste(fullData$Date,fullData$Time),"%d/%m/%Y %H:%M:%S")
  
  # get the indices for key two dates
  
  validDataIndex <-  ((fullData$Date == "1/2/2007" ) | ( fullData$Date == "2/2/2007" ))
  
  # filter the data with the computed index
  
  df <- fullData[validDataIndex,]
  
  # convert key fields to the numeric
  
  df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
  df$Global_active_power <- as.numeric(df$Global_active_power)
  df$Voltage <- as.numeric( df$Voltage)
  df$Global_intensity <- as.numeric( df$Global_intensity)
  df$Sub_metering_1 <- as.numeric( df$Sub_metering_1)
  df$Sub_metering_2 <- as.numeric( df$Sub_metering_2)
  df$Sub_metering_3 <- as.numeric( df$Sub_metering_3)
  
  # return the data frame. 
  return(df)
}






plot2 <- function() {
  # load the data from the file 
  
  df <- loadData()
  # plot using X axis as dateTime and y axis as active power column 
    with(df,plot(DateTime,Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab=""))
}

#
#
# open the png file, do the plotting and close the device
#

png("plot2.png", height=480, width = 480)
plot2()
dev.off()
