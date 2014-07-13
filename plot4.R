#=========================================================
#
#	File:  plot4.R
#
#	This file reads the data given for the project
#	and creates plot4.png file. 
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
  
  fullData <-  read.table("./household_power_consumption.txt",sep=";",as.is = TRUE , header=TRUE,na.strings=c("NA","?",""))
  
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






plot4 <- function() {
  
  # load the file , the function is in R script called prepare.R
  df <- loadData()
  
  #
  # set the 2x2 layout
  
  par(mfrow = c(2, 2))
  
  
  with(df, {
    
    #1 1st plot - empty plot and then add lines
    plot(DateTime,Global_active_power,type="n",ylab="Global Active Power",xlab="")
    lines(DateTime,Global_active_power)

    
    #2 second plot - empty plot and then add lines for the voltage
    plot(DateTime,Voltage,type="n",ylab="Voltage",xlab="datetime")
    lines(DateTime,Voltage)
    
    
    # Plot3 - empty plot, three lines with different colours and then legend 
    plot(DateTime,Sub_metering_1,type="n",ylab="Energy sub metering",xlab="")
    lines(DateTime,Sub_metering_1,col="black")
    lines(DateTime,Sub_metering_2,col="red")
    lines(DateTime,Sub_metering_3,col="blue")
    
    legend("topright",lty=c(1,1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),bty="n",y.intersp=1,xjust=1,inset=0,seg.len=2)
  
    #4 PLot 4 , get an empty plot followed by lines for reactive power
    plot(DateTime,Global_reactive_power,type="n",ylab="Global_reactive_power",xlab="datetime")
    lines(DateTime,Global_reactive_power)
    
  })
  
}


# open the png file
# do the plot
# close the device. 

png("plot4.png", height=480, width = 480,units="px",bg="transparent")
plot4()
dev.off()
