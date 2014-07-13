#=========================================================
#
#	File:  plot3.R
#
#	This file reads the data given for the project
#	and creates plot3.png file. 
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
  
  # read the ';' seperated text file
  
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






plot3 <- function() {
  # load the file ( code is in the prepare.R )
  df <- loadData()
  
  # create empty plot and then three different lines vs DataTime column which was created, finally add the legend 
  
  with(df,plot(DateTime,Sub_metering_1,type="n",ylab="Energy sub metering",xlab=""))
  with(df,{
    lines(DateTime,df$Sub_metering_1,col="black")
    lines(DateTime,df$Sub_metering_2,col="red")
    lines(DateTime,df$Sub_metering_3,col="blue")
  })
    legend("topright",lty=c(1,1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),y.intersp=1)
}



png("plot3.png", height=480, width = 480)
plot3()
dev.off()
