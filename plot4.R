###   Exploratory Data Analysis - Course Project 1
###   Filename: plot4.R
###   Date: 2015-05-10 
###   Author: R. Bridgwater
###   Build/test platform: Mac
###   Description: Create four plots and save in one PNG file.
###   -------------------------------------------------------------------

###   Read in data from flat file in working directory.
###   Skip first row. Convert question mark values to NAs. 
      raw <- read.table("household_power_consumption.txt", sep = ";", skip = 1, na.strings = "?")

###   Rename column headings.
      colLabels <- c("date", "time", "globalActivePower", "globalReactivePower","voltage","globalIntensity","subMetering1","subMetering2","subMetering3")
      colnames(raw) <- colLabels

###   Format "date" column to date class.
      raw$date <- as.Date(raw$date, "%d/%m/%Y")

###   Combine "date" and "time" columns 
###   to new column called "dateTime".
      raw$dateTime <- paste(raw$date, raw$time)

###   Reorder columns so new "dateTime" column 
###   is at the beginning of the dataframe.
      raw <- raw[,c(10,1,2,3,4,5,6,7,8,9)]

###   Convert the new "dateTime" column to a new date/time class (POSIXlt) 
      raw$dateTime <- strptime(raw$dateTime, format="%Y-%m-%d %H:%M:%S", tz="")

###   Remove "date and "time" columns.
      raw$date <- NULL
      raw$time <- NULL

###   Extract only rows from 2007-02-01 through 2007-02-02
      raw <- raw[(raw$dateTime >= "2007-02-01 00:00:00" & raw$dateTime < "2007-02-03 00:00:00"),]

###   Convert "dateTime" to POSIXct class 
###   to allow complete.cases function to work.
###   Note: could do this above at "strpTime()" step.
      raw$dateTime <- as.POSIXct(raw$dateTime)

###   Remove all rows that have NAs.
      tidy <- raw[complete.cases(raw),]

###   Create PNG file of multiple plot layout and save to working directory.
      png(filename = "plot4.png", width = 480, height = 480, units = "px")
      
      # Set global plot properties - rows/cols and margins
      par(mfrow = c(2,2), mar = c(4,4,4,4))
      
      with(raw5,{
        
      # Plot 1  
      plot(tidy$dateTime, tidy$globalActivePower, type ="n", main = "", xlab="", ylab ="Global Active Power (kilowatts)")
      lines(tidy$dateTime, tidy$globalActivePower,type="l")
      
      # Plot 2
      plot(tidy$dateTime, tidy$voltage, type ="n", main = "", xlab="datetime", ylab ="voltage")
      lines(tidy$dateTime, tidy$voltage,type="l")
      
      # Plot 3
      plot(tidy$dateTime, tidy$subMetering1, type ="n", main = "", xlab="", ylab ="Energy sub metering")
      lines(tidy$dateTime, tidy$subMetering1, type="l", col="black")
      lines(tidy$dateTime, tidy$subMetering2, type="l", col="red")
      lines(tidy$dateTime, tidy$subMetering3, type="l", col="blue")
      legend("topright", lty = 1, col = c("black","red","blue"), bty = "n", bg = 1, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
      
      # Plot 4
      plot(tidy$dateTime, tidy$globalReactivePower, type ="n", main = "", xlab="datetime", ylab ="global_reactive_power")
      lines(tidy$dateTime, tidy$globalReactivePower, type="l")
      
      })

###   Close graphic device
      dev.off()



