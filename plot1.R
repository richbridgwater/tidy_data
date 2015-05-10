###   Exploratory Data Analysis - Course Project 1
###   Filename: plot1.R
###   Date: 2015-05-10 
###   Author: R. Bridgwater
###   Build/test platform: Mac
###   Description: Create histogram plot and save as PNG file.
###   ---------------------------------------------------------------
    
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
###   to allow complete.cases function to work
###   Note: could do this above at "strpTime()" step.
      raw$dateTime <- as.POSIXct(raw$dateTime)
    
###   Remove all rows that have NAs.
      tidy <- raw[complete.cases(raw),]
    
###   Create PNG file of plot and save to working directory
      png(filename = "plot1.png", width = 480, height = 480, units = "px")
      with(tidy, hist(tidy$globalActivePower, col = "red", main = "Global Active Power", xlab ="Global Active Power (kilowatts)"))
    
###   Close graphic device
      dev.off()

