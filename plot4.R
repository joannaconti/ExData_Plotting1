plot4 <- function(x) {
    # Install R packages and read file
    install.packages("data.table")
    library(data.table)
    power = fread("household_power_consumption.txt")
    
    # Convert to real dates
    power$RealDate = as.Date(power$Date, format = "%d/%m/%Y")
    
    #Subset file to only include 02/01/07 and 02/02/07 dates
    subpower <- subset(power, RealDate == as.Date("2007-02-01") | RealDate == as.Date("2007-02-02"))
    
    # Convert to POSIX times
    subpower$CombTime <- paste(subpower$Date, subpower$Time)
    subpower$RealTime = as.character(strptime(subpower$CombTime, format = "%d/%m/%Y %H:%M:%S", tz=""))
    subpower$POStime = as.POSIXct(subpower$RealTime)
    
    #Convert Global_active_power to numeric
    subpower$numactive = as.numeric(subpower$Global_active_power)
    
    #Set device to png
    png(filename="plot4.png") 
    
    # Create 4 plots
    par(mfrow=c(2,2))
        
    # Create 1st plot of Global Active Power
    plot(subpower$POStime, subpower$numactive, type="o", pch="", xlab="", ylab="Global Active Power (kilowatts)")
    
    # Create 2nd plot of Voltage
    plot(subpower$POStime, as.numeric(subpower$Voltage), type="o", pch="", xlab="datetime", ylab="Voltage")  
    
    #Create third plot 
    plot(subpower$POStime, as.numeric(subpower$Sub_metering_1), type="o", pch="", xlab="", ylab="Energy sub metering")
    lines(subpower$POStime, as.numeric(subpower$Sub_metering_2), col="red")
    lines(subpower$POStime, as.numeric(subpower$Sub_metering_3), col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), cex=0.5, lwd=c(0.5,0.5), bty="n", col=c("black", "red", "blue")) 

    
    #Create 4th plotof Global Reactive Power
    plot(subpower$POStime, as.numeric(subpower$Global_reactive_power), type="o", pch="", xlab="", ylab="Global_reactive_power")

    dev.copy(png, file="plot4.png")
    dev.off()
    # Close device

}
