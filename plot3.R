plot3 <- function(x) {
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
    
    # Create plot of sub_metering_1 first and then add lines for 2 and 3
    plot(subpower$POStime, as.numeric(subpower$Sub_metering_1), type="o", pch="", xlab="", ylab="Energy sub metering")
    lines(subpower$POStime, as.numeric(subpower$Sub_metering_2), col="red")
    lines(subpower$POStime, as.numeric(subpower$Sub_metering_3), col="blue")
    
    # Add a legend
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), lwd=c(2.5,2.5), col=c("black", "red", "blue"))
    
    # Save plot to a png file and close device
    dev.copy(png, file="plot3.png")
    dev.off()
}