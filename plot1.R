plot1<- function(x) {
    # Install R packages and read file
    install.packages("data.table")
    library(data.table)
    power = fread("household_power_consumption.txt")

# Convert to real dates
power$RealDate = as.Date(power$Date, format = "%d/%m/%Y")

#Subset file to only include 02/01/07 and 02/02/07 dates
subpower <- subset(power, RealDate == as.Date("2007-02-01") | RealDate == as.Date("2007-02-02"))
  
# Convert to real times
subpower$CombTime <- paste(subpower$Date, subpower$Time)
subpower$RealTime = as.character(strptime(subpower$CombTime, format = "%m/%d/%Y %H:%M:%S", tz=""))

# Create histogram
hist(as.numeric(subpower$Global_active_power), col="red", main = "Global Active Power", xlab="Global Active Power (kilowatts)")

# Save plot to a png file and close device
dev.copy(png, file="plot1.png")
dev.off()
}