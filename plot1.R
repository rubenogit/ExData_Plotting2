
#Download and unzip the data file
address <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
address <- sub("^https", "http", address)
zipname <- "NEI_data.zip"
download.file(address,zipname)
unzip(zipname)

#housekeeping - remove the zip as it is no longer needed
file.remove("NEI_data.zip")
#housekeeping
rm(address, zipname)

#Read the data
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##aggregate emmisions per year and plot
emissions_per_year <- aggregate(Emissions ~ year, data = NEI, sum)

plot(emissions_per_year$year, emissions_per_year$Emissions,
     type="b", xlab="Year", ylab="PM2.5 Emissions",
     main="Total PM2.5 emissions from all sources",
     xaxt="n",
     sub="Total PM2.5 emissions for the US from 1999 to 2008 have decreased")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year$Emissions[emissions_per_year$year==1999],lty=4,col="red")

#save to png
dev.copy(png,'plot1.png')
dev.off()
