
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

###vehicles baltimore City (fips 24150)
###I include the Highway vehicles and the Off-highway vehicles
SCC_vehicles <- as.character(SCC$SCC[grep("(Highway Veh)|(Off-highway)", SCC$Short.Name)])
emissions_per_year_vehicles_balt <- aggregate(Emissions ~ year, data = NEI[NEI$SCC %in% SCC_vehicles & NEI$fips == "24510",], sum)

plot(emissions_per_year_vehicles_balt$year, emissions_per_year_vehicles_balt$Emissions,
     type="b", xlab="Year", ylab="PM2.5 Emissions",
     main="Motor vehicle related PM2.5 emissions Baltimore City",
     xaxt="n",
     sub="Motor vehicle related PM2.5 emissions for Baltimore City have decreased")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year_vehicles_balt$Emissions[emissions_per_year_vehicles_balt$year==1999],lty=4,col="red")

#save to png
dev.copy(png,'plot5.png')
dev.off()
