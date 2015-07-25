
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
emissions_per_year_vehicles_balt_la <- aggregate(Emissions ~ year + fips, data = NEI[NEI$SCC %in% SCC_vehicles & NEI$fips %in% c("24510","06037"),], sum)

bl <- emissions_per_year_vehicles_balt_la


#calculate emissions relative to 1999 level
bl$RelEmissions[bl$fips == "06037"] <- 
(bl$Emissions[bl$fips == "06037"] /
 bl$Emissions[bl$year == 1999 & bl$fips == "06037"])

bl$RelEmissions[bl$fips == "24510"] <- 
(bl$Emissions[bl$fips == "24510"] /
 bl$Emissions[bl$year == 1999 & bl$fips == "24510"])

#plot relative emissions
plot(bl$year,
     bl$RelEmissions,
     type="n",
     xlab="Year", ylab="PM2.5 Emissions as percentage of 1999 level",
     main="Relative motor vehicle related PM2.5 emissions LA and Baltimore City",
     xaxt="n", yaxt="n",
     sub="Motor vehicle related PM2.5 emissions: increased for LA and decreased for Baltimore")
#add years to x-axis
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
#add percentages to y-axis
axis(2, at=c(0.4,0.6,0.8,1.0,1.2), lab=c("40%","60%","80%","100%","120%")) 
#add the line for LA
lines(bl$year[bl$fips == "06037"],
      bl$RelEmissions[bl$fips == "06037"],
      type="b", col="red")
#add the line for Baltimore
lines(bl$year[bl$fips == "24510"],
      bl$RelEmissions[bl$fips == "24510"],
      type="b", col="blue")
abline(h=1.0,lty=4,col="black")
legend("bottomleft", legend=c("Los Angeles","Baltimore"), col=c("red","blue"), lty=1, bty="y")

#save to png
dev.copy(png,'plot6.png')
dev.off()
