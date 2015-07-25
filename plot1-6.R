
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
     main="Total PM2.5 emissions from all sources", xaxt="n")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year$Emissions[emissions_per_year$year==1999],lty=4,col="red")

#save to png
dev.copy(png,'plot1.png')
dev.off()


##aggregate emmisions per year for Baltimore City (fips 24150) and plot
emissions_per_year_balt <- aggregate(Emissions ~ year, data = NEI[NEI$fips == "24510",], sum)
#plot(emissions_per_year_balt$year, emissions_per_year_balt$Emissions, type="b")

plot(emissions_per_year_balt$year, emissions_per_year_balt$Emissions,
     type="b", xlab="Year", ylab="PM2.5 Emissions",
     main="Total PM2.5 emissions in Baltimore", xaxt="n")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year_balt$Emissions[emissions_per_year_balt$year==1999],lty=4,col="red")


#save to png
dev.copy(png,'plot2.png')
dev.off()

##aggregate emmisions per year and type for Baltimore City (fips 24150)
emissions_per_year_type_balt <- aggregate(Emissions ~ year + type, data = NEI[NEI$fips == "24510",], sum)

###use ggplot2 here
library(ggplot2)
Em1999 <- emissions_per_year_type_balt[emissions_per_year_type_balt$year==1999,]
plot3 <- ggplot(emissions_per_year_type_balt, aes(year, Emissions)) + 
geom_point() + 
geom_line() + 
geom_hline(data=Em1999, aes(yintercept=Emissions), linetype=4, colour="red") + 
facet_wrap(~type, ncol=2)
plot3

#save to png
dev.copy(png,'plot3.png')
dev.off()

#####coal
#SCC_coal <- as.character(SCC$SCC[grep("Coal", SCC$EI.Sector)])
#use Short Name to find combination of Comb and Coal
SCC_coal <- as.character(SCC$SCC[grep("(Comb).*(Coal)", SCC$Short.Name)])
emissions_per_year_coal <- aggregate(Emissions ~ year, data = NEI[NEI$SCC %in% SCC_coal,], sum)

#plot(emissions_per_year_coal$year, emissions_per_year_coal$Emissions, type="b")
plot(emissions_per_year_coal$year, emissions_per_year_coal$Emissions,
     type="b", xlab="Year", ylab="PM2.5 Emissions",
     main="Total coal related PM2.5 emissions", xaxt="n")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year_coal$Emissions[emissions_per_year_coal$year==1999],lty=4,col="red")

#save to png
dev.copy(png,'plot4.png')
dev.off()

###vehicles baltimore City (fips 24150)
###I include the Highway vehicles and the Off-highway vehicles
SCC_vehicles <- as.character(SCC$SCC[grep("(Highway Veh)|(Off-highway)", SCC$Short.Name)])
emissions_per_year_vehicles_balt <- aggregate(Emissions ~ year, data = NEI[NEI$SCC %in% SCC_vehicles & NEI$fips == "24510",], sum)

#plot(emissions_per_year_vehicles_balt$year, emissions_per_year_vehicles_balt$Emissions, type="b")
plot(emissions_per_year_vehicles_balt$year, emissions_per_year_vehicles_balt$Emissions,
     type="b", xlab="Year", ylab="PM2.5 Emissions",
     main="Motor vehicle related PM2.5 emissions Baltimore", xaxt="n")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year_vehicles_balt$Emissions[emissions_per_year_vehicles_balt$year==1999],lty=4,col="red")



#save to png
dev.copy(png,'plot5.png')
dev.off()

###vehicles baltimore City (fips 24150)
###I include the Highway vehicles and the Off-highway vehicles
SCC_vehicles <- as.character(SCC$SCC[grep("(Highway Veh)|(Off-highway)", SCC$Short.Name)])
emissions_per_year_vehicles_balt_la <- aggregate(Emissions ~ year + fips, data = NEI[NEI$SCC %in% SCC_vehicles & NEI$fips %in% c("24510","06037"),], sum)

bl <- emissions_per_year_vehicles_balt_la


#calculate relative emissions
bl$RelEmissions[bl$fips == "06037"] <- 
(bl$Emissions[bl$fips == "06037"] /
 bl$Emissions[bl$year == 1999 & bl$fips == "06037"])


bl$RelEmissions[bl$fips == "24510"] <- 
(bl$Emissions[bl$fips == "24510"] /
 bl$Emissions[bl$year == 1999 & bl$fips == "24510"])

plot(bl$year,
     bl$RelEmissions,
     type="n",
     xlab="Year", ylab="PM2.5 Emissions as percentage of 1999 level",
     main="Relative motor vehicle related PM2.5 emissions Baltimore and LA",
     xaxt="n", yaxt="n",)
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
axis(2, at=c(0.4,0.6,0.8,1.0,1.2), lab=c("40%","60%","80%","100%","120%")) 
lines(bl$year[bl$fips == "06037"],
      bl$RelEmissions[bl$fips == "06037"],
      type="b", col="red")
lines(bl$year[bl$fips == "24510"],
      bl$RelEmissions[bl$fips == "24510"],
      type="b", col="blue")
legend("bottomleft", legend=c("Los Angeles","Baltimore"), col=c("red","blue"), lty=1, bty="y")

#save to png
dev.copy(png,'plot6.png')
dev.off()

plot(bl$year,
     bl$Emissions,
     type="n",
     xlab="Year", ylab="PM2.5 Emissions",
     main="Total motor vehicle related PM2.5 emissions Baltimore and LA",
     xaxt="n")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
lines(bl$year[bl$fips == "24510"],
      bl$Emissions[bl$fips == "24510"],
      type="b", col="blue")
lines(bl$year[bl$fips == "06037"],
      bl$Emissions[bl$fips == "06037"],
      type="b", col="red")
abline(h=bl$Emissions[bl$year==1999 & bl$fips == "24510"],lty=4,col="black")
abline(h=bl$Emissions[bl$year==1999 & bl$fips == "06037"],lty=4,col="black")

legend("right", legend=c("Los Angeles","Baltimore"), col=c("red","blue"), lty=1, bty="y")

#save to png
dev.copy(png,'plot6b.png')
dev.off()
