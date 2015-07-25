
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

#Coal Combustion related emissions
#use Short Name to find combination of Comb and Coal
SCC_coal <- as.character(SCC$SCC[grep("(Comb).*(Coal)", SCC$Short.Name)])
emissions_per_year_coal <- aggregate(Emissions ~ year, data = NEI[NEI$SCC %in% SCC_coal,], sum)

plot(emissions_per_year_coal$year, emissions_per_year_coal$Emissions,
     type="b", xlab="Year", ylab="PM2.5 Emissions",
     main="Total coal combustion related PM2.5 emissions across the US",
     xaxt="n",
     sub="Coal combustion related PM2.5 emissions across the US have decreased")
axis(1, at=c(1999,2002,2005,2008), lab=c("1999","2002","2005","2008")) 
abline(h=emissions_per_year_coal$Emissions[emissions_per_year_coal$year==1999],lty=4,col="red")

#save to png
dev.copy(png,'plot4.png')
dev.off()
