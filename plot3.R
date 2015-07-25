
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
