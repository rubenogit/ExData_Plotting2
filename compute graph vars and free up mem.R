#plot1
emissions_per_year <- aggregate(Emissions ~ year, data = NEI, sum)

#plot2
emissions_per_year_balt <- aggregate(Emissions ~ year, data = NEI[NEI$fips == "24510",], sum)

#plot3
emissions_per_year_type_balt <- aggregate(Emissions ~ year + type, data = NEI[NEI$fips == "24510",], sum)

#plot4
SCC_coal <- as.character(SCC$SCC[grep("(Comb).*(Coal)", SCC$Short.Name)])
emissions_per_year_coal <- aggregate(Emissions ~ year, data = NEI[NEI$SCC %in% SCC_coal,], sum)

#plot5
SCC_vehicles <- as.character(SCC$SCC[grep("(Highway Veh)|(Off-highway)", SCC$Short.Name)])
emissions_per_year_vehicles_balt <- aggregate(Emissions ~ year, data = NEI[NEI$SCC %in% SCC_vehicles & NEI$fips == "24510",], sum)

#plot6
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

rm(NEI,SCC)




