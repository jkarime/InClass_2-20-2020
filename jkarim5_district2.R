#
# example of mapping data onto illinois counties
# based on example from https://people.ohio.edu/ruhil/Rbook/maps-in-r.html
#

library(ggplot2)
library(rgeos)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(ggthemes)
library(sp)
library(stringr)
library(plyr)
library(dplyr)

usa <- map_data("county")
il <- subset(usa, region == "illinois")

il$county = str_to_title(il$subregion)

#basic map with county boundaries and county names at the centroid of the county
getLabelPoint <- # Returns a county-named list of label points
  function(county) {Polygon(county[c('long', 'lat')])@labpt}
centroids = by(il, il$county, getLabelPoint)     # Returns list
centroids2 <- do.call("rbind.data.frame", centroids)  # Convert to Data Frame
centroids2$county = rownames(centroids)
names(centroids2) <- c('clong', 'clat', "county")

#simple map with county borsers and county names
#ggplot() + geom_polygon(data = il, aes(x = long, y = lat, group = group), fill = "white", color = "gray") + coord_fixed(1.2)  + geom_text(data = centroids2, aes(x = clong, y = clat, label = county), color = "darkblue", size = 2.25)  + theme_map()

# read in data on the number of electric vehicles registered in each county
evs <- read.table(file="http://www.evl.uic.edu/aej/424/EVs%20in%20IL.csv", sep=",", header=TRUE)

names(evs)[names(evs) == "ï..county"] <- "county"
# as usual here you should take a look at the data and see if the numbers make sense

#combine the county data and the EV data - they have the 'county' attribute in common
# join keeps the original ordering where merge does not
ilCountyPlusEV <- join(il, evs)

#plot the population per county
ggplot() + geom_polygon(data = ilCountyPlusEV, aes(x = long, y = lat, group = group, fill = population), color = "black") + coord_fixed(1.2) +
  geom_text(data = centroids2, aes(x = clong, y = clat, label = county), color = "black", size = 2.25) + scale_fill_distiller(palette = "Oranges") +
  labs(fill = "population") + theme_map()

#plot the percentage of cars to people in each county
ggplot() + geom_polygon(data = ilCountyPlusEV, aes(x = long, y = lat, group = group, fill = percent_cars), color = "black") + coord_fixed(1.2) +
  geom_text(data = centroids2, aes(x = clong, y = clat, label = county), color = "black", size = 2.25) + scale_fill_distiller(palette = "Oranges") +
  labs(fill = "car %") + theme_map()

#plot the total number of EVs per county
ggplot() + geom_polygon(data = ilCountyPlusEV, aes(x = long, y = lat, group = group, fill = evs), color = "black") + coord_fixed(1.2) +
  geom_text(data = centroids2, aes(x = clong, y = clat, label = county), color = "black", size = 2.25) + scale_fill_distiller(palette = "Oranges") +
  labs(fill = "# of EVs") + theme_map()

#plot the percentage of cars that are EVs in the county
ggplot() + geom_polygon(data = ilCountyPlusEV, aes(x = long, y = lat, group = group, fill = percent_cars_evs), color = "black") + coord_fixed(1.2) +
  geom_text(data = centroids2, aes(x = clong, y = clat, label = county), color = "black", size = 2.25) + scale_fill_distiller(palette = "Blues") +
  labs(fill = "EV %") + theme_map()

#There are better (more complicated) ways to use color here
#but this should give you a starting point for what you can do