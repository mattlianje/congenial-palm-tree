library(ggmap)
library(RCurl)
library(xlsx)
library(zipcode)

urlfile <- 'http://www.psc.isr.umich.edu/dis/census/Features/tract2zip/MedianZIP-3.xlsx'
destfile <- "census20062010.xlsx"
download.file(urlfile, destfile, mode="wb")

census <- read.xlsx2(destfile, sheetName = "Median")
census <- census[c('Zip', 'Median..')]
names(census) <- c('Zip', 'Median')
census$Median <- as.character(census$Median)
census$Median <- as.numeric(gsub(',','',census$Median))


data(zipcode)
census$Zip <- clean.zipcodes(census$Zip)
census <- merge(census, zipcode, by.x='Zip', by.y='zip')

map <- get_map(location='united states', zoom=4, maptype = "terrain", source='google', color='color')
ggmap(map) + geom_point(aes(x=longitude, y=latitude, show_guide = TRUE, colour=Median), data=census, alpha=.2, na.rm = T) + scale_color_gradient(low="beige", high="red")
