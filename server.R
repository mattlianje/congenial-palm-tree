library(shiny)
library(leaflet)
library(ggmap)

shinyServer(
  function(input, output, session){
    
    output$mymap <- renderLeaflet({
      
      getLocation <- input$address
      userLocation <- geocode(getLocation)
      locationLon <- userLocation$lon
      locationLat <- userLocation$lat
      
      yogaGyms <- read.csv("yoga_studio_locations_more.csv", TRUE, ",")
      class(yogaGyms)
      
      icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
      userLocationRow <- data.frame(Name = 'Your Location', x = locationLon, y = locationLat)
      yogaGyms <- rbind(yogaGyms, userLocationRow)
      
      getColor <- function(yogaGyms) {
        sapply(yogaGyms$Name, function(Name) {
          if(Name == "Your Location") {
            "green"
          } else {
            "orange"
          }  
        })
      }
      
      icons <- awesomeIcons(
        icon = 'ios',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(yogaGyms)
      )
      numberOfRows <- nrow(yogaGyms)
      leaflet(data = yogaGyms[1:numberOfRows,]) %>%
        addTiles() %>%
        setView(lng = locationLon, lat = locationLat, zoom=14) %>%
        addAwesomeMarkers(lng = ~x, lat = ~y, icon = icons, label = ~as.character(Name))
        #addMarkers(lng = ~x, lat = ~y, popup = "rival yoga studio")
      
      #leaflet() %>% addTiles() %>%
      #  addAwesomeMarkers(
      #    lng = locationLon, lat = locationLat,
      #    label = 'This is a label',
      #    icon = icon.glyphicon)
 
    })
    output$testPlot <- renderPlot({
      distType <- input$Distribution
      size <- input$sampleSize
      
      if(distType == "Normal"){
        randomVec <- rnorm(size, mean = as.numeric(input$mean), sd=as.numeric(input$sd))
      }
      else {
        randomVec <- rexp(size, rate = 1/as.numeric(input$lambda))
      }
      hist(randomVec, col="orange")
    })
  }
)