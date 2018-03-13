library(shiny)
library(leaflet)
library(ggmap)
library(leaflet.extras)
library(magrittr)

shinyServer(
  function(input, output, session){
    
    output$mymap <- renderLeaflet({
      
      getLocation <- input$address
      userLocation <- geocode(getLocation)
      locationLon <- userLocation$lon
      locationLat <- userLocation$lat
      
      yogaGyms <- read.csv("yoga_studio_locations_more.csv", TRUE, ",")
      class(yogaGyms)
      
      pedestrianTraffic <- read.csv("city_of_Toronto_pedestrian_traffic_open_data.csv", TRUE, ",")
      class(pedestrianTraffic)
      
      icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
      userLocationRow <- data.frame(Name = 'Your Location', x = locationLon, y = locationLat)
      yogaGyms <- rbind(yogaGyms, userLocationRow)
      
      # sets the color for the yoga gym markers
      
      getColor <- function(yogaGyms) {
        sapply(yogaGyms$Name, function(Name) {
          if(Name == "Your Location") {
            "green"
          } else {
            "orange"
          }  
        })
      }
      
      # changes the color of the pedestrian traffic points depending on magnitude of traffic
      
      getTrafficColor <- function(pedestrianTraffic) {
        sapply(pedestrianTraffic$pedestrianVolume, function(pedestrianVolume) {
          if (pedestrianVolume <= 7500) {
            "red"
          }
          else if(pedestrianVolume <= 15000) {
            "red"
          }
          else {
            "red"
          }
        })
      }
      
      # styling for the yoga gym markers
      
      icons <- awesomeIcons(
        icon = 'ios',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(yogaGyms)
      )
      
      numberOfRows <- nrow(yogaGyms)
      numberOfPedestrianRows <- nrow(pedestrianTraffic)
      
      leaflet(data = yogaGyms[1:numberOfRows,]) %>%
        addTiles() %>%
        setView(lng = locationLon, lat = locationLat, zoom=14) %>%
        addAwesomeMarkers(lng = ~x, lat = ~y, icon = icons, label = ~as.character(Name))
      
 #### TODO uncomment the 4 lines below to show density map   
      # trafficColor <- getTrafficColor(pedestrianTraffic)
      # leaflet(data = pedestrianTraffic) %>% addTiles() %>%
      #   setView(lng = locationLon, lat = locationLat, zoom=14) %>%
      #   addCircleMarkers(radius = ~pedestrianVolume/1000, lng = ~Longitude, lat = ~Latitude, weight = 5, color = trafficColor, stroke = FALSE, fillOpacity = 0.3)
 ######      
      
    # end of the render leaflet
    })
    
    
  #TODO figure out how to get the observe working to have multilayer map
    
  #  observe({
  #    
  #    pedestrianTraffic <- read.csv("city_of_Toronto_pedestrian_traffic_open_data.csv", TRUE, ",")
  #    class(pedestrianTraffic)
      
  #    leafletProxy("mymap", data = pedestrianTraffic) %>% addTiles() %>%
        #setView(lng = locationLon, lat = locationLat, zoom=14) %>%
        # addWebGLHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~pedestrianVolume, size = 25)
  #      addCircleMarkers(radius = ~pedestrianVolume/1000, lng = ~Longitude, lat = ~Latitude, weight = 5, color = "red", stroke = FALSE, fillOpacity = 0.3)
      
  #  })
    
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