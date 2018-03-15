library(shiny)
library(leaflet)
library(ggmap)
library(leaflet.extras)
library(magrittr)

shinyServer(
  function(input, output, session){
    
    #data loading jobs
    pedestrianTraffic <<- read.csv("city_of_Toronto_pedestrian_traffic_open_data.csv", TRUE, ",")
    class(pedestrianTraffic)
    
    yogaGyms <<- read.csv("yoga_studio_locations_more.csv", TRUE, ",")
    class(yogaGyms)
    
  # runs after the do button (get map button) is clicked  
  observeEvent(input$do, {
    
    output$mymap <- renderLeaflet({
      
      getLocation <- input$address
      userLocation <- geocode(getLocation)
      
      # Globalized instead of a subclass implementation to use on multiple layers of the map
      locationLon <<- userLocation$lon
      locationLat <<- userLocation$lat
      
      icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
      userLocationRow <- data.frame(Name = 'Your Location', x = locationLon, y = locationLat)
      yogaGyms <- rbind(yogaGyms, userLocationRow)
      
      # sets the color for the yoga gym markers
      
      getColor <- function(yogaGyms) {
        sapply(yogaGyms$Name, function(Name) {
          if(Name == "Your Location") {
            "green"
          } else {
            "beige"
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
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(yogaGyms)
      )
      
      leaflet(data = yogaGyms) %>%
        addTiles() %>%
        setView(lng = locationLon, lat = locationLat, zoom=14) %>%
        addAwesomeMarkers(lng = ~x, lat = ~y, icon = icons, label = ~as.character(Name))
      
    # end of the render leaflet
    })
    
      leafletProxy("mymap", session = shiny::getDefaultReactiveDomain(), 
                   data = pedestrianTraffic,
                   deferUntilFlush = TRUE) %>% 
        
                   addTiles() %>%
                   addCircleMarkers(radius = ~pedestrianVolume/1000, 
                                    lng = ~Longitude, 
                                    lat = ~Latitude, 
                                    weight = 5, color = "red", stroke = FALSE, fillOpacity = 0.3)
      
  # end of the on get map button click    
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