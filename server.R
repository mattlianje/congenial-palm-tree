library(shiny)
library(leaflet)
library(ggmap)
library(leaflet.extras)
library(magrittr)
library(lpSolve)
library(plyr)
library(geosphere)
library(shinyjs)

shinyServer(function(input, output, session) {
  #data loading jobs
  observeEvent(input$refresh,{
    session$reload()
  })
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
      
      
      #getMUserToHotspots <- function(i) {
      #  mapdist(as.numeric(pedestrianTraffic[i, c ('Longitude', 'Latitude')]),
      #         as.numeric(locationLon, locationLat))$m
      #}
      
      
      newPedestrianTraffic <- head(arrange(pedestrianTraffic, desc(pedestrianVolume)), n = 10)
      class(newPedestrianTraffic)
      newPedestrianTraffic$distanceToUser <- NA
      
      # print(newPedestrianTraffic)
      
      
      #from <- c(newPedestrianTraffic[5, "Longitude"], newPedestrianTraffic[5, "Latitude"])
      #to <- c(locationLon, locationLat)
      #test <- mapdist(from, to)
      #test2 <- (test$m)
      #print(test2)
      #print(typeof(test2))
      #newPedestrianTraffic[2, "distanceToUser"] = test2
      #print(newPedestrianTraffic)
      
      
      for (i in 1:nrow(newPedestrianTraffic)) {
        from <- c(newPedestrianTraffic[i, "Longitude"], newPedestrianTraffic[i, "Latitude"])
        to <- c(locationLon, locationLat)
        test <- distm(from, to, fun = distHaversine)
        test2 <- (test)
        
        newPedestrianTraffic[i, "distanceToUser"] = test
      }
      
      newPedestrianTraffic$dToUserXWeight <- newPedestrianTraffic$pedestrianVolume * newPedestrianTraffic$distanceToUser
      toMinimize <- sum(newPedestrianTraffic$dToUserXWeight)
      print(newPedestrianTraffic)
      cat("with the user long lat at first the value to minimize is", toMinimize)
      
      # computational duplicate vars for the solver
      # x <- locationLat
      # y <- locationLon
      
      # print(newPedestrianTraffic)
      # Write piece of function that optimizes user latlong such that he is closest to population hubs
      # f <- function(a,b) sum(newPedestrianTraffic[i, "pedestrianVolume"] * distm(c(newPedestrianTraffic[i, "Longitude"], newPedestrianTraffic[i, "Latitude"]), c(a, b)))
      # ymin <- optimize(f, c(x,y), tol = 0.0001, b=locationLat)
      # print(ymin)
      
      icon.glyphicon <- makeAwesomeIcon(icon = 'flag',
                        markerColor = 'blue',
                        iconColor = 'black')
      userLocationRow <- data.frame(Name = 'Your Location', x = locationLon, y = locationLat)
      yogaGyms <- rbind(yogaGyms, userLocationRow)
      
      # sets the color for the yoga gym markers
      
      getColor <- function(yogaGyms) {
        sapply(yogaGyms$Name, function(Name) {
          if (Name == "Your Location") {
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
          else if (pedestrianVolume <= 15000) {
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
        setView(lng = locationLon, lat = locationLat, zoom = 14) %>%
        addAwesomeMarkers(lng = ~ x, lat = ~ y, icon = icons, label = ~ as.character(Name))
      
      # end of the render leaflet
    })
    
    leafletProxy("mymap",session = shiny::getDefaultReactiveDomain(),data = pedestrianTraffic,deferUntilFlush = TRUE) %>%
      addTiles() %>%
      addCircleMarkers(radius = ~ pedestrianVolume / 1000,lng = ~ Longitude,lat = ~ Latitude, weight = 5, color = "red", stroke = FALSE, fillOpacity = 0.3)
    
    # end of the on get map button click
  })
  
  output$testPlot <- renderPlot({
    distType <- input$Distribution
    size <- input$sampleSize
    
    if (distType == "Normal") {
      randomVec <-
        rnorm(size,
              mean = as.numeric(input$mean),
              sd = as.numeric(input$sd))
    }
    else {
      randomVec <- rexp(size, rate = 1 / as.numeric(input$lambda))
    }
    hist(randomVec, col = "orange")
  })
})