library(shiny)
library(leaflet)
library(ggmap)
library(leaflet.extras)
library(magrittr)
library(lpSolve)
library(plyr)
library(geosphere)
#library(shinyjs)
library(gdata)
library(RCurl)
library(XLConnect)
library(googleway)
library(jsonlite)
library(rsconnect)
rsconnect::setAccountInfo(name='matthieu-court',
                          token='E9BF06E0309F2053A73CC5B1DFF14E07',
                          secret='gpBRs12P0wxm+BdFawH03pek1ZQlsbWyYbiPqZgl')
#rsconnect::deployApp()

shinyServer(function(input, output, session) {
  #data loading jobs
  observeEvent(input$refresh,{
    session$reload()
  })
  pedestrianTraffic <<- read.csv("city_of_Toronto_pedestrian_traffic_open_data.csv", TRUE, ",")
  class(pedestrianTraffic)
  
  #Test loading the data dynamically from the city of Toronto website
  #### Only pull this xls if you have java on your machine. Else, hardcode a CSV ####
  tmp = tempfile(fileext = ".xlsx")
  download.file(url = "https://www.toronto.ca/ext/open_data/catalog/data_set_files/8hrVeh&PedVolume_6-Mar-2018.xlsx", destfile = tmp, mode = "wb")
  webImport <<- readWorksheetFromFile(file = tmp, sheet = "Total TMM count", header = TRUE)
  class(webImport)
  colnames(webImport)[colnames(webImport) == "X8.Peak.Hr.Pedestrian.Volume"] <- "pedestrianVolume"
  webImport = webImport[-c(3,5,6,10)]
  print(webImport)
  
  
  #yogaGyms <<- read.csv("yoga_studio_locations_more.csv", TRUE, ",")
  #class(yogaGyms)
  observeEvent(input$load, {
  
  })
  # runs after the do button (get map button) is clicked
  observeEvent(input$do, {
    
    output$mymap <- renderLeaflet({
      withProgress(message = 'loading map...', value = 1, {
      
      #type of location the user wants to search for 
        getLocation <<- input$address
        userLocation <<- geocode(getLocation)
        
        # Globalized instead of a subclass implementation to use on multiple layers of the map
        locationLon <<- userLocation$lon
        locationLat <<- userLocation$lat
        locationType <- input$locationType
        
        # Make the Google Places API call and clean up the results
        key <- 'AIzaSyChVlEEFBjIaVbni1rakTQiPzGhdpcWtOc'
        googlePull <<- google_places(radar = TRUE, location = c(locationLat, locationLon), radius = (input$distConstraint)*1000, keyword = locationType, key = key)
        cleanGooglePull <- googlePull$results
        cleanGooglePull = cleanGooglePull[-c(3,4)]
        cleanGooglePull = cleanGooglePull[c(2,1)]
        class(cleanGooglePull)
      
        colnames(cleanGooglePull)[colnames(cleanGooglePull) == "id"] <- "Name"
        
        cleanGooglePull$Name <- "Rival Location"
        
        Name <- cleanGooglePull$Name
        x <- googlePull$results$geometry$location$lng
        y <- googlePull$results$geometry$location$lat
        
        yogaGyms <<- data.frame(Name,
                                x,
                                y)
        class(yogaGyms)
        print(yogaGyms)
      
      incProgress(1, message = "Cleaning up some data...")
      
      newPedestrianTraffic <- head(arrange(webImport, desc(pedestrianVolume)), n = 10)
      class(newPedestrianTraffic)
      newPedestrianTraffic$distanceToUser <- NA
      
      
      incProgress(2, message = "running our top secret optimization algorithm...")
      
      #for (i in 1:nrow(newPedestrianTraffic)) {
      #  from <- c(newPedestrianTraffic[i, "Longitude"], newPedestrianTraffic[i, "Latitude"])
      #  to <- c(locationLon, locationLat)
      #  test <- distm(from, to, fun = distHaversine)
      #  test2 <- (test)
      #  
      #  newPedestrianTraffic[i, "distanceToUser"] = test
      #}
      
      for (i in 1:nrow(webImport)) {
        from <- c(webImport[i, "Longitude"], webImport[i, "Latitude"])
        to <- c(locationLon, locationLat)
        test <- distm(from, to, fun = distHaversine)
        test2 <- (test)
        
        webImport[i, "distanceToUser"] = test
      }
      
      webImport <- subset(webImport, distanceToUser < (input$distConstraint * 1000))
      print(webImport)
      locations <- webImport[-c(1,2,3,6,7)]
      class(locations)
      locations2 <- locations[,c(2,1)]
      class(locations2)
      
      
      locations3 = data.frame(locations2)
      locations3$pedestrianVolume = webImport$pedestrianVolume
      print("locations 3 is###")
      print(locations3)
      
      incProgress(3, message = "running our top secret algorithm...")
      constraint <- input$distConstraint
      
      # First we grab what type of optimization the user wants
      optimType <- input$optimType
      
      userLocationRow <- data.frame(Name = 'Your Original Location', x = locationLon, y = locationLat)
      
      yogaLocations = data.frame(x=yogaGyms$x,
                                 y=yogaGyms$y)
      
      #### OPTIMIZATION SYSTEMS ####
      # User just wants to be close to pedestrian hot spots in his neighbourhood
      if(optimType == "Close to popular spots"){
        
        min.RSS <- function(data, par) {
          with(data, sum(locations3$pedestrianVolume * distm(cbind(locations3$Longitude, locations3$Latitude), par)))
        }
        
        (result <- optim(par = c(locationLon, locationLat), 
                         min.RSS,
                         lower = c(locationLon - constraint*0.009, locationLat - constraint*0.009),
                         upper = c(locationLon + constraint*0.009, locationLat + constraint*0.009),
                         data = locations3,
                         method = "BFGS"))
        
        minimizedLon <- result$par[1]
        minimizedLat <- result$par[2]
        
        minimizedLocationRow <- data.frame(Name = 'Minimized Location', x = minimizedLon, y = minimizedLat)
        yogaGyms <- rbind(yogaGyms, minimizedLocationRow)
        
        
      }else if(optimType == "Far away from my rivals"){
        
        max.RSS <- function(data, par) {
          with(data, sum(distm(yogaLocations, par)))
        }
        
        (result2 <- optim(par = c(locationLon, locationLat), 
                          max.RSS,
                          lower = c(locationLon - constraint*0.3*0.009, locationLat - constraint*0.3*0.009),
                          upper = c(locationLon + constraint*0.3*0.009, locationLat + constraint*0.3*0.009),
                          control = list(fnscale = -1),
                          data = yogaLocations,
                          method = "BFGS"))
        
        
        maximizedLon <- result2$par[1]
        maximizedLat <- result2$par[2]
        
        maximizedLocationRow <- data.frame(Name = 'Maximized Location', x = maximizedLon, y = maximizedLat)
        yogaGyms <- rbind(yogaGyms, maximizedLocationRow)
        
      }else{
        
      ### Combo Solver ###
        
        min.RSS <- function(data, par) {
          with(data, sum(locations3$pedestrianVolume * distm(cbind(locations3$Longitude, locations3$Latitude), par)))
        }
        
        (result3 <- optim(par = c(locationLon, locationLat), 
                          min.RSS,
                          lower = c(locationLon - constraint*0.009, locationLat - constraint*0.009),
                          upper = c(locationLon + constraint*0.009, locationLat + constraint*0.009),
                          data = locations2,
                          method = "BFGS"))
        
        optimized0Lon <- result3$par[1]
        optimized0Lat <- result3$par[2]
        
        max.RSS <- function(data, par) {
          with(data, sum(distm(yogaLocations, par)))
        }
        
        (result4 <- optim(par = c(optimized0Lon, optimized0Lat), 
                          max.RSS,
                          lower = c(optimized0Lon - constraint*0.3*0.009, optimized0Lat - constraint*0.3*0.009),
                          upper = c(optimized0Lon + constraint*0.3*0.009, optimized0Lat + constraint*0.3*0.009),
                          control = list(fnscale = -1),
                          data = yogaLocations,
                          method = "BFGS"))
        
        optimizedLon <- result4$par[1]
        optimizedLat <- result4$par[2]
        
        optimizedLocationRow <- data.frame(Name = 'Combo-Optimized Location', x = optimizedLon, y = optimizedLat)
        yogaGyms <- rbind(yogaGyms, optimizedLocationRow)
        
      }
      
      # adding the updated user location to the map
      yogaGyms <- rbind(yogaGyms, userLocationRow)
      
      newPedestrianTraffic$dToUserXWeight <- newPedestrianTraffic$pedestrianVolume * newPedestrianTraffic$distanceToUser
      toMinimize <- sum(newPedestrianTraffic$dToUserXWeight)
      print(newPedestrianTraffic)
      cat("with the user long lat at first the value to minimize is", toMinimize)
      incProgress(4, message = "Finishing up...")
      
      icon.glyphicon <- makeAwesomeIcon(icon = 'flag',
                        markerColor = 'blue',
                        iconColor = 'black')
     
      # sets the color for the yoga gym markers
      
      getColor <- function(yogaGyms) {
        sapply(yogaGyms$Name, function(Name) {
          if (Name == "Your Original Location") {
            "green"
            
          } else if (Name == "Minimized Location") {
            "blue"
            
          } else if(Name == "Maximized Location") {
            "red"
            
          } else if (Name == "Combo-Optimized Location"){
            "purple"
            
          } else {
            "beige"
            
          }
        })
      }
      
      # changes the color of the pedestrian traffic points depending on magnitude of traffic
      
      getTrafficColor <- function(webImport) {
        sapply(webImport$pedestrianVolume, function(pedestrianVolume) {
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
      
      })
      # end of the render leaflet
    })
    
    leafletProxy("mymap",session = shiny::getDefaultReactiveDomain(),data = pedestrianTraffic,deferUntilFlush = TRUE) %>%
      addTiles() %>%
      addCircleMarkers(radius = ~ pedestrianVolume / 1000,lng = ~ Longitude,lat = ~ Latitude, weight = 5, color = "red", stroke = FALSE, fillOpacity = 0.3)
    
  
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