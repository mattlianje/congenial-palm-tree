library(shiny)
library(leaflet)
library(ggmap)

shinyServer(
  function(input, output, session){
    
    locationTest <- geocode("241 Combe Ave North York")
    locationLon <- locationTest$lon
    locationLat <- locationTest$lat
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = locationLon, lat = locationLat, zoom=14) %>%
        addMarkers(lng = locationLon, lat = locationLat, popup = "City of Toronto")
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