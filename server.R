library(shiny)
library(leaflet)

shinyServer(
  function(input, output, session){
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -79.3832, lat = 43.6532, zoom=12) %>%
        addMarkers(lng = -79.3832, lat = 43.6532, popup = "City of Toronto")
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