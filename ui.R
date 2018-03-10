library(shiny)
library(utils)
library(ggmap)
library(ggplot2)
library(leaflet)

#Change the wd to where you have the folder with your UI and Server files
setwd("C:/Users/matth/Desktop/shiny_tests")

shinyUI(
    pageWithSidebar(
    headerPanel("sexy scheduler"),
    
    sidebarPanel(
      #selectInput("Distribution", "Please select Distribution Type",
      #            choices=c("Normal", "Exponential")),
      #sliderInput("sampleSize", "Please Select Sample Size: ",
      #            min=100, max=5000, value=1000, step=100),
      #conditionalPanel(condition = "input.Distribution == 'Normal' ",
      #                 textInput("mean", "Please select the mean", 10),
      #                 textInput( "sd", "Please select the standard deviation", 3)),
      #conditionalPanel(condition = "input.Distribution == 'Exponential' ",
      #                 textInput("lambda", "Please Select Eponential Lambda: ", 1)),
      textInput("address", h4("Your studio address"),
                      value = "Enter address")
    ),
    mainPanel(
      #plotOutput("testPlot"),
      leafletOutput("mymap")
      #murder<-subset(crime, offense == "murder"),
      #qmplot(lon, lat, data = murder, colour = I('red'), size = I(3), darken =.3)
    )
  )
)