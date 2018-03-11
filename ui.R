library(shiny)
library(utils)
library(ggmap)
library(ggplot2)
library(leaflet)

#Change the wd to where you have the folder with your UI and Server files
setwd("C:/Users/matth/Desktop/shiny_tests")

shinyUI(
    pageWithSidebar(
    headerPanel("saku laughter, saku fun"),
    
    sidebarPanel(
      #selectInput("Distribution", "Please select Distribution Type",
      #            choices=c("Normal", "Exponential")),
      selectInput("startTime", "Please select the time you start working",
                   choices=c("7 AM", "8 AM", "9 AM", "10 AM", "11 AM")),
      selectInput("endTime", "Please select the time you end work",
                  choices=c("3 PM", "4 PM", "5 PM", "6 PM", "7 PM")),
      sliderInput("workDay", "Please select start/end times of your workday: ",
                   min=1, max=23, value=c(9,20), step=0.5, post= " hrs", sep = ",", animate = TRUE),
      #conditionalPanel(condition = "input.Distribution == 'Normal' ",
      #                 textInput("mean", "Please select the mean", 10),
      #                 textInput( "sd", "Please select the standard deviation", 3)),
      #conditionalPanel(condition = "input.Distribution == 'Exponential' ",
      #                 textInput("lambda", "Please Select Eponential Lambda: ", 1)),
      textInput("address", h4("Your studio address"),
                      value = "Toronto")
    ),
    mainPanel(
      #plotOutput("testPlot"),
      leafletOutput("mymap")
      #murder<-subset(crime, offense == "murder"),
      #qmplot(lon, lat, data = murder, colour = I('red'), size = I(3), darken =.3)
    )
  )
)