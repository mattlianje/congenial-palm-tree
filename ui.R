library(shiny)
library(utils)
library(ggmap)
library(ggplot2)
library(leaflet)
library(lpSolve)
library(plyr)
library(geosphere)
#library(shinyjs)

#Change the wd to where you have the folder with your UI and Server files
#setwd("C:/Users/matth/Desktop/shiny_tests")

shinyUI(pageWithSidebar(
  headerPanel("Congenial Palm Tree", windowTitle = "congenial palm tree"),
  
  sidebarPanel(
    h3("Lets get started..."),
    
    # textbox that grabs the current user location
    
    textInput("locationType", h5("Type of business:"),
              value = "ex. yoga studio"),
    textInput("address", h5("Location:"),
              value = "ex. near U of T"),
    selectInput("optimType", h5("Where do you want to be?"), choices = c("Close to popular spots", "Far away from my rivals", "A combination of both")),
    sliderInput("distConstraint", h5("Max. distance from your location (in Km):"), min = 0, max = 5, value = 3, step = 0.25, post = " km", animate = TRUE),
    actionButton("do", "get map", icon("map"), style = "color: #0080ff"),
    actionButton("refresh", "refresh", style = "color: #0080ff")
  ),
  mainPanel(#plotOutput("testPlot"),
    leafletOutput("mymap"))
))