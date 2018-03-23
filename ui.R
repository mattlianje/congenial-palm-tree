library(shiny)
library(utils)
library(ggmap)
library(ggplot2)
library(leaflet)
library(lpSolve)
library(plyr)
library(geosphere)
library(shinyjs)

#Change the wd to where you have the folder with your UI and Server files
setwd("C:/Users/matth/Desktop/shiny_tests")

shinyUI(pageWithSidebar(
  headerPanel("protein, water, liquor", windowTitle = "congenial palm tree"),
  
  sidebarPanel(
    # Not using these dd s for the moment nigs
    
    #selectInput("Distribution", "Please select Distribution Type",
    #            choices=c("Normal", "Exponential")),
    #selectInput("endTime", "Please select the time you end work",
    #            choices=c("3 PM", "4 PM", "5 PM", "6 PM", "7 PM")),
    
    # This one is cooking
    h3("Persistence pays off"),
    #img(src = "sakufun.jpg", width = "100%"),
    #sliderInput("workDay", h4("Your work hours..."), min = 0, max = 23, value = c(9, 20), step = 0.5, post = " hrs", sep = ",", animate = TRUE),
    
    # Boiler plate tests with basic R functions
    #conditionalPanel(condition = "input.Distribution == 'Normal' ",
    #                 textInput("mean", "Please select the mean", 10),
    #                 textInput( "sd", "Please select the standard deviation", 3)),
    #conditionalPanel(condition = "input.Distribution == 'Exponential' ",
    #                 textInput("lambda", "Please Select Eponential Lambda: ", 1)),
    
    # textbox that grabs the current user location
    
    textInput("locationType", h5("what type of business do you want to start"),
              value = "yoga studio"),
    textInput("address", h5("where in Toronto do you want to start your business"),
              value = "Yonge and Dundas"),
    sliderInput("distConstraint", h5("Optimize within this distance (in km).."), min = 0, max = 5, value = 3, step = 0.25, post = " km", animate = TRUE),
    #actionButton("load", "load data", style = "color: #6666ff"),
    selectInput("optimType", h5("How do you want our system to determine the optimal location?"), choices = c("Minimize Dist. from Popular Spots", "Maximize Dist. from Competitors", "Minimization/Maximization Combination")),
    
    actionButton("do", "get map", icon("map"), style = "color: #6666ff"),
    actionButton("refresh", "refresh", style = "color: #6666ff")
  ),
  mainPanel(#plotOutput("testPlot"),
    leafletOutput("mymap"))
))