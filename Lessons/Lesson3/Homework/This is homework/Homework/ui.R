library(shiny)
library(ggplot2)
library(dplyr)




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Domáca úloha"),
  
                
    sidebarPanel(
      selectInput("select", label = h3("Farbičky podľa"), 
                  choices = list("Region" = "Region", "Unit" = "Unit",
                                 "Segment" = "Segment", "Business" = "Business",
                                 "Year" = "Year"), 
                  selected = 1)
    ),
    
    mainPanel(plotOutput("plot")),
  
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    hr()
  
  
))
