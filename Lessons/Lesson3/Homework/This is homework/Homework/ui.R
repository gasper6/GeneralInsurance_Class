library(shiny)
library(ggplot2)
library(dplyr)




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Domáca úloha"),
  
  selectInput("select", label = h3("Vyber kategóriu"), 
              choices = list("Region" = 1, "Unit" = 2,
                             "Segment" = 3, "Business" = 4,
                             "Year" = 5), 
              selected = 1),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  hr(),
  mainPanel(plotOutput("plot"))
  
))
