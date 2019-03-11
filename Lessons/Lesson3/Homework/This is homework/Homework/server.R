#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)


shinyServer(function(input, output) {
  
  data <- reactive({
    setwd("D:\\GeneralInsurance_Class")
    dt_KPI_raw <- read.csv("./Data/lesson2_KPI.csv")
    
    data <- na.omit(dt)
    data <- data[-(data$Losses)  < 0,]
    data <- data[-(data$Expenses)< 0,]
    data <- data[-(data$Premium) < 0,]
    data$LossRatio <- data$Losses / data$Premium
    data$ExpRatio  <- data$Expenses / data$Premium
    data$ComRatio  <- data$LossRatio + data$ExpRatio
  })
  
  output$value <- renderPrint({ input$select })
  
#  output$plot <- renderPlot(plot(c(-1,0,1), c(1,0,1)))
  
  output$plot <- renderPlot(ggplot(data = data(),
                                    mapping = aes(x = Premium, y = Expenses, colour = Region)) +
                               geom_point() +
                               geom_smooth())
})
