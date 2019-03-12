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

setwd("D:\\GeneralInsurance_Class")
dt <- read.csv("./Data/lesson2_KPI.csv")

dt <- na.omit(dt)
dt <- dt[-(dt$Losses)  < 0,]
dt <- dt[-(dt$Expenses)< 0,]
dt <- dt[-(dt$Premium) < 0,]
dt$LossRatio <- dt$Losses / dt$Premium
dt$ExpRatio  <- dt$Expenses / dt$Premium
dt$ComRatio  <- dt$LossRatio + dt$ExpRatio

shinyServer(function(input, output) {
  output$value <- renderPrint({ input$select })
  
  
  
  output$plot <- renderPlot({
    
    # If it's stupid but it works, it ain't stupid.
    switch (input$select,
      "Region" = {color <- dt$Region},
      "Unit"   = {color <- dt$Unit},
      "Segment" = {color <- dt$Segment},
      "Business" = {color <- dt$Business},
      "Year" = {color <- dt$Year}
    )
    
   # print(color)
    
    ggplot(data = dt,
          mapping = aes(x = LossRatio, y = ExpRatio, colour = color)) +
       geom_point() #+
       #geom_smooth()
      })
})

