# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(dplyr)
library(rCharts)
library(ggplot2)
CatPos <- read.csv("https://raw.githubusercontent.com/karthikskumar/avitoDataDump/RData-File/Cat_Pos_IsClick1.csv")
CatPos <- CatPos %>% group_by(CategoryID,Position) %>% summarise(Clicks=sum(IsClick))

shinyUI(fluidPage(
  # Application title
  titlePanel("Expected Number of Clicks for a given Category and Position"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("cat","Category:",
                         CatPos$CategoryID)
      , selectInput("pos", "Position:",
                         CatPos$Position)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1")
    )
  )
))
