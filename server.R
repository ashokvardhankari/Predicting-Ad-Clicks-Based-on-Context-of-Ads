
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(rCharts)
library(ggplot2)
CatPos <- read.csv("https://raw.githubusercontent.com/karthikskumar/avitoDataDump/RData-File/Cat_Pos_IsClick1.csv")
CatPos <- CatPos %>% group_by(CategoryID,Position) %>% summarise(Clicks=sum(IsClick))
avg <- mean(CatPos$Clicks)
# CatPos$Clicks <- as.factor(CatPos$Clicks)

shinyServer(function(input, output) {
  
  # output$CatPos <- renderTable(as.data.frame(CatPos[,-1],header=T))
  
  catInput <- reactive({
    input$cat
  })
  
  posInput <-  reactive({
    input$pos
  })
  
  output$plot1 <- renderPlot({
  
    CatPos <- subset(CatPos, CategoryID==catInput())
    CatPos <- subset(CatPos, Position==posInput())
    
    p1 <- ggplot(CatPos, aes(Position,Clicks)) +
          geom_bar(stat="identity", width = 0.33, size=2, color="red", fill="#3b9fe6") +
          theme_bw() +
          ylim(0,10000) +
          # xlim(unique(CatPos$Position)-1,unique(CatPos$Position)+1) +
          scale_x_discrete("Position") + 
          geom_hline(yintercept = avg, size = 1, linetype="dashed") +
          ggtitle(" ")
          # annotate("text", label = "Mean Probability", y = avg+300)
    # p1 = mPlot(x = 'Position', y = list('Clicks'), data = CatPos, type = 'Bar', labels = list("Clicks"))
    # p1$addParams(dom = 'plot1')
    print(p1)
  })

})
