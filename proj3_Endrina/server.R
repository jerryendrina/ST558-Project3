# Load needed libraries
#library(shiny)
#library(shinyjs)
#library(tidyverse)
#library(readxl)
#library(caret)
#library(plotly)
#library(DT)
#library(ggfortify)
#library(MASS)
#library(rpart.plot)
#library(rpart)
#library(class)

# Read in data and subset it
#data <- read_excel("../COVID-19-Constructed-Dataset.xlsx")
#data <- data %>% mutate(mathPassFail = ifelse(mathscoreSL < 60, "Fail", "Pass")) %>%
#    dplyr::select(-c(studentID, mathscoreSL))
#cols <- c(1:4, 6:10, 16)
#data[cols] <-lapply(data[cols], factor)



library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})