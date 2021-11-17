# Load needed libraries
library(shiny)
library(tidyverse)
library(readxl)
library(plotly)


# Read in data and subset it
data <- read_excel("../COVID-19-Constructed-Dataset.xlsx")
data <- data %>% mutate(mathPassFail = ifelse(mathscoreSL < 60, "Fail", "Pass")) %>%
    dplyr::select(-c(studentID, mathscoreSL))
cols <- c(1:4, 6, 9:10, 16)
data[cols] <-lapply(data[cols], factor)


#create histogram









# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
#2nd Tab: DATA EXPLORATION

  output$explorePlot <- renderPlotly({
    if(input$plottype == 'hist'){
      #generate histogram
      if(input$histvar == "householdincome"){
        plot_ly(x=~data$householdincome, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Household Income </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      } else if(input$histvar == "readingscore"){
        plot_ly(x=~data$readingscore, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Reading Score </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      } else if(input$histvar == "writingscore"){
        plot_ly(x=~data$writingscore, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Writing Score </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      } else if(input$histvar == "mathscore"){
        plot_ly(x=~data$mathscore, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Math Score </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      } else if(input$histvar == "readingscoreSL"){
        plot_ly(x=~data$readingscoreSL, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Reading Score State Level </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      } else if(input$histvar == "writingscoreSL"){
        plot_ly(x=~data$writingscoreSL, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Writing Score State Level </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      }
      
    } else if(input$plottype == 'bar'){
      #create regular bar plot
        if (input$barcolor == FALSE){
          if(input$barvar == 'freelunch'){
            plot_ly(data = data, x=~freelunch, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Lunch Status </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'covidpos'){
            plot_ly(data = data, x=~covidpos, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Covid Status </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'numcomputers'){
            plot_ly(data = data, x=~numcomputers, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Number of Computers </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'familysize'){
            plot_ly(data = data, x=~familysize, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Family Size </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          }
        }
      
      
    } 
    
    
  })
  
})





