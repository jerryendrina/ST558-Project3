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

#rename levels of factors
levels(data$school) <- list(Wealthy="0", Poor="1")
levels(data$gender) <- list(Male="1", Female="0")
levels(data$covidpos) <- list(Positive="1", Negative="0")
levels(data$freelunch) <- list(EatsFreeLunch="1", PaysForLunch = "0")
levels(data$fathereduc) <- list(PhD="4", Masters="3", Bachelor="2", HSDiploma="1", NoHSDiploma="0")
levels(data$mothereduc) <- list(PhD="4", Masters="3", Bachelor="2", HSDiploma="1", NoHSDiploma="0")

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
      } else if(input$histvar == "numcomputers"){
        plot_ly(x=~data$numcomputers, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Number of Computers </b>')),
                 yaxis=list(title=list(text='<b> Frequency </b>')))
      } else if(input$histvar == "familysize"){
        plot_ly(x=~data$familysize, type="histogram", nbinsx = input$bins) %>% 
          layout(xaxis=list(title=list(text='<b> Family Size </b>')),
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
          } else if(input$barvar == 'school'){
            plot_ly(data = data, x=~school, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> School Type </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'gradelevel'){
            plot_ly(data = data, x=~gradelevel, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Grade Level </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'gender'){
            plot_ly(data = data, x=~gender, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Gender </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'fathereduc'){
            plot_ly(data = data, x=~fathereduc, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Fathers Education </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } else if(input$barvar == 'mothereduc'){
            plot_ly(data = data, x=~mothereduc, type="histogram") %>%
              layout(xaxis = list(title = list(text='<b> Mothers Education </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')))
          } 
          
          
        } else{
          #Group bars by pass/fail state level test
          if(input$barvar == "freelunch"){
            plot_ly(data = data, x = ~freelunch, type = "histogram", 
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Lunch Status </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>'))) 
          } else if(input$barvar == "covidpos"){
            plot_ly(data = data, x = ~covidpos, type = "histogram", 
                    color = mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Covid Status </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "school"){
            plot_ly(data = data, x = ~school, type = "histogram", 
                    color = mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> School Type </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "gradelevel"){
            plot_ly(data = data, x = ~gradelevel, type = "histogram", 
                    color = mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Grade Level </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "gender"){
            plot_ly(data = data, x = ~gender, type = "histogram", 
                    color = mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Gender </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "fathereduc"){
            plot_ly(data = data, x = ~fathereduc, type = "histogram", 
                    color = mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Fathers Education </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "mothereduc"){
            plot_ly(data = data, x = ~mothereduc, type = "histogram", 
                    color = mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Mothers Education </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          }
        }
      
    } else{
      #generte scatter plot
      if(input$scatcolor == FALSE){
        plot_ly(data=data, x=~mathscore, y=~householdincome, type="scatter",
                mode="markers", marker=list(size=4)) %>%
          layout(xaxis = list(title = list(text='<b> Math Score in Class </b>')),
                 yaxis = list(title = list(text='<b> Household Income </b>')))
      } else {
        plot_ly(data = data, x = ~mathscore, y = ~householdincome, type = "scatter",
                mode = "markers", marker = list(size = 4),
                color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
          layout(xaxis = list(title = list(text='<b> Math Score in Class </b>')),
                 yaxis = list(title = list(text='<b> Household Income </b>')),
                 legend = list(x = .8, y = 0.9, title=list(text='<b>  Pass/Fail? </b>')))  
      }
      
      
    }
    
    
  })
  
})





