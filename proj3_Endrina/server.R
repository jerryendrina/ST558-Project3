# Load needed libraries
library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)


# Read in data and subset it
data <- read_excel("../COVID-19-Constructed-Dataset.xlsx")

data <- data %>% mutate(mathPassFail = ifelse(mathscoreSL < 60, "Fail", "Pass")) %>%
  dplyr::select(-c(studentID, mathscoreSL))

data <- data %>% mutate(school=if_else(school==0, "Wealthy", "Poor"),
                        gender=if_else(gender==1, "Male", "Female"),
                        covidpos=if_else(covidpos==1, "Positive", "Negative"),
                        freelunch=if_else(freelunch==1, "EatsFreeLunch", "PaysForLunch"),
                        fathereduc=if_else(fathereduc==4, "PhD", 
                                           if_else(fathereduc==3, "Masters", 
                                                   if_else(fathereduc==2, "Bachelor",
                                                           if_else(fathereduc==1, "HSDiploma",
                                                                   "NoHSDiploma")))),
                        mothereduc=if_else(mothereduc==4, "PhD", 
                                           if_else(mothereduc==3, "Masters", 
                                                   if_else(mothereduc==2, "Bachelor",
                                                           if_else(mothereduc==1, "HSDiploma",
                                                                   "NoHSDiploma"))))
)

cols <- c(1:4, 6, 9:10, 16)
data[cols] <-lapply(data[cols], factor)

colNames <- names(data)

#split data into training and test set









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
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Covid Status </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "school"){
            plot_ly(data = data, x = ~school, type = "histogram", 
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> School Type </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "gradelevel"){
            plot_ly(data = data, x = ~gradelevel, type = "histogram", 
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Grade Level </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "gender"){
            plot_ly(data = data, x = ~gender, type = "histogram", 
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Gender </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "fathereduc"){
            plot_ly(data = data, x = ~fathereduc, type = "histogram", 
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Fathers Education </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          } else if(input$barvar == "mothereduc"){
            plot_ly(data = data, x = ~mothereduc, type = "histogram", 
                    color = ~mathPassFail, colors = c("#5ab4ac", "#d8b365")) %>%
              layout(xaxis = list(title = list(text='<b> Mothers Education </b>')),
                     yaxis = list(title = list(text='<b> Frequency </b>')),
                     legend = list(x = 1, y = 0.9, title=list(text='<b> Pass/Fail? </b>')))
          }
        }
      
    } else{
      #generate scatter plot
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
  #Numeric Summary Table
  output$exploreSummary <- renderDataTable({
    if(input$plottype == 'hist'){
      if(input$histvar == "householdincome"){
        data %>% dplyr::select(householdincome, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(householdincome),2), Q1=round(quantile(householdincome,0.25),2),
                           Median=round(median(householdincome),2), Mean=round(mean(householdincome), 2),
                           Q3 = round(quantile(householdincome,.75),2), Max=round(max(householdincome),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else if (input$histvar == "readingscore"){
        data %>% dplyr::select(readingscore, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(readingscore),2), Q1=round(quantile(readingscore,0.25),2),
                           Median=round(median(readingscore),2), Mean=round(mean(readingscore), 2),
                           Q3 = round(quantile(readingscore,.75),2), Max=round(max(readingscore),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else if (input$histvar == "writingscore"){
        data %>% dplyr::select(writingscore, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(writingscore),2), Q1=round(quantile(writingscore,0.25),2),
                           Median=round(median(writingscore),2), Mean=round(mean(writingscore), 2),
                           Q3 = round(quantile(writingscore,.75),2), Max=round(max(writingscore),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else if (input$histvar == "mathscore"){
        data %>% dplyr::select(mathscore, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(mathscore),2), Q1=round(quantile(mathscore,0.25),2),
                           Median=round(median(mathscore),2), Mean=round(mean(mathscore), 2),
                           Q3 = round(quantile(mathscore,.75),2), Max=round(max(mathscore),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else if (input$histvar == "readingscoreSL"){
        data %>% dplyr::select(readingscoreSL, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(readingscoreSL),2), Q1=round(quantile(readingscoreSL,0.25),2),
                           Median=round(median(readingscoreSL),2), Mean=round(mean(readingscoreSL), 2),
                           Q3 = round(quantile(readingscoreSL,.75),2), Max=round(max(readingscoreSL),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else if (input$histvar == "writingscoreSL"){
        data %>% dplyr::select(writingscoreSL, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(writingscoreSL),2), Q1=round(quantile(writingscoreSL,0.25),2),
                           Median=round(median(writingscoreSL),2), Mean=round(mean(writingscoreSL), 2),
                           Q3 = round(quantile(writingscoreSL,.75),2), Max=round(max(writingscoreSL),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else if (input$histvar == "numcomputers"){
        data %>% dplyr::select(numcomputers, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(numcomputers),2), Q1=round(quantile(numcomputers,0.25),2),
                           Median=round(median(numcomputers),2), Mean=round(mean(numcomputers), 2),
                           Q3 = round(quantile(numcomputers,.75),2), Max=round(max(numcomputers),2)) %>%
          datatable(rownames=FALSE, class="compact")
      } else {
        data %>% dplyr::select(familysize, mathPassFail) %>% dplyr::group_by(mathPassFail) %>%
          dplyr::summarize(Min=round(min(familysize),2), Q1=round(quantile(familysize,0.25),2),
                           Median=round(median(familysize),2), Mean=round(mean(familysize), 2),
                           Q3 = round(quantile(familysize,.75),2), Max=round(max(familysize),2)) %>%
          datatable(rownames=FALSE, class="compact")
      }
    } else if (input$plottype == "bar"){
      if(input$barvar == "freelunch"){
        table(data$freelunch, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("Lunch Status", "Pass/Fail", "Count"))
      } else if(input$barvar == "covidpos"){
        table(data$covidpos, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("Covid Status", "Pass/Fail", "Count"))
      } else if(input$barvar == "school"){
        table(data$school, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("School Type", "Pass/Fail", "Count"))
      } else if(input$barvar == "gradelevel"){
        table(data$gradelevel, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("Grade Level", "Pass/Fail", "Count"))
      } else if(input$barvar == "gender"){
        table(data$gender, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("Gender", "Pass/Fail", "Count"))
      } else if(input$barvar == "fathereduc"){
        table(data$fathereduc, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("Gender", "Pass/Fail", "Count"))
      } else {
        table(data$mothereduc, data$mathPassFail) %>%
          datatable(rownames=FALSE, class="compact",
                    colnames=c("Gender", "Pass/Fail", "Count"))
      }
      
    }
    
  })
  
  #DATA SET TAB
  data.out <- reactive({
    data[input$rows[1]:input$rows[2], input$cols]
  })
  
  output$fulldata <- renderDataTable({
    data.out()
  })
  
  output$download <- downloadHandler(
    filename = function(){"data.csv"},
    content = function(fname){
      write.csv(data.out(), fname)
    })
  
})





