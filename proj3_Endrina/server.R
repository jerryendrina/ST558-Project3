# Load needed libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(imager)
library(readxl)
library(plotly)
library(DT)
library(caret)
library(rattle)
library(rpart.plot)


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
  
################### 2nd Tab: DATA EXPLORATION ##########################

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
  
  
  ################ 3rd Tab: MODELING #######################
  
  #log reg formula
  output$logRegEx <- renderUI({
    withMathJax(
      helpText(
        "$$\\ln(\\frac{p_i}{1-p_i}) = \\beta_0 + \\Sigma^k_{j=1}\\beta_jx_{ij}$$"))
  })
  
  #input box for min # of cp in the tree
  output$minCpInput <- renderUI({
    numericInput(
      inputId = "minCp",
      label = "Minimum",
      min = 0,
      max = 1000,
      value = 0.01
    )
  })
  
  #input box for max # of cp in the tree
  output$maxCpInput <- renderUI({
    minCp <- input$minCp
    value <- 10
    if(minCp > value){
      value <- minCp
    }
    numericInput(
      inputId = "maxCp",
      label = "Maximum",
      min = minCp,
      max = 1000,
      value = value)
  })
  
  ###modeling training###
  
  observeEvent(input$trainStart,{
    
    #progress object
    progress <- Progress$new()
    #closes when reacive is exited 
    on.exit(progress$close())
    #message to user
    progress$set(message = "Running Cross-Validation", value = 0)
    
    #variables to use for each model
    logRegVars <- unlist(input$logRegVars)
    treeVars <- unlist(input$treeVars)
    randForVars <- unlist(input$randForVars)
    
    #proportion of testing and k-folds params
    set.seed(143)
    propTesting <- input$propTesting
    numFolds <- input$numFolds
    
    #cps to try
    minCp <- input$minCp
    maxCp <- input$maxCp
    numCps <- input$numCps
    Cps <- seq(minCp, maxCp, length.out=numCps)

    #random forest mtrys
    randForMtry <- as.numeric(input$randForMtry)
    
    #set seed
    set.seed(143)
    
    #testing indexes
    testInd <- createDataPartition(
      data$mathPassFail, 
      p=1-input$proTesting,
      list = F)
    
    #split data
    train <- data[-testInd, ]
    test <- data[testInd, ]
    
    #suppress any warning in the modeling process
    suppressWarnings(library(caret))
    
    #set train controls
    TrControl <- trainControl(
      method = "cv",
      number = numFolds
    )
    
    #increment progress bar and update detail in text
    progress$inc(0.2, detail = "Fitting Logistic Regression Model")
    
    #logistic regression using cv
    logRegModel <- train(
      mathPassFail ~ . , 
      data = train[, c(c("mathPassFail"), logRegVars)],
      method = "glm",
      family = "binomial",
      metric = "Accuracy",
      trControl = TrControl
      )
    
    #increment progress bar and update detail in text
    progress$inc(0.4, detail = "Fitting Classification Tree Model")
    
    #classification tree using cv
     treeModel = train(
       mathPassFail ~ . ,
       data = train[, c(c("mathPassFail"), treeVars)],
       method = "rpart",
       metric = "Accuracy",
       tuneGrid = expand.grid(cp = Cps),
       trControl = TrControl
     )

    #increment progress bar and update detail in text
     progress$inc(0.6, detail = "Fitting Random Forest Model")
    
    #random forest using cv
    rfModel <- train(
      mathPassFail ~.,
      data = train[, c(c("mathPassFail"), randForVars)],
      method = "rf",
      metric = "Accuracy",
      tuneGrid = expand.grid(mtry = randForMtry),
      trControl = TrControl
    )
    
    #increment progress bar and update detail in text
    progress$inc(0.8, detail = "Evaluating Performance of Models using Test Set")


    #test predictions
    logRegPreds <- predict(logRegModel, test, type="raw")
    treePreds <- predict(treeModel, test, type="raw")
    randForPreds <- predict(rfModel, test, type="raw")
    
    
    
    #test set accuracy rates
    accVec <- c(
      mean(logRegPreds == test$mathPassFail, na.rm=TRUE),
      mean(treePreds == test$mathPassFail, na.rm=TRUE),
      mean(randForPreds == test$mathPassFail, na.rm=TRUE)
    )

    #convert to percentages in a matrix
    accMatrix <- t(as.matrix(accVec)) * 100
    colnames(accMatrix) <- c(
      "Logistic Regression",
      paste0("Tree (Cp = ", treeModel$bestTune$cp, ")"),
      paste0("Random Forest (mtry = ", rfModel$bestTune$mtry, ")")
    )
    
    #convert to dataframe
    accTable <- as.data.frame(accMatrix) %>%
      mutate_all(round, digits = 3) %>%
      mutate_all(paste0, sep="%")

    #output accuracy rates table
    output$accTableOutput <- renderDataTable({
      datatable(accTable)
    })

    # #output for logistic regression summary
     output$logRegSummary <- renderDataTable({
       round(as.data.frame(summary(logRegModel)$coef), 3)
     })
    
    #create tree diagram
    output$treeSummary <- renderPlot({
       rpart.plot(treeModel$finalModel, 
                  box.palette="RdBu", 
                  shadow.col="gray", 
                  nn=TRUE)
     })
    
    #output feature importance plot for random forest
    output$rfVarImpPlot <- renderPlot({
      ggplot(varImp(rfModel, type=2)) +
        geom_col(fill="blue") +
        ggtitle("Variable Importance in Random Forest")
    })

    #save fitted models in a folder
    saveRDS(logRegModel, "../Fitted Models/logRegModel.rds")
    saveRDS(treeModel, "../Fitted Models/treeModel.rds")
    saveRDS(rfModel, "../Fitted Models/rfModel.rds")
    
  })
  
  #### Modeling: Prediction ####
  
  ## Log Reg Input ##
  output$logRegPredInputs <- renderUI({
    
    #get variables to use for each model
    logRegVars <- input$logRegVars
    
    #list of numeric variables
    numeric <- c("householdincome", "numcomputers", "familysize", "readingscore", 
                 "writingscore", "mathscore", "readingscoreSL", "writingscoreSL") 

    #loop through the vars and create numeric input boxes for each.
    tags$ul(tagList(
      lapply(logRegVars, function(variable){
        if(variable %in% numeric){
          numericInput(
            inputId = paste0(variable, "value"),
            label = paste0(variable),
            value = round(median(pull(data[ ,variable]), na.rm=TRUE),2),
            step = 0.1)
        } else if(variable == "school"){
          numericInput(
            inputId = "school value",
            label = "school: wealthy=0, poor=1",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "gradelevel"){
          numericInput(
            inputId = "gradelevel value",
            label = "gradelevel",
            min = 6,
            max = 12,
            value = 6)
        } else if(variable == "gender"){
          numericInput(
            inputId = "gender value",
            label = "gender: male=1, female=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "covidpos"){
          numericInput(
            inputId = "covidpos value",
            label = "covidpos: positive=1, negative=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "freelunch"){
          numericInput(
            inputId = "freelunch value",
            label = "freelunch: free=1, pay=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "fathereduc"){
          numericInput(
            inputId = "fathereduc value",
            label = "fathereduc: none=0,HS=1,Bach.=2, Masters=3,PhD=4",
            min = 0,
            max = 4,
            value = 1)
        } else if(variable == "mothereduc") {
          numericInput(
            inputId = "mothereduc value",
            label = "mothereduc: none=0,HS=1,Bach.=2, Masters=3,PhD=4",
            min = 0,
            max = 4,
            value = 1)
        }
      })
    ))
  })
  
  ## Class Tree Inputs ##
  output$treePredInputs <- renderUI({
    
    #get variables to use for each model
    treeVars <- input$treeVars
    
    #list of numeric variables
    numeric <- c("householdincome", "numcomputers", "familysize", "readingscore", 
                 "writingscore", "mathscore", "readingscoreSL", "writingscoreSL") 
    
    #loop through the vars and create numeric input boxes for each.
    tags$ul(tagList(
      lapply(treeVars, function(variable){
        if(variable %in% numeric){
          numericInput(
            inputId = paste0(variable, "value"),
            label = paste0(variable),
            value = round(median(pull(data[ ,variable]), na.rm=TRUE),2),
            step = 0.1)
        } else if(variable == "school"){
          numericInput(
            inputId = "school value",
            label = "school: wealthy=0, poor=1",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "gradelevel"){
          numericInput(
            inputId = "gradelevel value",
            label = "gradelevel",
            min = 6,
            max = 12,
            value = 6)
        } else if(variable == "gender"){
          numericInput(
            inputId = "gender value",
            label = "gender: male=1, female=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "covidpos"){
          numericInput(
            inputId = "covidpos value",
            label = "covidpos: positive=1, negative=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "freelunch"){
          numericInput(
            inputId = "freelunch value",
            label = "freelunch: free=1, pay=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "fathereduc"){
          numericInput(
            inputId = "fathereduc value",
            label = "fathereduc: none=0,HS=1,Bach.=2, Masters=3,PhD=4",
            min = 0,
            max = 4,
            value = 1)
        } else if(variable == "mothereduc") {
          numericInput(
            inputId = "mothereduc value",
            label = "mothereduc: none=0,HS=1,Bach.=2, Masters=3,PhD=4",
            min = 0,
            max = 4,
            value = 1)
        }
      })
    ))
  })
  
  
  ## Random Forest Inputs ##
  output$randForPredInputs <- renderUI({
    
    #get variables to use for each model
    randForVars <- input$randForVars
    
    #list of numeric variables
    numeric <- c("householdincome", "numcomputers", "familysize", "readingscore", 
                 "writingscore", "mathscore", "readingscoreSL", "writingscoreSL") 
    
    #loop through the vars and create numeric input boxes for each.
    tags$ul(tagList(
      lapply(randForVars, function(variable){
        if(variable %in% numeric){
          numericInput(
            inputId = paste0(variable, "value"),
            label = paste0(variable),
            value = round(median(pull(data[ ,variable]), na.rm=TRUE),2),
            step = 0.1)
        } else if(variable == "school"){
          numericInput(
            inputId = "school value",
            label = "school: wealthy=0, poor=1",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "gradelevel"){
          numericInput(
            inputId = "gradelevel value",
            label = "gradelevel",
            min = 6,
            max = 12,
            value = 6)
        } else if(variable == "gender"){
          numericInput(
            inputId = "gender value",
            label = "gender: male=1, female=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "covidpos"){
          numericInput(
            inputId = "covidpos value",
            label = "covidpos: positive=1, negative=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "freelunch"){
          numericInput(
            inputId = "freelunch value",
            label = "freelunch: free=1, pay=0",
            min = 0,
            max = 1,
            value = 1)
        } else if(variable == "fathereduc"){
          numericInput(
            inputId = "fathereduc value",
            label = "fathereduc: none=0,HS=1,Bach.=2, Masters=3,PhD=4",
            min = 0,
            max = 4,
            value = 1)
        } else if(variable == "mothereduc") {
          numericInput(
            inputId = "mothereduc value",
            label = "mothereduc: none=0,HS=1,Bach.=2, Masters=3,PhD=4",
            min = 0,
            max = 4,
            value = 1)
        }
      })
    ))
  })
  
  
  observeEvent(input$predStart, {
    #retrieve model
    modelType <- input$modelType
    
    #load appropriate model based on selection
    if (modelType == "logReg"){
      #get names of variables
      varsOfInterest <- unlist(lapply(input$logRegVars, paste0, sep="value"))
      #load in the log reg model
      myModel <- readRDS("../Fitted Models/logRegModel.rds")
      
    } else if (modelType == "tree"){
      #get names of variables
      varsOfInterest <- unlist(lapply(input$treeVars, paste0, sep="value"))
      #load in the log reg model
      myModel <- readRDS("../Fitted Models/treeModel.rds")
    } else {
      #get names of variables
      varsOfInterest <- unlist(lapply(input$randForVars, paste0, sep="value"))
      #load in the log reg model
      myModel <- readRDS("../Fitted Models/rfModel.rds")
    } 
    
    #loop through the user inputs to create a vector of inputs
    inputCopy <- c()
    for (variable in varsOfInterest){
      inputCopy <- c(inputCopy, input[[variable]])
    }
    #create a 1-row matrix
    inputCopy <- t(matrix(inputCopy))
    
    #column names
    colnames(inputCopy) <- str_remove_all(varsOfInterest, pattern="value")
    
    #create a data.frame
    userInputs <- as.data.frame(inputCopy)
    
    #get class and probability predictions
    classPred <- predict(myModel, userInputs, type="raw")
    probPred <- predict(myModel, userInputs, type="prob")
    
    #combine to single matrix
    preds <- cbind(classPred, round(probPred, 3))
    
    #add column names
    colnames(preds) <- c(
      "Prediction",
      "Predicted Prob. of Failure",
      "Predicted Prob. of Passing"
    )
    
    #convert preds matrix to data frame
    preds <- as.data.frame(preds)
    
    #return predictions
    output$preds <- renderDataTable({
      preds
    })
    
  })
  
  #return output
  return(output)
  
  
  
  
  
  ################ 4th Tab: DATA SET  #####################

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





