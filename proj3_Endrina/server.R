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
library(caret)
library(ggplot2)



#read-in data
cancer <- read_excel("../data_breast_cancer.xlsx") %>% as_tibble() %>%
  dplyr::select(-c(id, ends_with("se"), ends_with("worst")))

cancer$diagnosis <- as.factor(cancer$diagnosis)
colNames2 <-names(cancer[,2:10])







# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
################### 2nd Tab: DATA EXPLORATION ##########################
  
  
  

 

  #scatter plot data
  scatData <- reactive({
    cancer[ , c(input$xVar, input$yVar)]
  })
  
  #histogram data
  histData <- reactive({
   cancer[ ,c(input$histVar, "diagnosis")]
    #x[ ,1] <- scale(x[ ,1])
    #x
  })
  
  #generate plot
  output$graph <- renderPlot({

    if(input$plotType == 'scatterPlot'){
      dataScat <- scatData()
      #base plotting
      g <- ggplot(dataScat, aes(x = !!rlang::sym(input$xVar), 
                                y = !!rlang::sym(input$yVar)))
      
      #add smooth line
      if (input$geomSmooth){
        g + geom_point() + geom_smooth()
      } else{
        g + geom_point()
      }
      
    } else {
      
      if(input$scale){
        
        dataHist <- histData()
        dataHist[, 1] <- scale(dataHist[ ,1])

        if(input$fill){
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar),
                               fill=!!rlang::sym("diagnosis"))) +
            geom_histogram(binwidth = input$bins)
        } else {
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar))) +
            geom_histogram(binwidth = input$bins)
         }
        
      } else {
        
        dataHist <- histData()

        if(input$fill){
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar),
                               fill=!!rlang::sym("diagnosis"))) +
            geom_histogram(binwidth = input$bins)
        } else {
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar))) +
            geom_histogram(binwidth = input$bins)
        }
      }
    }
  })
  
  
  #Numeric Summary Table
  output$numSummary <- renderDataTable({
    
    if(input$fill){
      
      if(input$scale){
        dataHist <- histData()
        dataHist[ ,1] <- scale(dataHist[, 1])
        dataHist %>% group_by(diagnosis) %>%
          summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                    Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                    Median = round(median(!!rlang::sym(input$histVar)), 2), 
                    Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                    Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                    Max = round(max(!!rlang::sym(input$histVar))), 2)
        
        
      } else {
        
        dataHist <- histData()
        dataHist %>% group_by(diagnosis) %>%
          summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                    Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                    Median = round(median(!!rlang::sym(input$histVar)), 2), 
                    Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                    Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                    Max = round(max(!!rlang::sym(input$histVar))), 2)
        
      }
      
    } else {
      
      if(input$scale){
        
        dataHist <- histData()
        dataHist[ ,1] <- scale(dataHist[, 1])
        dataHist %>% 
          summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                    Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                    Median = round(median(!!rlang::sym(input$histVar)), 2), 
                    Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                    Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                    Max = round(max(!!rlang::sym(input$histVar))), 2) 
        
        
      } else {
        
        dataHist <- histData()
        dataHist %>% 
          summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                    Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                    Median = round(median(!!rlang::sym(input$histVar)), 2), 
                    Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                    Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                    Max = round(max(!!rlang::sym(input$histVar))), 2) 
        
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
      max = 0.1,
      value = 0
    )
  })
  
  #input box for max # of cp in the tree
  output$maxCpInput <- renderUI({
    minCp <- input$minCp
    value <- 0.1
    if(minCp > value){
      value <- minCp
    }
    numericInput(
      inputId = "maxCp",
      label = "Maximum",
      min = minCp,
      max = 0.1,
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
    Cps <- seq(minCp, maxCp, 0.001)

    #random forest mtrys
    randForMtry <- as.numeric(input$randForMtry)
    
    #set seed
    set.seed(143)
    
    #testing indexes
    testInd <- createDataPartition(
      cancer$diagnosis, 
      p=1-input$proTesting,
      list = F)
    
    #split data
    train <- cancer[-testInd, ]
    test <- cancer[testInd, ]
    colnames(train) <- make.names(colnames(train))
    
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
      diagnosis ~ . , 
      data = train[, c(c("diagnosis"), logRegVars)],
      method = "glm",
      family = "binomial",
      trControl = TrControl
      )
    
    #increment progress bar and update detail in text
    progress$inc(0.4, detail = "Fitting Classification Tree Model")
    
    #classification tree using cv
     treeModel = train(
       diagnosis ~ . ,
       data = train[, c(c("diagnosis"), treeVars)],
       method = "rpart",
       tuneGrid = expand.grid(cp = Cps),
       trControl = TrControl
     )

    #increment progress bar and update detail in text
     progress$inc(0.6, detail = "Fitting Random Forest Model")
    
    #random forest using cv
    rfModel <- train(
      diagnosis ~.,
      data = train[, c(c("diagnosis"), randForVars)],
      method = "rf",
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
      mean(logRegPreds == test$diagnosis, na.rm=TRUE),
      mean(treePreds == test$diagnosis, na.rm=TRUE),
      mean(randForPreds == test$diagnosis, na.rm=TRUE)
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
      mutate_all(round, digits = 4) %>%
      mutate_all(paste0, sep="%")

    #output accuracy rates table
    output$accTableOutput <- renderDataTable({
      datatable(accTable)
    })

    # #output for logistic regression summary
     output$logRegSummary <- renderDataTable({
       round(as.data.frame(summary(logRegModel)$coef), 4)
     })
    
    #create tree diagram
    output$treeSummary <- renderPlot({
      fancyRpartPlot(treeModel$finalModel)
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
    
    #loop through the vars and create numeric input boxes for each.
    tags$ul(tagList(
      lapply(logRegVars, function(variable){
        numericInput(
          inputId = paste0(variable, "value"),
          label = paste0(variable),
          value = round(median(pull(cancer[ ,variable]), na.rm=TRUE), 2),
          step = 0.1)
      })
    ))
  })
  
  ## Class Tree Inputs ##
  output$treePredInputs <- renderUI({
    
    #get variables to use for each model
    treeVars <- input$treeVars
    
    #loop through the vars and create numeric input boxes for each.
    tags$ul(tagList(
      lapply(treeVars, function(variable){
           numericInput(
             inputId = paste0(variable, "value"),
             label = paste0(variable),
             value = round(median(pull(cancer[ ,variable]), na.rm=TRUE), 2),
             step = 0.1)
      })
    ))
  })
  
  
  ## Random Forest Inputs ##
  output$randForPredInputs <- renderUI({
    
    #get variables to use for each model
    randForVars <- input$randForVars
    
    #loop through the vars and create numeric input boxes for each.
    tags$ul(tagList(
      lapply(randForVars, function(variable){
        numericInput(
          inputId = paste0(variable, "value"),
          label = paste0(variable),
          value = round(median(pull(cancer[ ,variable]), na.rm=TRUE), 2),
          step = 0.1)
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
    preds <- cbind(classPred, round(probPred, 4))
    
    #add column names
    colnames(preds) <- c(
      "Prediction",
      "Predicted Benign Probability",
      "Predicted Malignant Probability "
    )
    
    #convert preds matrix to data frame
    preds <- as.data.frame(preds)
    
    #return predictions
    output$preds <- renderDataTable({
      preds
    })
    
  })
  

  
  
  
  
  
  ################ 4th Tab: DATA SET  #####################

  dataFilter <- reactive({
    vars <- unlist(input$cols)
    
    if (input$filter == "all"){
      cancer%>% select(c("diagnosis", vars))
    } else {
      cancer%>% select(c("diagnosis", vars)) %>% filter(diagnosis == input$filter)
    }
  })
  
  
  output$fulldata <- renderDataTable({
    
    dataFilter()

  })
  
  output$download <- downloadHandler(
    filename = function(){"cancer_data.csv"}, 
    content = function(fname){
      write.csv(dataFilter(), fname)
    })
  
  #return output
  return(output)
  
})





