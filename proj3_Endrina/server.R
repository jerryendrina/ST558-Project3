# Load needed libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(imager)
library(readxl)
library(DT)
library(caret)
library(rattle)
library(ggplot2)


#read-in data
cancer <- read_excel("../data_breast_cancer.xlsx") %>% 
  as_tibble() %>%
  dplyr::select(-c(id, ends_with("se"), ends_with("worst")))

#convert diagnosis variable to factors
cancer$diagnosis <- as.factor(cancer$diagnosis)

#create list of names of all predictors
colNames2 <-names(cancer[,2:10])



#Define server
shinyServer(function(input, output, session) {
  
################### 2nd Tab: DATA EXPLORATION ##########################
  
  #scatter plot data
  scatData <- reactive({
    cancer[ , c(input$xVar, input$yVar, "diagnosis")]
  })
  
  #histogram data
  histData <- reactive({
   cancer[ ,c(input$histVar, "diagnosis")]
  })
  
  ####### graphical summary ###########
  output$graph <- renderPlot({

    #generate scatterplot
    if(input$plotType == 'scatterPlot'){
      
      #filter data
      dataScat <- scatData()
      
      #base plot
      g <- ggplot(dataScat, aes(x = !!rlang::sym(input$xVar), 
                                y = !!rlang::sym(input$yVar))) + 
        geom_point(aes(colour="red", alpha=0.6))
      
      #if add regression line is chosen
      if (input$geomSmooth){
        
        #regression line then group by diagnosis
        if (input$groupBy){
          g + geom_smooth(aes(color=!!rlang::sym("diagnosis"))) + 
            geom_point(aes(color=!!rlang::sym("diagnosis")))
        
        #regression line, not grouped by diagnosis
        } else {
          g + geom_smooth() +
            geom_point(aes(colour="red", alpha=0.6))
        }
        
      #if group by diagnosis is chosen
      } else if (input$groupBy) {
        
        #group by diagnosis then add regression line
        if (input$geomSmooth){
          g + geom_smooth(aes(color=!!rlang::sym("diagnosis"))) + 
            geom_point(aes(color=!!rlang::sym("diagnosis")))
        
        #group by diagnosis, no regression line
        } else {
          g + geom_point(aes(color=!!rlang::sym("diagnosis")))
        }
      
      #only base plot
      } else {
        g
      }
      
    #generate histogram 
    } else {
      
      #if scale and center is clicked
      if(input$scale){
        
        #filter data
        dataHist <- histData()
        dataHist[, 1] <- scale(dataHist[ ,1])

        #group data by diagnosis
        if(input$fill){
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar),
                               fill=!!rlang::sym("diagnosis"))) +
            geom_histogram(binwidth = input$bins)
        
        #no grouping
        } else {
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar))) +
            geom_histogram(binwidth = input$bins, fill="#FF6666")
        }
      
      #histogram not centered and scaled
      } else {
        
        #filter data
        dataHist <- histData()
        
        #group by diagnosis
        if(input$fill){
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar),
                               fill=!!rlang::sym("diagnosis"))) +
            geom_histogram(binwidth = input$bins)
        
        #no grouping
        } else {
          ggplot(dataHist, aes(x = !!rlang::sym(input$histVar))) +
            geom_histogram(binwidth = input$bins, fill="#FF6666")
        }
      }
    }
  })
  
  
  ############ numeric summaries #############
  
  output$numSummary <- renderDataTable({
    
    #if scatterplot is clicked
    if (input$plotType == "scatterPlot"){
      
      #group by diagnosis
      if(input$groupBy == FALSE){
        
        #filter data
        dataScat <- scatData()
        
        #generate summaries of chosen x var
        x <- dataScat %>%
          summarise(
                    N = length(!!rlang::sym(input$xVar)),
                    Min = round(min(!!rlang::sym(input$xVar)), 2), 
                    Q1 = round(quantile(!!rlang::sym(input$xVar), .25), 2),
                    Median = round(median(!!rlang::sym(input$xVar)), 2), 
                    Mean = round(mean(!!rlang::sym(input$xVar)), 2), 
                    Q3 = round(quantile(!!rlang::sym(input$xVar), .75), 2), 
                    Max = round(max(!!rlang::sym(input$xVar)), 2)
                    )
        
        #generate summaries of chosen y var
        y <- dataScat %>%
          summarise(
            N = length(!!rlang::sym(input$yVar)),
            Min = round(min(!!rlang::sym(input$yVar)), 2), 
            Q1 = round(quantile(!!rlang::sym(input$yVar), .25), 2),
            Median = round(median(!!rlang::sym(input$yVar)), 2), 
            Mean = round(mean(!!rlang::sym(input$yVar)), 2), 
            Q3 = round(quantile(!!rlang::sym(input$yVar), .75), 2), 
            Max = round(max(!!rlang::sym(input$yVar)), 2)
            )
        
        #bind summaries and convert to data set
        z <- bind_rows(x, y)
        z$Variable <- c(input$xVar, input$yVar)
        z[,c(8,1:7)]
      
      #if histogram is clicked
      } else {
        
        #filter data
        dataScat <- scatData()
        
        #generate summaries of chosen x var
        x <- dataScat %>% group_by(diagnosis) %>%
          summarise(
            N = length(!!rlang::sym(input$xVar)),
            Min = round(min(!!rlang::sym(input$xVar)), 2), 
            Q1 = round(quantile(!!rlang::sym(input$xVar), .25), 2),
            Median = round(median(!!rlang::sym(input$xVar)), 2), 
            Mean = round(mean(!!rlang::sym(input$xVar)), 2), 
            Q3 = round(quantile(!!rlang::sym(input$xVar), .75), 2), 
            Max = round(max(!!rlang::sym(input$xVar)), 2)
          )
        
        #generate summarie of chosen y var
        y <- dataScat %>% group_by(diagnosis) %>%
          summarise(
            N = length(!!rlang::sym(input$yVar)),
            Min = round(min(!!rlang::sym(input$yVar)), 2), 
            Q1 = round(quantile(!!rlang::sym(input$yVar), .25), 2),
            Median = round(median(!!rlang::sym(input$yVar)), 2), 
            Mean = round(mean(!!rlang::sym(input$yVar)), 2), 
            Q3 = round(quantile(!!rlang::sym(input$yVar), .75), 2), 
            Max = round(max(!!rlang::sym(input$yVar)), 2)
          )
        
        #bind summaries and convert to dataframe
        z <- bind_rows(x, y)
        z$Variable <- c(input$xVar, input$xVar, input$yVar, input$yVar)
        z[,c(9,1:8)]
      }
      
    #if histogram is clicked 
    } else {
      
      #group by diagnosis
      if(input$fill){
        
        #center and scale data
        if(input$scale){
          
          #filter data
          dataHist <- histData()
          dataHist[ ,1] <- scale(dataHist[, 1])
          
          #generate summaries of chosen variable
          dataHist %>% group_by(diagnosis) %>%
            summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                      Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                      Median = round(median(!!rlang::sym(input$histVar)), 2), 
                      Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                      Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                      Max = round(max(!!rlang::sym(input$histVar)),2))
        
        #data not centered and scaled
        } else {
          
          #filter data
          dataHist <- histData()
          
          #generate summaries of chosen variable
          dataHist %>% group_by(diagnosis) %>%
            summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                      Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                      Median = round(median(!!rlang::sym(input$histVar)), 2), 
                      Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                      Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                      Max = round(max(!!rlang::sym(input$histVar)),2))
        }
      
      #base plot and summaries
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
                      Max = round(max(!!rlang::sym(input$histVar)),2))
        } else {
          dataHist <- histData()
          dataHist %>% 
            summarise(Min = round(min(!!rlang::sym(input$histVar)), 2), 
                      Q1 = round(quantile(!!rlang::sym(input$histVar), .25), 2),
                      Median = round(median(!!rlang::sym(input$histVar)), 2), 
                      Mean = round(mean(!!rlang::sym(input$histVar)), 2), 
                      Q3 = round(quantile(!!rlang::sym(input$histVar), .75), 2), 
                      Max = round(max(!!rlang::sym(input$histVar)),2))
        }
      }
    }
  })
  
  ############################ 3rd Tab: MODELING ##############################
  
  #log reg formula
  output$logRegFormula <- renderUI({
    withMathJax(
      helpText(
        "$$\\ln(\\frac{p_i}{1-p_i}) = \\beta_0 + \\Sigma^k_{j=1}\\beta_jx_{ij}$$"))
  })
  
  #slider input for min number of cp in the tree model
  output$minCpInput <- renderUI({
    sliderInput(
      inputId = "minCp",
      label = "Minimum 'cp' Value:",
      min = 0,
      max = 0.1,
      step = 0.001,
      value = 0
    )
  })
  
  #slider input for max number of cp in the tree nodel
  output$maxCpInput <- renderUI({
    minCp <- input$minCp
    value <- 0.1
    if(minCp > value){
      value <- minCp
    }
    sliderInput(
      inputId = "maxCp",
      label = "Maximum 'cp' Value:",
      min = minCp,
      max = 0.1,
      step = 0.001,
      value = value)
  })
  
  ###modeling training###
  
  observeEvent(input$trainStart,{
    
    #status bar object
    progress <- Progress$new()
    
    #closes when reactive is exited 
    on.exit(progress$close())
    
    #display a messahe that cross-validation is being done
    progress$set(message = "Running Cross-Validation", value = 0)
    
    #collect variables to use for each model
    logRegVars <- unlist(input$logRegVars)
    treeVars <- unlist(input$treeVars)
    randForVars <- unlist(input$randForVars)
    
    #save proportion of testing and k-folds parameters
    set.seed(143)
    propTesting <- input$propTesting
    numFolds <- input$numFolds
    
    #cps to try
    minCp <- input$minCp
    maxCp <- input$maxCp
    Cps <- seq(minCp, maxCp, 0.001)

    #random forest mtrys
    randForMtry <- as.numeric(input$randForMtry)
    
    #set seed
    set.seed(143)
    
    #create testing indexes
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
    
    #set controls for training
    TrControl <- trainControl(
      method = "cv",
      number = numFolds
    )
    
    #update message display
    progress$inc(0.2, detail = "Fitting Logistic Regression Model")
    
    #logistic regression using cv
    logRegModel <- train(
      diagnosis ~ . , 
      data = train[, c(c("diagnosis"), logRegVars)],
      method = "glm",
      family = "binomial",
      trControl = TrControl
      )
    
    #update message to display
    progress$inc(0.4, detail = "Fitting Classification Tree Model")
    
    #classification tree using cv
     treeModel = train(
       diagnosis ~ . ,
       data = train[, c(c("diagnosis"), treeVars)],
       method = "rpart",
       tuneGrid = expand.grid(cp = Cps),
       trControl = TrControl
     )

    #update message to display
     progress$inc(0.6, detail = "Fitting Random Forest Model")
    
    #random forest using cv
    rfModel <- train(
      diagnosis ~.,
      data = train[, c(c("diagnosis"), randForVars)],
      method = "rf",
      tuneGrid = expand.grid(mtry = randForMtry),
      trControl = TrControl
    )
    
    #update message to display
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

    #generate accuracy rates table
    output$accuracyTableOutput <- renderDataTable({
      datatable(accTable)
    })

    #generate for logistic regression summary
     output$logRegSummary <- renderDataTable({
       round(as.data.frame(summary(logRegModel)$coef), 4)
     })
    
    #generate tree diagram
    output$treeSummary <- renderPlot({
      fancyRpartPlot(treeModel$finalModel)
     })
    
    #generate feature importance plot for random forest
    output$randomForestPlot <- renderPlot({
      ggplot(varImp(rfModel, type=2)) +
        geom_col(fill="blue") +
        ggtitle("Variable Importance in Random Forest")
    })

    #save fitted models in a folder for later access
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
  
  
  ######################### 4th Tab: DATA SET  ################################
  
  #create reactive input var
  dataFilter <- reactive({
    
    #convert all inputs to a list
    vars <- unlist(input$cols)
    
    #do not filter data set
    if (input$filter == "all"){
      cancer%>% select(c("diagnosis", vars))
      
    #filter data set based on specified options
    } else {
      cancer%>% select(c("diagnosis", vars)) %>% filter(diagnosis == input$filter)
    }
  })
  
  #render data
  output$fulldata <- renderDataTable({
    dataFilter()
  })
  
  #create button to download data
  output$download <- downloadHandler(
    filename = function(){"cancer_data.csv"}, 
    content = function(fname){
      write.csv(dataFilter(), fname)
    })
  
  #return output
  return(output)
})





