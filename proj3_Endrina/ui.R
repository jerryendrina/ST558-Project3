library(shiny)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(DT)
library(shinythemes)
library(rattle)
library(caret)
library(ggplot2)


#read-in data
cancer <- read_excel("../data_breast_cancer.xlsx") %>% as_tibble() %>%
  dplyr::select(-c(id, ends_with("se"), ends_with("worst")))

#convert diagnosis variable to factors
cancer$diagnosis <- as.factor(cancer$diagnosis)

#create list of names of all predictors
colNames2 <-names(cancer[,2:10])


# Define UI
shinyUI(navbarPage(
  
  #add title
  title = "Breast Cancer Diagnosis Prediction Shiny App",
  #add theme
  theme = shinytheme("united"),
  
  tabsetPanel(
    
    ######################## first tab: about #################################
    tabPanel("About", fluid=TRUE,
             
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 h2("ST558 Final Project"),
                 h3("By: Jeremias Endrina Jr."),
                 br(),
                 h4("Links to Github"),
                 tags$div(
                   tags$ul(
                     tags$li(a(href="https://github.com/jerryendrina/ST558-Project3", 
                                          "Repository")),
                     tags$li(a(href="https://jerryendrina.github.io/", 
                                          "Blog"))
                     )
                   )
               ),
               
               #Main Panel
               mainPanel(
                 h2("Welcome to the Breast Cancer Diagnosis Prediction Shiny App!"),
                 br(),
                 
                 #purpose of the app
                 tags$div(
                   h3("Purpose"),
                   tags$p("This app is made in partial fulfillment of the 
                          requirements for the course ST558. In addition, 
                          this is created in an attempt to create an app that 
                          predicts diagnosis of breast cancer as benign or 
                          malignant based on different variable inputs. Three 
                          supervised learning models will be generated from 
                          fitting on training data set and will be assessed on 
                          test data set to calculate performance in classifying 
                          observations. Prior to that, a data exploration will 
                          be done to aid in understanding how predictor 
                          variables relate to diagnosis as the response variable.
                          Lastly, data set can be filtered based on user's 
                          specified options."),
                   
                   #brief description of data and its link
                   h3("Data Description"),
                   tags$p("The Breast Cancer (Wisconsin) data set is used in this
                          app and is downloaded from UCI machine learning 
                          repository. It has 32 attributes and 569 observations.
                          In this app, only 10 variables are used which are based
                          on ten real-valued continuous features computed from 
                          cell nucleus,namely: radius, texture, perimeter, area, 
                          smoothness, compactness, concavity, concave points, 
                          symmetry, and fractal dimension."),
                   tags$div(
                     tags$ul(
                       tags$li(a(href="https://archive-beta.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+diagnostic", 
                                 "Data set link!")),
                     )
                   ),
                   
                   #purpose of each page
                   h3("App Navigation"),
                   tags$p("This app contains pages that can be clicked and 
                          explored its content:"),
                   tags$ul(
                     tags$li("About: To provide description of the app."),
                     tags$li("Data Exploration: To generate numerical and 
                             graphical summaries."),
                     tags$li("Modeling: To specify various aspects of three 
                             different supervised learning models as well as 
                             get predictions."),
                     tags$li("Data: To filter through the data, subset, and 
                             save it to a .csv file.")
                   )
                 )
               )
             )
             ),
    
    ##################### second tab: data exploration #######################
    tabPanel("Data Exploration", fluid=TRUE,
             
             #sidebar
             sidebarLayout(
               sidebarPanel(
                 
                 #create subheading
                 h3("Options:"),
                 br(),
                 
                 #add radio button widget to choose a plot to use
                 radioButtons(
                   inputId="plotType", 
                   label="Plot Type",
                   choiceValues=c("histogram", "scatterPlot"),
                   choiceNames=c("Histogram", "Scatter Plot"),
                   selected="histogram"),
                 
                 #create widgets to show only if histogram is selected
                 conditionalPanel(
                   condition = "input.plotType == 'histogram'",
                   selectInput(
                     inputId="histVar",
                     label="Histogram Variables",
                     choices= colNames2,
                     selected = "radius_mean"
                   ),
                   
                   #widget to adjust bins of histogram
                   sliderInput(
                     inputId = "bins", 
                     label = "Number of Bins",
                     min=0.1, 
                     max=20, 
                     value=0.5,
                     step=0.2
                   ),
                   
                   #widget to center and scale data or not
                   radioButtons(
                     inputId = "scale",
                     label= "Center and scale data?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE),
                   
                   #widget to group data by diagnosis
                   radioButtons(
                     inputId = "fill",
                     label= "Group by diagnosis?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE)
                 ),

                 #create widgets to show only when scatter plot is selected
                 conditionalPanel(
                   condition = "input.plotType == 'scatterPlot'",
                   
                   #widget to specify x-axis variable
                   selectizeInput(
                     inputId = "xVar",
                     label = "Choose X Variable:", 
                     selected = "radius_mean", 
                     choices = colNames2),
                   
                   #widget to specify y-axis variable
                   selectizeInput(
                     inputId = "yVar",
                     label = "Choose Y Variable:", 
                     selected = "texture_mean", 
                     choices = colNames2),
                   
                   #widget to group date by diagnosis
                   radioButtons(
                     inputId = "groupBy",
                     label= "Group by diagnosis?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE),
                   
                   #widget to add regression line to the plot
                   radioButtons(
                     inputId = "geomSmooth",
                     label= "Add regression line?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE)
                   ),
                br(),
                h4("Note: Numerical summaries are generated automatically for selected variable/s."),
               ),
               
               #main panel
               mainPanel(
                 h3("Graphical Summary"),
                 plotOutput("graph"),
                 br(),
                 h3("Numerical Summaries"),
                 dataTableOutput("numSummary")
               )
             )
          ),

    
    ################## third tab: modeling page with 3 tabs ###################
    
    navbarMenu(
      
      #add a title
      title = "Modeling",
      
      #### MODELING INFO TAB ####
      tabPanel(
        title = "Modeling Information",
        mainPanel(fluidPage(
          br(),
          h4("Purpose of the Modeling Section"),
          br(),
          br(),
          
          #logistic regression overview
          h4("Logistic Regression"),
          uiOutput("logRegEx"),
          br(),
          br(),
          
          #classification tree overview
          h4("Classification Trees"),
          br(),
          br(),
          
          #random forest overview
          h4("Random Forests"),
          br(),
          br()
        ))),
      
      #### FITTING MODELS TAB ####
      tabPanel(
        title = "Model Fitting",
        
        #sidebar panel
        sidebarPanel(
          h3("1.General Modeling Options"),
          
          #splitting data option
          sliderInput(
            inputId = "proTesting",
            label = "1.1 Proportion of Data for Test Set:",
            value = 0.2,
            min = 0.1,
            max = 0.5,
            step = 0.05),
          
          #cross validation option
          div(
            numericInput(
              inputId = "numFolds",
              label = "1.2 Number of folds for cross-validation:",
              value = 3,
              min = 3,
              max = 5,
              step = 1)),
          br(),
          
          #nodeling options
          h3("2. Specific Modeling Options"),
          
          #logistic regression options
          selectInput(
            inputId = "logRegVars",
            label = "2.1 Variables for Logistic Regression:",
            choices = colNames2,
            selected = colNames2,
            multiple = TRUE,
            selectize = TRUE
          ),
          
          #classification tree options
          selectInput(
            inputId = "treeVars",
            label = "2.2 Variables for Classification Tree:",
            choices = colNames2,
            selected = colNames2,
            multiple = TRUE,
            selectize = TRUE
          ),
          
          #additional tree parameters
          h5(tags$b("2.2.1 Complexity Parameters for Trees")),
          div(
            uiOutput("minCpInput"),
            style="display:inline-block"),
          div(
            uiOutput("maxCpInput"),
            style= "display:inline-block"),
          div(
            numericInput(
              inputId = "numCps",
              label = "Number of Values:",
              min = 1,
              max = 5,
              value = 3,
              step = 1),
            style = "display:inline-block"),
          
          #random forest options
          selectInput(
            inputId = "randForVars",
            label = "2.3 Variables for Random Forest:",
            choices = colNames2,
            selected = colNames2,
            multiple = TRUE,
            selectize = TRUE),
          
          #additional random forests parameters
          h5(tags$b("2.3.1 Complexity Parameters for Random Forest")),
          div(
            selectizeInput(
            inputId = "randForMtry",
            label = "Select 5 values for mtry:",
            choices = 1:length(colnames(cancer)[2:32]),
            multiple = TRUE,
            selected = c(2, 5, 8, 10, 12),
            options = list(maxItems = 5)),
            style="display:inline-block"),
          
          #action button for fitting
          br(),
          actionButton(
            inputId = "trainStart",
            label = "Fit Models")
          ),
        
        #main panel
        mainPanel(
          h3("Model Performance in Test Set"),
          dataTableOutput("accTableOutput"),
          br(),
          h3("Logistic Regression Summary"),
          dataTableOutput("logRegSummary"),
          br(),
          h3("Tree Diagram"),
          plotOutput("treeSummary"),
          br(),
          h3("Random Forest Feature Importance"),
          plotOutput("rfVarImpPlot")
        )
      ),
      
      #### PREDICTION TAB ####
      
      tabPanel(
        
        #title
        title = "Prediction",

        ##sidebarPanel
        sidebarPanel(
          
          #widget to select model to use
          radioButtons(
            inputId = "modelType",
            label = "Choose a Model",
            choiceNames = c(
              "Logistic Regressin",
              "Classification Tree",
              "Random Forest"),
            choiceValues = c("logReg", "tree", "randFor"),
            selected = "logReg"),

          #create conditions to change variables shown depending on models used
          conditionalPanel(
            condition = "input.modelType == 'logReg'",
            uiOutput("logRegPredInputs"),
          ),
          conditionalPanel(
            condition = "input.modelType == 'tree'",
            uiOutput("treePredInputs")
          ),
          conditionalPanel(
            condition = "input.modelType == 'randFor'",
            uiOutput("randForPredInputs")
          ),
          
          #action button to fit model
          actionButton(
            inputId = "predStart",
            label = "Predict Benign or Malignant!")
        ),

        ##mainpanel
        mainPanel(
          h3("Breast Cancer Diagnosis Prediction: Benign(B) or Malignant(M):"),
          dataTableOutput("preds")
        )
      )
    ),
    
    ############################ fourth tab: Data Set ##########################
    
    tabPanel("Data", fluid=TRUE,
             
             #sidebar
             sidebarLayout(
               sidebarPanel(
                 
                 #widget to select variables
                 selectInput(
                             inputId = "cols", 
                             label="Select predictor variable/s:", 
                             choices= colNames2,
                             multiple = TRUE,
                             selected = colNames2,
                             selectize = TRUE),
                 
                 #widget to filter by diagnosis
                 selectInput(
                   inputId = "filter",
                   label="Filter by diagnosis:",
                   choices= c("all", "M", "B"),
                   selected = "all"),
                 
                 #create download button
                 downloadButton('download', "Download data!")
               ),
               
               #main panel
               mainPanel(
                 h2("The Data Set"),
                 tags$p("Use this page to download any or all data that you need."),
                 br(),
                 dataTableOutput("fulldata")
               )
             ))
    )
  )
)
