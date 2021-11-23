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

cancer$diagnosis <- as.factor(cancer$diagnosis)
colNames2 <-names(cancer[,2:10])






# Define UI
shinyUI(navbarPage(
  
  #add title
  title = "Breast Cancer Diagnosis Prediction",
  #add theme
  theme = shinytheme("united"),
  
  tabsetPanel(
    
    ################### first tab: information #############################
    tabPanel("Information", fluid=TRUE,
             # Sidebar
             sidebarLayout(
               sidebarPanel(h2("ST558 Final Project"),
                            h3("By: Jeremias Endrina Jr."),
                            br(),
                            h4("Links to Github"),
                            tags$div(
                              tags$ul(
                                tags$li(a(href = "https://github.com/jerryendrina/ST558-Project3", "Repository")),
                                tags$li(a(href = "https://jerryendrina.github.io/", "Blog"))
                              )
                            )
               ),
               
               #Main Panel
               mainPanel(
                 h2("Welcome to my Breast Cancer Diagnosis Prediction Shiny App!"),
                 br(),
                 tags$div(
                   h3("Purpose"),
                   tags$p("Write intro here!"),
                   h3("Data Description"),
                   tags$p("Write something about data here"),
                   h3("App Navigation"),
                   tags$p("This app contains tabs that you can click and explore its content:"),
                   tags$ul(
                     tags$li("Information:"),
                     tags$li("Exploration:"),
                     tags$li("Unsupervised:"),
                     tags$li("Modeling:"),
                     tags$li("Data:")
                   )
                 )
               )
             )
             ),
    
    ##################### second tab: data exploration #######################
    tabPanel("Data Exploration", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 h2("Data Exploration"),
                 br(),
                 
                 h3("Graphical Summaries"),
                 
                 radioButtons(
                   inputId="plotType", 
                   label="Plot Type",
                   choiceValues=c("histogram", "scatterPlot"),
                   choiceNames=c("Histogram", "Scatter Plot"),
                   selected="histogram"),
                 
                 #only show if histogram is selected
                 conditionalPanel(
                   condition = "input.plotType == 'histogram'",
                   selectInput(
                     inputId="histVar",
                     label="Histogram Variables",
                     choices= colNames2,
                     selected = "radius_mean"
                   ),
                   sliderInput(
                     inputId = "bins", 
                     label = "Number of Bins",
                     min=0.1, 
                     max=20, 
                     value=0.5,
                     step=0.2
                   ),
                   radioButtons(
                     inputId = "scale",
                     label= "Center and scale data?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE),
                   radioButtons(
                     inputId = "fill",
                     label= "Group by diagnosis?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE)
                 ),

                 #only show when scatter plot is selected
                 conditionalPanel(
                   condition = "input.plotType == 'scatterPlot'",
                   
                   selectizeInput(
                     inputId = "xVar",
                     label = "Choose X Variable:", 
                     selected = "radius_mean", 
                     choices = colNames2),
                   
                   selectizeInput(
                     inputId = "yVar",
                     label = "Choose Y Variable:", 
                     selected = "texture_mean", 
                     choices = colNames2),
                   
                   radioButtons(
                     inputId = "groupBy",
                     label= "Group by diagnosis?",
                     choiceValues = c(TRUE, FALSE),
                     choiceNames = c("Yes", "No"),
                     selected = FALSE,
                     inline = TRUE),
                   
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
               mainPanel(
                 h3("Graphical Summary"),
                 plotOutput("graph"),
                 br(),
                 h3("Numerical Summaries"),
                 dataTableOutput("numSummary")
                
               )
             )
          ),

    
    ################## third tab: Modeling page with 3 tabs ###################
    navbarMenu(
      
      #add a title
      title = "Modeling",
      
      #### MODELING INFO TAB ####
      tabPanel(
        title = "Modeling Information",
        mainPanel(fluidPage(
          br(),
          h4("Purpose of the Modeling Section"),
          #"The purpose...",
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
          #option to split data
          sliderInput(
            inputId = "proTesting",
            label = "1.1 Proportion of Data for Test Set:",
            value = 0.2,
            min = 0.1,
            max = 0.5,
            step = 0.05),
          #option for number of cross validation
          div(
            numericInput(
              inputId = "numFolds",
              label = "1.2 Number of folds for cross-validation:",
              value = 3,
              min = 3,
              max = 5,
              step = 1)),
          br(),
          #Options for each model
          h3("2. Specific Modeling Options"),
          #logistic regression
          selectInput(
            inputId = "logRegVars",
            label = "2.1 Variables for Logistic Regression:",
            choices = colNames2,
            selected = colNames2,
            multiple = TRUE,
            selectize = TRUE
          ),
          #classification tree
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
          #random forest
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
        
        #main panel of modeling
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
          #buttons to select model to use
          radioButtons(
            inputId = "modelType",
            label = "Choose a Model",
            choiceNames = c(
              "Logistic Regressin",
              "Classification Tree",
              "Random Forest"),
            choiceValues = c("logReg", "tree", "randFor"),
            selected = "logReg"),

          #depending on which model selected, change variables shown
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
            label = "Predict Benign or Malignant.")
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
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput(
                             inputId = "cols", 
                             label="Select predictor variable/s:", 
                             choices= colNames2,
                             multiple = TRUE,
                             selected = colNames2,
                             selectize = TRUE),
                 
                 selectInput(
                   inputId = "filter",
                   label="Filter by diagnosis:",
                   choices= c("all", "M", "B"),
                   selected = "all"),

                 downloadButton('download', "Download data!")
                 
               ),
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
