library(shiny)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(shinythemes)
library(rattle)


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

colNames <- names(data[,1:15])




# Define UI
shinyUI(navbarPage(
  
  #add title
  title = "State Level Math Score of Students during the Pandemic",
  #add theme
  theme = shinytheme("flatly"),
  
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
                 h2("Welcome to my Project Shiny App!"),
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
    tabPanel("Exploration", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 h2("Data Exploration"),
                 br(),
                 h3("Graphical Summaries"),
                 radioButtons(inputId="plottype", label="Plot Type",
                              choiceValues=c("hist","bar","scat"),
                              choiceNames=c("Histogram","Bar Plot", "Scatter Plot")),
                 
                 conditionalPanel("input.plottype == 'hist'",
                                  selectInput(inputId="histvar",
                                              label="Histogram Variables",
                                              choices=c("Household Income"="householdincome",
                                                        "Reading Score"="readingscore",
                                                        "Writing Score"="writingscore",
                                                        "Math Score"="mathscore",
                                                        "Reading Score State Level"="readingscoreSL",
                                                        "Writing Score State Level"="writingscoreSL",
                                                        "Number of Computers"="numcomputers",
                                                        "Family Size"="familysize"
                                                        )),
                                  sliderInput("bins", "Number of Bins",
                                              min=0, max=50, value=10)
                                  ),
                 conditionalPanel("input.plottype == 'bar'",
                                  selectInput(inputId = 'barvar',
                                              label = "Bar Plot Variables",
                                              choices=c("Lunch Status"="freelunch",
                                                        "Covid Positive"="covidpos",
                                                        "School Type"="school",
                                                        "Grade Level"="gradelevel",
                                                        "Gender"="gender",
                                                        "Father's Education"="fathereduc",
                                                        "Mother's Education"="mothereduc"
                                                        )),
                                  checkboxInput(inputId="barcolor",
                                                label="Group whether pass or fail in Math State Test?")
                                  ),
                 conditionalPanel("input.plottype == 'scat'",
                                  checkboxInput(inputId = "scatcolor",
                                                label = "Color by pass or fail in Math State Test?")
                                  ),
                br(),
                h4("Note: Numerical summaries are generated automatically for selected variable/s."),
                br(),
                h4("Data Set"),
                selectInput(inputId="exploreFunc", label="Choose the numeric summary to display:",
                            selected="dat",
                            choices=c("Means"="avg",
                                      "Standard Deviations" = "sd",
                                      "Data only!" = "dat")),
                  conditionalPanel("input.exploreFunc=='dat'",
                                   checkboxGroupInput(inputId = "xplrSub", label="Click variables to display:",
                                                      selected = colNames,
                                                      choices = colNames)
                                   ),
                conditionalPanel("input.exploreFunc != 'dat'",
                                 checkboxGroupInput(inputId="exploreNumSub", label="Click variables to display:",
                                                    selected = c("householdincome","numcomputers","familysize",
                                                                 "readingscore","writingscore","mathscore",
                                                                 "readingscoreSL", "writingscoreSL"),
                                                    choices = c("householdincome","numcomputers","familysize",
                                                                "readingscore","writingscore","mathscore",
                                                                "readingscoreSL", "writingscoreSL"))
                                 ),
                 
                 
               ),
               mainPanel(
                 h2("Graphical Summary"),
                 plotlyOutput("explorePlot"),
                 br(),
                 h2("Numerical Summaries"),
                 dataTableOutput("exploreSummary"),
                 h2('Data Set')
               )
             )),

    
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
            choices = colNames,
            selected = colNames,
            multiple = TRUE,
            selectize = TRUE
          ),
          #classification tree
          selectInput(
            inputId = "treeVars",
            label = "2.2 Variables for Classification Tree:",
            choices = colNames,
            selected = c("householdincome",
                         "readingscore",
                         "writingscore",
                         "mathscore",
                         "mathscoreSL",
                         "readingscoreSL",
                         "writingscoreSL"),
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
            choices = colNames,
            selected = colNames,
            multiple = TRUE,
            selectize = TRUE),
          #additional random forests parameters
          h5(tags$b("2.3.1 Complexity Parameters for Random Forest")),
          div(
            selectizeInput(
            inputId = "randForMtry",
            label = "Select up to 5 values for mtry:",
            choices = 1:length(colnames(data)[1:16]),
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

          #action button to fit model
          actionButton(
            inputId = "predStart",
            label = "Predict Pass or Fail"),

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
          )
        ),

        ##mainpanel
        mainPanel(
          h3("Prediction of Math State Level Test with Your Inputs:"),
          dataTableOutput("preds")
        )
      )
      
      
      
    ),
    
      
    
    ############################ fourth tab: Data Set ##########################
    tabPanel("Data", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "rows", 
                   label = "Select Row Range", 
                   min = 1, 
                   max = 1400, 
                   value = c(1, 1400)),
                 checkboxGroupInput(
                             inputId = "cols", 
                             label="Select Columns", selected=colNames, choices=colNames),
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
