library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)
library(shinythemes)


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




# Define UI
shinyUI(navbarPage(
  
  #add title
  title = "State Level Math Score of Students during the Pandemic",
  #add theme
  theme = shinytheme("flatly"),
  
  tabsetPanel(
    
    #first tab:
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
    
    #second tab
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

    
    #third tab
    tabPanel("Modeling Page", fluid=TRUE,        #logistic, classification tree and RF
             sidebarLayout(
               sidebarPanel(
                 h3("Modeling Options"),
                 sliderInput("pTrain", "Percent of Data to Train:",
                             min=.60, value=0.75, max=.80, step=.05),
                 sliderInput("cp", "Cp for Classification Tree:",
                             min = .01, value = .0565, max = .065, step = .005),
                 br(),
                 checkboxGroupInput(inputId="preds", label="Click predictors to include:",
                                    selected= colNames,
                                    choices= colNames
                                    ),
                 actionButton("run", "Run modeling!"),
                 h3("Prediction Inputs"),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("school") > -1',
                                  selectInput("school", label = "School Type", 
                                              choices = c("Wealthy", "Poor"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("gradelevel") > -1',
                                  selectInput("gradelevel", label = "Grade Level", 
                                              choices = c("6","7","8","9","10","11","12"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("gender") > -1',
                                  selectInput("gender", label = "Gender", 
                                              choices = c("Male","Female"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("covidpos") > -1',
                                  selectInput("covidpos", label = "Covid Status", 
                                              choices = c("Positive","Negative"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("freelunch") > -1',
                                  selectInput("freelunch", label = "Lunch Status", 
                                              choices = c("EatsFreelunch","PaysForLunch"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("numcomputers") > -1',
                                  numericInput("numcomputers", label = "Number of Computers", 
                                               value = 0, min = 0, max = 10, step=1)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("familysize") > -1',
                                  numericInput("familysize", label = "Family Size", 
                                               value = 0, min = 0, max = 20, step=1)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("householdincome") > -1',
                                  numericInput("householdincome", label = "House Hold Income", 
                                               value = 0, min = 0, max = 500000, step=10000)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("fathereduc") > -1',
                                  selectInput("fathereduc", label = "Father's Education", 
                                              choices = c("NoHSDiploma","HSDiploma","Bachelor","Masters","PhD"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("mothereduc") > -1',
                                  selectInput("mothereduc", label = "Mother's Education", 
                                              choices = c("NoHSDiploma","HSDiploma","Bachelor","Masters","PhD"))),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("readingscore") > -1',
                                  numericInput("readingscore", label = "Reading Score", 
                                               value = 0, min = 0, max = 100, step=10)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("writingscore") > -1',
                                  numericInput("writingscore", label = "Writing Score", 
                                               value = 0, min = 0, max = 100, step=10)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("mathscore") > -1',
                                  numericInput("mathscore", label = "Math Score", 
                                               value = 0, min = 0, max = 100, step=10)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("readingscoreSL") > -1',
                                  numericInput("readingscoreSL", label = "Reading Score State Level", 
                                               value = 0, min = 0, max = 100, step=10)),
                 conditionalPanel(condition = 'input.preds && input.preds.indexOf("writingscoreSL") > -1',
                                  numericInput("writingscoreSL", label = "Writing Score State Level", 
                                               value = 0, min = 0, max = 100, step=10))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Modeling Info", type='tabs',
                            mainPanel(
                              h2("Write Descriptions of 3 Models here!")
                              )
                            ),
                   tabPanel("Modeling Fit", type='tabs', 
                            mainPanel(
                              h2("Model Fitting")
                              )
                            ),
                   tabPanel("Prediction", type='tabs',
                            mainPanel(
                              h2("Prediction")
                              )
                            )
                 )
               )
             )),
    
    #fourth tab
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
