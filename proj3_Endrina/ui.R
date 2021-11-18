library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
                                                      selected = c("school","gradelevel","gender","covidpos",
                                                                   "householdincome","freelunch","numcomputers",
                                                                   "familysize","fathereduc","mothereduc","readingscore",
                                                                   "writingscore","mathscore","readingscoreSL",
                                                                   "writingscoreSL", "mathPassFail"),
                                                      choices = c("school","gradelevel","gender","covidpos",
                                                                  "householdincome","freelunch","numcomputers",
                                                                  "familysize","fathereduc","mothereduc","readingscore",
                                                                  "writingscore","mathscore","readingscoreSL",
                                                                  "writingscoreSL", "mathPassFail"))
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
    tabPanel("Unsupervised", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 h2("Unsupervised"),
                 br()
               ),
               mainPanel(
                 h3("Visualization"),
                 br(),
                 h3('Analysis Output')
               )
             )),
    
    #fourth tab
    tabPanel("Modeling", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 h2("Modeling"),
                 br()
               ),
               mainPanel(
                 h2("Model Description"),
                 br(),
                 h3('Prediction Output')
               )
             )),
    
    #fifth tab
    tabPanel("Data", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 h2("Data"),
                 br()
               ),
               mainPanel(
                 h2("The Data Set"),
                 br()
               )
             ))
    
    
    
    
    )

  )
)
