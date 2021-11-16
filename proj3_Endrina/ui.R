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
                            h4("External Links"),
                            tags$div(
                              tags$ul(
                                tags$li(a(href = "https://github.com/jerryendrina/ST558-Project3", "REPOSITORY")),
                                tags$li(a(href = "https://jerryendrina.github.io/", "BLOG"))
                              )
                            )
               ),
               
               #Main Panel
               mainPanel(
                 h2("Welcome to my Project Shiny App!"),
                 br(),
                 tags$div(
                   h3("Introduction"),
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
                 h3("Graphical Summaries")
               ),
               mainPanel(
                 h2("Graphical Summary"),
                 br(),
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
