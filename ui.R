#install.packages("shinydashboard")



library(shiny)
library(shinyFiles)
library(shinydashboard)


dashboardPage(
  dashboardHeader(title = 'Upload Group Folders'),
  dashboardSidebar( shinyDirButton('directory', 'Browse WT', 'Please select WT Group Folder'),tags$p(),
                       
                       tags$hr(),
                       shinyDirButton('directory2', 'Browse HT', 'Please select HT Group Folder'),tags$p(),
                       tags$p(),
                       
                       
                       
                       hr()
                       #fluidRow(column(1, verbatimTextOutput("value")))
      
                    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    
    box(
      title = "Selected Folders", background = "black",
      textOutput('directorypath'), actionButton("goButton","Upload"),
      textOutput('directorypath2'), actionButton("goButton2","Upload")
    ),
    fluidRow(
      headerPanel(
        'Analysis Output'),
      tabsetPanel(
        tabPanel("Entropy Analysis",plotOutput("plot1"), tags$hr()),
        tabPanel("Linear Model Analysis", plotOutput("plot2"),tags$hr(),plotOutput("plot3"),tags$hr(),verbatimTextOutput("summaryMLE")), 
        tabPanel("Markov Model Graphs", plotOutput("plot4"),tags$hr(),plotOutput("plot5")),
        tabPanel("Classification Analysis", selectInput("select", label = h4("Select Entropy Level for Classification"), 
                                                        choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3)), plotOutput("plot6"),tags$hr(),plotOutput("plot7"))
      ))))