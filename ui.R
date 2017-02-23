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
        tabPanel("Entropy Analysis",plotOutput("plot1"), tags$hr(),downloadButton('downloadPlot1', 'Download Plot')
),
        tabPanel("Linear Model Analysis", plotOutput("plot2"),downloadButton('downloadPlo2', 'Download Plot')
,tags$hr(),plotOutput("plot3"),downloadButton('downloadPlot3', 'Download Plot'),tags$hr(),plotOutput("plot4"),downloadButton('downloadPlot4', 'Download Plot'),tags$hr()
,verbatimTextOutput("summaryMLE"),downloadButton('downloadData', 'Download Summary')
), 
        tabPanel("Markov Model Graphs", plotOutput("plot5"),downloadButton('downloadPlot5', 'Download Plot')
,tags$hr(),plotOutput("plot6"),downloadButton('downloadPlot6', 'Download Plot')),
        tabPanel("Classification Analysis", selectInput("percentage", label = h4("Select percentage of calls for Classification"), 
                                                        choices = list("-" = 0,"10%" = 0.1,"20%" = 0.2,"30%" = 0.3,"40%" = 0.4,"50%" = 0.5)), selectInput("select", label = h4("Select Entropy Level for Classification"), 
                                                        choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3)), plotOutput("plot7"),downloadButton('downloadPlot7', 'Download Plot'),tags$hr(),plotOutput("plot8"),downloadButton('downloadPlot8', 'Download Plot'))
      ))))