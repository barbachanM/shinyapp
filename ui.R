#install.packages("shinydashboard")



library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

useShinyjs() # Include shinyjs


dashboardPage(
  dashboardHeader(title = 'Upload Group Folders'),
  dashboardSidebar( shinyDirButton('directory', 'Browse WT', 'Please select WT Group Folder'),tags$p(),
                       
                       tags$hr(),
                       shinyDirButton('directory2', 'Browse Mut', 'Please select Mut Group Folder'),tags$p(),
                       tags$p(), helpText("Please upload folders containg tab delimited .csv files. "),
                       
                       
                       
                       hr()
                       #fluidRow(column(1, verbatimTextOutput("value")))
      
                    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    box(
      title = "Selected Folders", background = "light-blue",
      #textOutput('directorypath'), actionButton("goButton","Upload"),
      #verbatimTextOutput(outputId, placeholder = FALSE), actionButton("goButton","Upload")
      htmlOutput("directorypath"),
      
      actionButton("goButton","Upload"),
      conditionalPanel(condition = "!output.setupCompleteWT",tags$div(class="header", checked=NA,
                                                                    list(
                                                                      tags$p("Please upload WT folder")))),
              conditionalPanel(condition = "output.setupCompleteWT",tags$div(class="header", checked=NA,
                                                                           list(
                                                                             tags$p("Uploaded!")))),
      htmlOutput("directorypath2"),actionButton("goButton2","Upload"),
      conditionalPanel(condition = "!output.setupCompleteHT",tags$div(class="header", checked=NA,
                                                                      list(
                                                                        tags$p("Please upload Mut folder")))),
      conditionalPanel(condition = "output.setupCompleteHT",tags$div(class="header", checked=NA,
                                                                     list(
                                                                       tags$p("Uploaded!"))))
      #textOutput('directorypath2'), actionButton("goButton2","Upload")
    ),

    conditionalPanel(condition = "output.setupComplete",
    fluidRow(
      headerPanel(
        'Analysis Output'),
      tabsetPanel(
        tabPanel("Entropy Analysis"
        ,plotOutput("plot1"), tags$hr(),downloadButton('downloadPlot1', 'Download Plot')
),
        tabPanel("Linear Model Analysis", tags$div(class="header", checked=NA,
                                      
                                                     tags$em(bsButton("help1","Info", icon = NULL, style = "inverse",
                                                                      size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                      value = FALSE)
                                                     )
                                                   ),verbatimTextOutput("summaryMLE"),downloadButton('downloadData', 'Download Summary'),tags$hr(),plotOutput("plot3"),downloadButton('downloadPlot3', 'Download Plot'),tags$hr(),plotOutput("plot4"),downloadButton('downloadPlot4', 'Download Plot'), plotOutput("plot2"),downloadButton('downloadPlo2', 'Download Plot')
,tags$hr()

), 
        tabPanel("Markov Model Graphs", tags$div(class="header", checked=NA,
                                                 
                                                 tags$em(bsButton("help2","Info", icon = NULL, style = "inverse",
                                                                  size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                  value = FALSE)
                                                 )
        ),plotOutput("plot5"),downloadButton('downloadPlot5', 'Download Plot')
,tags$hr(),plotOutput("plot6"),downloadButton('downloadPlot6', 'Download Plot')),
        tabPanel("SPLS-DA", tags$div(class="header", checked=NA,
                                                     
                                                     tags$em(bsButton("help3","Info", icon = NULL, style = "inverse",
                                                                      size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                      value = FALSE)
                                                     )
        ), selectInput("percentage", label = h4("Select percentage of calls for Classification"), 
                                                        choices = list("-" = 0,"10%" = 0.1,"20%" = 0.2,"30%" = 0.3,"40%" = 0.4,"50%" = 0.5)), selectInput("select", label = h4("Select Entropy Level for Classification", bsTooltip("select", "The entropy level of the analysis should be chosen based on the linear model result found on the Linear Model Analysis tab.",                                                                                                                                     placement = "right", trigger = "hover")),
                                                        choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3)), plotOutput("plot7"),downloadButton('downloadPlot7', 'Download Plot'),tags$hr(),plotOutput("plot8"),downloadButton('downloadPlot8', 'Download Plot')),
tabPanel("Caret - StepLDA", tags$div(class="header", checked=NA,
                                             
                                             tags$em(bsButton("help3","Info", icon = NULL, style = "inverse",
                                                              size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                              value = FALSE)
                                             )
), selectInput("percentage", label = h4("Select percentage of calls for Classification"), 
               choices = list("-" = 0,"10%" = 0.1,"20%" = 0.2,"30%" = 0.3,"40%" = 0.4,"50%" = 0.5)), selectInput("select", label = h4("Select Entropy Level for Classification", bsTooltip("select", "The entropy level of the analysis should be chosen based on the linear model result found on the Linear Model Analysis tab.",                                                                                                                                     placement = "right", trigger = "hover")),
                                                                                                                 choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3)), plotOutput("plot7"),downloadButton('downloadPlot7', 'Download Plot'),tags$hr(),plotOutput("plot8"),downloadButton('downloadPlot8', 'Download Plot')),
tabPanel("Boruta", tags$div(class="header", checked=NA,
                                             
                                             tags$em(bsButton("help3","Info", icon = NULL, style = "inverse",
                                                              size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                              value = FALSE)
                                             )
), selectInput("percentage", label = h4("Select percentage of calls for Classification"), 
               choices = list("-" = 0,"10%" = 0.1,"20%" = 0.2,"30%" = 0.3,"40%" = 0.4,"50%" = 0.5)), selectInput("select", label = h4("Select Entropy Level for Classification", bsTooltip("select", "The entropy level of the analysis should be chosen based on the linear model result found on the Linear Model Analysis tab.",                                                                                                                                     placement = "right", trigger = "hover")),
                                                                                                                 choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3)), plotOutput("plot7"),downloadButton('downloadPlot7', 'Download Plot'),tags$hr(),plotOutput("plot8"),downloadButton('downloadPlot8', 'Download Plot'))

      ))), 
conditionalPanel(condition = "!output.setupComplete",
                 tags$div(class="header", checked=NA,
                          list(tags$hr(),
                            tags$h1("Upload folders")
                          )
                 )))
)
