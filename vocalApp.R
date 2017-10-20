##### New Interface VocalApp ####

#### Loading libraries... ####


library(devtools)
library(hash)
library(stringi)
library(stringr)
library(lmerTest)
require(ggplot2)
library(markovchain)
library(vegetarian)
library(igraph)
library(mixOmics)
library(data.table)
library(shiny)
library(shinyFiles)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(caret)
library(Boruta)
library(mlbench)
library(shinythemes)
library(gtools)
#install.packages("readr") # you only need to do this one time on your system
library(readr)



#### Out of Shiny specific functions ####

rv <- reactiveValues()
rv$setupComplete <- FALSE
rv$setupCompleteWT <- FALSE
rv$setupCompleteHT <- FALSE


#### Server Function ####

server = function(input, output) {
  
  # Get Folder Paths
  volumes = getVolumes()
  valuesF1 <- reactiveValues() 
  valuesF2 <- reactiveValues()
  
  ## Process Folder Paths
  
  folderInput1 <- reactive({
    shinyDirChoose(input, 'directory', roots = volumes, session = session, 
                   restrictions = system.file(package = 'base'))
    return(parseDirPath(volumes, input$directory))
  })
  
  folderInput2 <- reactive({
    shinyDirChoose(input, 'directory2', roots = volumes, session = session, 
                   restrictions = system.file(package = 'base'))
    return(parseDirPath(volumes, input$directory2))
  })
  
  #output$directorypath = renderPrint({folderInput1()})
  
  output$directorypath <- renderUI({
    HTML(folderInput1())
  })
  output$directorypath2 = renderUI({
    HTML(folderInput2())
  })
  
  
  files1 <- reactive({
    list.files(path = folderInput1(), pattern = "*.csv", full.names = T)
  })
  nFiles1 <-reactive({length(files1())})
  
  files2 <- reactive({
    list.files(path = folderInput2(), pattern = "*.csv", full.names = T)
  })
  nFiles2 <- reactive({ length(files2())})
  
  observe({
    if (input$run == 0) # tells action button to do nothing when not clicked ..
      return()
    isolate({
      observe({
        alphabet = input$fileAlphabet
        if (is.null(alphabet)) {
          return(NULL)
        }
        dataH1 = read_file(alphabet$datapath)
        
        alphabetFromText = unlist(str_split(dataH1, ","))

        for (i in 1:10){
          assign(paste("Alphabet.", i, sep = ""), permutations(n=length(alphabetFromText),r=i,v=alphabetFromText,repeats.allowed=T))

        }
        output$alphabetdata = renderPrint(Alphabet.10)
        
        
      })
    })})

  
  

  
}

#### Ui Function ####
ui = dashboardPage(skin = "black",
  dashboardHeader(title = 'Harpia'),
  dashboardSidebar( textInput("email", "Email", "email adress")),
  dashboardBody( box( title = "Choose Entropy Level and Upload Alphabet",
                      
                      fileInput("fileAlphabet", "Choose Alphabet File",     accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
                        
                      ),
                    helpText("Please upload a comma separated file containg the vocalization alphabet."),
                    
                    verbatimTextOutput("alphabetdata"),
                      
                      tags$p(),
                      
                      selectInput("selectLevel", label = h4("Choose maximum level of entropy", bsTooltip("selectEntropy", "Please note that the time of processing is directly proportional with the number of levels of entropy of your choice.",                                                                                                                                     placement = "right", trigger = "hover")),
                                  choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3, "H4" = 4)),tags$hr()
                      
  ), box( title = "Upload Group Files",
                      shinyDirButton('directory', 'Upload WT', 'Please select WT Group Folder'),tags$p(),
                    
                    
                    shinyDirButton('directory2', 'Upload Mut', 'Please select Mut Group Folder'),tags$p(),
                    tags$p(), helpText("Please upload folders containg tab delimited .csv files. ")),
                    
                    
                    
                    tags$p(),
  box(
          actionButton("run", "Run!", width = "100%",icon("cogs")
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
                       
                       tabPanel("Boruta - Random forest", tags$div(class="header", checked=NA,
                                                                   
                                                                   tags$em(bsButton("help4","Info", icon = NULL, style = "inverse",
                                                                                    size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                                    value = FALSE)
                                                                   )
                       ),selectInput("selectB", label = h4("Select Entropy Level for Classification", bsTooltip("selectB", "The entropy level of the analysis should be chosen based on the linear model result found on the Linear Model Analysis tab.",                                                                                                                                     placement = "right", trigger = "hover")),
                                     choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3)),plotOutput("borutaplot"),downloadButton('downloadborutaplot', 'Download Plot')
                       ,tags$hr(),verbatimTextOutput("boruta"),downloadButton('borutaOutcome', 'Download Boruta Outcome')
                       ,tags$hr(),verbatimTextOutput("bStats"),downloadButton('borutaStats', 'Download Boruta Outcome')), tabPanel("Markov Model Graphs", tags$div(class="header", checked=NA,
                                                                                                                                                                   
                                                                                                                                                                   tags$em(bsButton("help2","Info", icon = NULL, style = "inverse",
                                                                                                                                                                                    size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                                                                                                                                    value = FALSE)
                                                                                                                                                                   )
                       ),plotOutput("plot5"),downloadButton('downloadPlot5', 'Download Plot')
                       ,tags$hr(),plotOutput("plot6"),downloadButton('downloadPlot6', 'Download Plot'))
                     ))
                   
  ), 
  conditionalPanel(condition = "!output.setupComplete",
                   tags$div(class="header", checked=NA,
                            list(tags$hr()
                                 #tags$h1("Pre-Analysis Section")
                            )
                   )))
)
                    
                    
  )
               



#### Calling Shiny App ####
shinyApp(ui = ui, server = server)