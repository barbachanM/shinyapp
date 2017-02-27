
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
library(shinydashboard)


alphabetH1 = c('C','Cx','D','F','Fs','H','Ha','Sh','Ts','U')
nH1 = length(alphabetH1)
alphabetH2 = c()
alphabetH3 = c()
for (element in alphabetH1){
  for (sElement in alphabetH1){
    new = paste(element,sElement,sep = '\t')
    #print(new)
    alphabetH2 = c(alphabetH2, new)
  }
}
nH2 = length(alphabetH2)
for (element in alphabetH1){
  for (tElement in alphabetH2){
    new = paste(element,tElement,sep = '\t')
    #print(new)
    alphabetH3 = c(alphabetH3, new)
  }
}
nH3 = length(alphabetH3)

shinyServer(function(input, output, session) {
  volumes = getVolumes()
  
  
  valuesF1 <- reactiveValues() 
  valuesF2 <- reactiveValues() 
  
  
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
  
  output$directorypath = renderPrint({folderInput1()})
  output$directorypath2 = renderPrint({folderInput2()}) 
  

  files1 <- reactive({
    list.files(path = folderInput1(), pattern = "*.csv", full.names = T)
  })
  nFiles1 <-reactive({length(files1())})

  files2 <- reactive({
    list.files(path = folderInput2(), pattern = "*.csv", full.names = T)
  })
  nFiles2 <- reactive({ length(files2())})
  

  observeEvent(input$goButton, {
    if(identical(valuesF1$flag, "True")){
      valuesF1$flag = NULL
    }
    valuesF1$flag<-"True"
  })
  
  
  observeEvent(input$goButton2, {
    if(identical(valuesF2$flag, "True")){
      valuesF2$flag = NULL
    }
    valuesF2$flag<-"True"
  })
  

  
  output$t1 = renderPrint({
    if (is.null(valuesF1$flag)) 
      return("Insert WT Folder")
    if (identical(valuesF1$flag, "True")) {
      return(isolate(nFiles1()))}
  } )
  
  
  output$t2 = renderPrint({
    if (is.null(valuesF2$flag)) 
      return("Insert HT Folder")
    if (identical(valuesF2$flag, "True")) {
      return(isolate(nFiles2()))}
  } )

  
### Reactive Expressions ###
  
  ### WT ###
 
  EntropyDataWT = reactive({
    if (identical(valuesF1$flag, "True")){
  lof1 =  isolate(files1())  
  nf1 = isolate(nFiles1())
  
  df = data.frame(matrix(0,ncol = 4, nrow = nf1))
  rownames(df) = lof1
  colnames(df) = c("H0", "H1", "H2", "H3")
  
  C_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
  rownames(C_2) = alphabetH2
  colnames(C_2) = lof1
  
  F_1 = data.frame(matrix(0,ncol = nf1, nrow = nH1))
  rownames(F_1) = alphabetH1
  colnames(F_1) = lof1
  
  F_2 = data.frame(matrix(0,ncol = nf1, nrow = nH2))
  rownames(F_2) = alphabetH2
  colnames(F_2) = lof1
  
  F_3 = data.frame(matrix(0,ncol = nf1, nrow = nH3))
  rownames(F_3) = alphabetH3
  colnames(F_3) = lof1
  
  combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2, F_3 = F_3)
  
  
  
  return(combo)
    }
    else(return(NULL))
  })

  EntropyAnalysisWT = reactive({ 
    if(!is.null(EntropyDataWT())){
      data = EntropyDataWT()
      
      f1 = isolate(row.names(data$df))
      Entropy = isolate(data$df)
      Counts2 = isolate(data$C_2)
      F1 = isolate(data$F_1)
      F2 = isolate(data$F_2)
      F3 = isolate(data$F_3)
      
      
      for(f in f1){
        
        fileIN = readLines(f)
        
        EntropyHash = hash(keys = c('H1','H2','H3'))
        EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
        EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
        EntropyHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
        
        CountHash = hash(keys = c('H1','H2','H3'))
        CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
        CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
        CountHash$H3 = hash(keys = alphabetH3,values = rep(0,nH3) )
        
        ProbabilityHash = hash(keys = c('H1','H2','H3'))
        ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
        ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
        ProbabilityHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
        
        
        for (call in alphabetH1){
          values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
        }
        for (call in alphabetH2){
          values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
          Counts2[f][call,] = values(CountHash$H2, keys= call)
        }
        for (call in alphabetH3){
          values(CountHash$H3, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
          
        }
        
        totalH1 = sum(values(CountHash$H1))
        
        y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
        totalH2 = c()
        for(g in y){
          totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
        }
        
        names(totalH2) = alphabetH1
        
        z = split(seq(1:nH3), ceiling(seq_along(seq(1:nH3))/10))
        totalH3 = c()
        for(g in z){
          totalH3 = c(totalH3, sum(values(CountHash$H3,keys = alphabetH3[g])))
        }
        names(totalH3) = alphabetH2
        h11 = 0
        for (call in alphabetH1){
          values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
          F1[f][call,] = values(ProbabilityHash$H1, keys= call)
          #values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
          values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
          if (!is.nan(values(EntropyHash$H1, keys= call))){
            h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
          else{
            values(EntropyHash$H1, keys= call) = 0
          }
        }
        h22 = 0
        for (call in alphabetH2){
          first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
          values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
          F2[f][call,] = values(ProbabilityHash$H2, keys= call)
          # values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
          values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
          if (!is.nan(values(EntropyHash$H2, keys= call))){
            h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
          else{
            values(EntropyHash$H2, keys= call) = 0
          }
        }
        h33 = 0
        for (call in alphabetH3){
          firstTwo = unlist(strsplit(call,'\t', fixed=FALSE))
          first = firstTwo[1]
          firstTwo = paste(firstTwo[1],'\t',firstTwo[2],sep='')
          values(ProbabilityHash$H3, keys= call) = values(CountHash$H3, keys= call)/totalH3[firstTwo]
          F3[f][call,] = values(ProbabilityHash$H3, keys= call)
          # values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
          values(EntropyHash$H3, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= call)*log2(values(ProbabilityHash$H3, keys= call))
          if (!is.nan(values(EntropyHash$H3, keys= call))){
            h33 = h33 + (as.double(values(EntropyHash$H3, keys= call)))}
          else{
            values(EntropyHash$H3, keys= call) = 0
          }
        }
        
        Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
        Entropy[f,"H1"] = h11
        Entropy[f,"H2"] = h22
        Entropy[f,"H3"] = h33
        
      }
      outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2, F3=F3)
    return(outputData)}
    else(return(NULL))
  })

  
  ##### HT 

  EntropyDataHT = reactive({
    if (identical(valuesF2$flag, "True")){
      lof2 =  isolate(files2())  
      nf2 = isolate(nFiles2())
      
      df = data.frame(matrix(0,ncol = 4, nrow = nf2))
      rownames(df) = lof2
      colnames(df) = c("H0", "H1", "H2", "H3")
      
      C_2 = data.frame(matrix(0,ncol = nf2, nrow = nH2))
      rownames(C_2) = alphabetH2
      colnames(C_2) = lof2
      
      F_1 = data.frame(matrix(0,ncol = nf2, nrow = nH1))
      rownames(F_1) = alphabetH1
      colnames(F_1) = lof2
      
      F_2 = data.frame(matrix(0,ncol = nf2, nrow = nH2))
      rownames(F_2) = alphabetH2
      colnames(F_2) = lof2
      
      F_3 = data.frame(matrix(0,ncol = nf2, nrow = nH3))
      rownames(F_3) = alphabetH3
      colnames(F_3) = lof2
      
      combo = list(df = df,C_2 = C_2,F_1 = F_1, F_2 = F_2, F_3 = F_3)
      
      return(combo)
    }
    else(return(NULL))
  })
  
  EntropyAnalysisHT = reactive({ 
    if(!is.null(EntropyDataHT())){
      data = EntropyDataHT()
      
      f1 = isolate(row.names(data$df))
      Entropy = isolate(data$df)
      Counts2 = isolate(data$C_2)
      F1 = isolate(data$F_1)
      F2 = isolate(data$F_2)
      F3 = isolate(data$F_3)
      
      
      for(f in f1){
        
        fileIN = readLines(f)
        
        EntropyHash = hash(keys = c('H1','H2','H3'))
        EntropyHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
        EntropyHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
        EntropyHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
        
        CountHash = hash(keys = c('H1','H2','H3'))
        CountHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
        CountHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
        CountHash$H3 = hash(keys = alphabetH3,values = rep(0,nH3) )
        
        ProbabilityHash = hash(keys = c('H1','H2','H3'))
        ProbabilityHash$H1 = hash(keys = alphabetH1, values = rep(0,nH1))
        ProbabilityHash$H2 = hash(keys = alphabetH2,values = rep(0,nH2) )
        ProbabilityHash$H3 = hash(keys = alphabetH3, values = rep(0,nH3) )
        
        
        for (call in alphabetH1){
          values(CountHash$H1, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
        }
        for (call in alphabetH2){
          values(CountHash$H2, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
          Counts2[f][call,] = values(CountHash$H2, keys= call)
        }
        for (call in alphabetH3){
          values(CountHash$H3, keys= call) = sum(stri_count_regex(fileIN, paste(call,"\t", sep="")))
          
        }
        
        totalH1 = sum(values(CountHash$H1))
        
        y = split(seq(1:nH2), ceiling(seq_along(seq(1:nH2))/10))
        totalH2 = c()
        for(g in y){
          totalH2 = c(totalH2, sum(values(CountHash$H2,keys = alphabetH2[g])))
        }
        
        names(totalH2) = alphabetH1
        
        z = split(seq(1:nH3), ceiling(seq_along(seq(1:nH3))/10))
        totalH3 = c()
        for(g in z){
          totalH3 = c(totalH3, sum(values(CountHash$H3,keys = alphabetH3[g])))
        }
        names(totalH3) = alphabetH2
        h11 = 0
        for (call in alphabetH1){
          values(ProbabilityHash$H1, keys= call) = values(CountHash$H1, keys= call)/totalH1
          F1[f][call,] = values(ProbabilityHash$H1, keys= call)
          # values(ProbabilityHashHT$H1, keys= call) = c(values(ProbabilityHashHT$H1, keys= call),values(ProbabilityHash$H1, keys= call))
          values(EntropyHash$H1, keys= call) = -1*values(ProbabilityHash$H1, keys= call)*log2(values(ProbabilityHash$H1, keys= call))
          if (!is.nan(values(EntropyHash$H1, keys= call))){
            h11 = h11 + (as.double(values(EntropyHash$H1, keys= call)))}
          else{
            values(EntropyHash$H1, keys= call) = 0
          }
        }
        h22 = 0
        for (call in alphabetH2){
          first = unlist(strsplit(call,'\t', fixed=FALSE))[1]
          values(ProbabilityHash$H2, keys= call) = as.double(values(CountHash$H2, keys= call))/totalH2[first]
          #F2[f][call,] = values(ProbabilityHash$H2, keys= call)
           #values(ProbabilityHashHT$H2, keys= call) = c(values(ProbabilityHashHT$H2, keys= call),as.double(values(CountHash$H2, keys= call))/totalH2[first])
          values(EntropyHash$H2, keys= call) = -1*as.double(values(ProbabilityHash$H1, keys= first))*as.double(values(ProbabilityHash$H2, keys= call))*log2(values(ProbabilityHash$H2, keys= call))
          if (!is.nan(values(EntropyHash$H2, keys= call))){
            h22 = h22 + (as.double(values(EntropyHash$H2, keys= call)))}
          else{
            values(EntropyHash$H2, keys= call) = 0
          }
        }
        h33 = 0
        for (call in alphabetH3){
          firstTwo = unlist(strsplit(call,'\t', fixed=FALSE))
          first = firstTwo[1]
          firstTwo = paste(firstTwo[1],'\t',firstTwo[2],sep='')
        values(ProbabilityHash$H3, keys= call) = values(CountHash$H3, keys= call)/totalH3[firstTwo]
          #F3[f][call,] = values(ProbabilityHash$H3, keys= call)
          #values(ProbabilityHashHT$H3, keys= call) = c(values(ProbabilityHashHT$H3, keys= call),values(CountHash$H3, keys= call)/totalH3[firstTwo])
          values(EntropyHash$H3, keys= call) = -1*values(ProbabilityHash$H1, keys= first)*values(ProbabilityHash$H2, keys= firstTwo)*values(ProbabilityHash$H3, keys= call)*log2(values(ProbabilityHash$H3, keys= call))
          if (!is.nan(values(EntropyHash$H3, keys= call))){
            h33 = h33 + (as.double(values(EntropyHash$H3, keys= call)))}
          else{
            values(EntropyHash$H3, keys= call) = 0
          }
        }
        
        Entropy[f,"H0"] = log2(length(which(values(EntropyHash$H1)!=0)))
        Entropy[f,"H1"] = h11
        Entropy[f,"H2"] = h22
        Entropy[f,"H3"] = h33
        
      }
      outputData = list(Entropy = Entropy, Counts2 = Counts2, F1=F1, F2 = F2, F3=F3)
      return(outputData)}
    else(return(NULL))
  })
  
  
  
  #### Plots ###
  
  plot1 = reactive({
    if(!is.null(EntropyAnalysisHT()) & !is.null(EntropyAnalysisWT())){
      HT_Data = EntropyAnalysisHT()
      WT_Data = EntropyAnalysisWT()
      EntropyHTLM = HT_Data$Entropy
      EntropyWTLM = WT_Data$Entropy
      
      HT = colMeans(EntropyHTLM)
      WT = colMeans(EntropyWTLM)
      
      dataEntropy = data.frame(
        Group = factor(c(rep("HT",4),c(rep("WT",4)))),
        Level = factor(c("H0","H1","H2","H3","H0","H1","H2","H3"), levels=c("H0","H1","H2","H3")),
        Entropy = c(HT,WT))
      
      
      return({ggplot(data=dataEntropy, aes(x=Level, y=Entropy, group=Group, colour=Group)) +
          geom_line() +
          geom_point()})
    }
    
  })
  output$plot1 = renderPlot({ 
    print(plot1())
    })
  
  
  output$downloadPlot1 <- downloadHandler(
    filename = function() { "EntropyAnalysis.png" },
    content = function(file) {
      ggsave(file, plot = plot1(), device = "png")
    })
  
  createMLEData = reactive({
    if(!is.null(EntropyAnalysisHT()) & !is.null(EntropyAnalysisWT())){
      HT_Data = EntropyAnalysisHT()
      WT_Data = EntropyAnalysisWT()
      EntropyHTLM = HT_Data$Entropy
      EntropyWTLM = WT_Data$Entropy
      
      HT = colMeans(EntropyHTLM)
      WT = colMeans(EntropyWTLM)
      
      
      EntropyData = rbind(EntropyWTLM, EntropyHTLM)
      Genotype = c(rep("WT",length(row.names(EntropyWTLM))),rep("HT",length(row.names(EntropyHTLM))))
      EntropyData = cbind(EntropyData, Genotype)
      Mouse = c()
      for (m in rownames(EntropyData)){
        print(m)
        #m = substr(m,9,12)
        Mouse = c(Mouse,m)
        
      }
      MLEData = data.frame(matrix(vector(), 0, 4,
                                  dimnames=list(c(), c("Mouse", "Entropy", "Level","Genotype" ))),
                           stringsAsFactors=T)
      for (n in rownames(EntropyData)){
       # m2 = substr(n,9,12)
        mouseData = data.frame(Mouse = c(rep(n,4)),
                               Entropy = c(EntropyData[n,"H0"],EntropyData[n,"H1"],
                                           EntropyData[n,"H2"],EntropyData[n,"H3"]),
                               Level = factor(c("H0","H1","H2","H3")),
                               Genotype = {
                                 if(EntropyData[n,"Genotype"] == "WT"){Genotype = c(rep("WT",4))}
                                 else{Genotype = c(rep("HT",4))}}
        )
        
        MLEData = rbind.data.frame(MLEData,mouseData)
      }
      
      for (n in rownames(EntropyData)){
        #m2 = substr(n,9,12)
        mouseData = data.frame(Mouse = c(rep(n,4)),
                               Entropy = c(EntropyData[n,"H0"],EntropyData[n,"H1"],
                                           EntropyData[n,"H2"],EntropyData[n,"H3"]),
                               Level = factor(c("H0","H1","H2","H3")),
                               Genotype = c(rep(EntropyData[n,"Genotype"],4)))
        
        MLEData = rbind.data.frame(MLEData,mouseData)
         
      }
      return(MLEData)
      } 
      else(return(NULL))
    
    
  })
  
  lmerAnalysis = reactive({
    if(!is.null(createMLEData())){
      MLEData = isolate(createMLEData())
     # options(lmerControl=list(check.nobs.vs.rankZ = "warning", check.nobs.vs.nlev = "warning",
     #                         check.nobs.vs.nRE = "warning", check.nlev.gtreq.5 = "warning", check.nlev.gtr.1 = "warning"))
      mod1 = lmer(Entropy ~ Genotype*Level +  (1|Mouse),MLEData)
      summary(mod1)
      return(mod1)
      
    }
    else(return(NULL))
    
  })
  
  plot2 = reactive({    
    if(!is.null(createMLEData())){
      MLEData = isolate(createMLEData())
      return({boxplot(Entropy ~ Genotype*Level,
                      col=c("white","lightgray"),
                      las = 2,ylab ="Entropy", 
                      xlab ="Genotype*Level",cex.lab=1.3, cex.axis=0.6, cex.main=1.5,MLEData)})
    }
    
    else(stop("Upload folder") )})
  
  output$plot2 = renderPlot({ 

print(plot2())
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() { "LinearModelBoxPlot.png" },
    content = function(file) {
      ggsave(file, plot = plot2(), device = "png")
    })
  
  plot3 = reactive({if(!is.null(lmerAnalysis())){
    mle = isolate(lmerAnalysis())
    return({plot(mle)
    })}})
  output$plot3 = renderPlot({ 
    print(plot3())
    
  })
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() { "LMER_Plot.png" },
    content = function(file) {
      ggsave(file, plot = plot3(), device = "png")
    })
  
  plot4 = reactive({if(!is.null(lmerAnalysis())){
    mle = isolate(lmerAnalysis())
    return({qqnorm(resid(mle))
      qqline(resid(mle))
    })}})
  output$plot4 = renderPlot({ 
    print(plot4())
    
  })
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() { "LMER_Plot2.png" },
    content = function(file) {
      ggsave(file, plot = plot3(), device = "png")
    })
  
  
  summaryMLE = reactive({if(!is.null(lmerAnalysis())){
    mle = isolate(lmerAnalysis())
    return({summary(mle)})
  }})
  
  output$summaryMLE = renderPrint({ 
    if(!is.null(lmerAnalysis())){
   # mle = isolate(lmerAnalysis())
    return({summaryMLE()})}
  })
  output$downloadData <- downloadHandler(
    filename = function() { "LMER_Summary.txt" },
    content = function(file) {
      write.table(summaryMLE(), file)
    })
  
  plot5 = reactive({if(!is.null(EntropyAnalysisWT())){
    WT_Data = EntropyAnalysisWT()
    countWT2 = rowMeans(WT_Data$Counts2)
    observations = c()
    for (n in alphabetH2){
      aux = c()
      call = unlist(strsplit(n,'\t', fixed=FALSE))
      aux = rep(call,countWT2[n])
      observations = c(observations,aux)
    }
    names(countWT2) = alphabetH2
    markovModelH2 = markovchainFit(data=observations)
    tpmH2 = as.matrix(markovModelH2$estimate@transitionMatrix)
    
    g <- graph.adjacency(tpmH2, weighted=TRUE)
    complicatedCalls = intersect(unique(observations),c("Cx", "Ts", "Fs", "Ha", "C"))
    otherCalls = setdiff(unique(observations),complicatedCalls)
    callOrder = c(complicatedCalls,otherCalls)
    V(g)$color = "dodgerblue3"
    for (calls in complicatedCalls){
      V(g)[calls]$color = "firebrick3"
    }
    
    E(g)$weight = edge.betweenness(g)
    deg <- degree(g, mode="all")
    V(g)$size <- deg*2
    E(g)$arrow.size <- .1
    E(g)$edge.color <- "gray80"
    E(g)$width <- edge.betweenness(g)*.06
    #E(g)$width <- E(g)$weight*.06
    V(g)$label.cex = .7
    return(plot(g, main = "Transition Graph for WT Group", layout=layout_in_circle(g, order = callOrder), vertex.label.color= "white",
                vertex.label.family = "Helvetica", edge.label.font = 2))
  }
    else(stop("Upload folder") )
  })
  
  output$plot5 = renderPlot({ 
    print(plot5())
  })
  output$downloadPlot5 <- downloadHandler(
    filename = function() { "TransitionGraphforWTGroup.png" },
    content = function(file) {
      ggsave(file, plot = plot5(), device = "png")
    })
  
 plot6= reactive({ 
    if(!is.null(EntropyAnalysisHT())){
      HT_Data = EntropyAnalysisHT()
      countHT2 = rowMeans(HT_Data$Counts2)
      observations = c()
      for (n in alphabetH2){
        aux = c()
        call = unlist(strsplit(n,'\t', fixed=FALSE))
        aux = rep(call,countHT2[n])
        observations = c(observations,aux)
      }
      names(countHT2) = alphabetH2
      markovModelH2 = markovchainFit(data=observations)
      tpmH2 = as.matrix(markovModelH2$estimate@transitionMatrix)
      
      g <- graph.adjacency(tpmH2, weighted=TRUE)
      complicatedCalls = intersect(unique(observations),c("Cx", "Ts", "Fs", "Ha", "C"))
      otherCalls = setdiff(unique(observations),complicatedCalls)
      callOrder = c(complicatedCalls,otherCalls)
      V(g)$color = "dodgerblue3"
      for (calls in complicatedCalls){
        V(g)[calls]$color = "firebrick3"
      }
      
      E(g)$weight = edge.betweenness(g)
      deg <- degree(g, mode="all")
      V(g)$size <- deg*2
      E(g)$arrow.size <- .1
      E(g)$edge.color <- "gray80"
      E(g)$width <- edge.betweenness(g)*.06
      #E(g)$width <- E(g)$weight*.06
      V(g)$label.cex = .7
      return(plot(g, main = "Transition Graph for Mut Group", layout=layout_in_circle(g, order = callOrder), vertex.label.color= "white",
                  vertex.label.family = "Helvetica", edge.label.font = 2))
    }
    else(stop("Upload folder") )
    
  })
  
 output$plot6 = renderPlot({ 
   print(plot6())
 })
 output$downloadPlot6 <- downloadHandler(
   filename = function() { "TransitionGraphforHTGroup.png" },
   content = function(file) {
     ggsave(file, plot = plot5(), device = "png")
   })
 
 
  spls_DA = reactive({ 
    if(!is.null(EntropyAnalysisHT()) & !is.null(EntropyAnalysisWT()) & input$select != 0 & input$percentage != 0) {
      HT_Data = EntropyAnalysisHT()
      WT_Data = EntropyAnalysisWT()
    
      if (input$select == 1){
        FreqDataFrame = cbind(WT_Data$F1, HT_Data$F1)
        tFreqDataFrame = t(FreqDataFrame)
        tFreqDataFrame[is.na(tFreqDataFrame)] <- 0
        Genotype = as.factor(c(c(rep("WT", ncol(WT_Data$F1)), rep("HT", ncol(HT_Data$F1)))))
        Calls = as.factor(alphabetH1)
        keepX = as.double(input$percentage)*nH1
      }
      if (input$select == 2){
        FreqDataFrame = cbind(WT_Data$F2, HT_Data$F2)
        tFreqDataFrame = t(FreqDataFrame)
        tFreqDataFrame[is.na(tFreqDataFrame)] <- 0
        Genotype = as.factor(c(c(rep("WT", ncol(WT_Data$F2)), rep("HT", ncol(HT_Data$F2)))))
        Calls = as.factor(alphabetH2)
        keepX = as.double(input$percentage)*nH2
      }

      if (input$select == 3){
        FreqDataFrame = cbind(WT_Data$F3, HT_Data$F3)
        tFreqDataFrame = t(FreqDataFrame)
        tFreqDataFrame[is.na(tFreqDataFrame)] <- 0
        Genotype = as.factor(c(c(rep("WT", ncol(WT_Data$F3)), rep("HT", ncol(HT_Data$F3)))))
        Calls = as.factor(alphabetH3)
        keepX = as.double(input$percentage)*nH3
      }
      
      ##### spls-DA analysis #####
      
      X = tFreqDataFrame
      Y = Genotype
      calls.splsda <- splsda(X, Y, ncomp = 2, keepX = rep(keepX, 2)) 
      outputdata = list(calls.splsda = calls.splsda, Genotype = Genotype)
      return(outputdata)
    } else(return(NULL) )
  
  })
      
plot7 = reactive({ 
    if(!is.null(spls_DA())){
      splsda = spls_DA()
      return(
      plotIndiv(splsda$calls.splsda, ind.names = splsda$Genotype, comp = c(1, 2),
                 ellipse = TRUE, style = "ggplot2", cex = c(4, 4), title = ""))
    }
    else(stop("Upload folder") )
    
  })
output$plot7= renderPlot({ 
  print(plot7())
})
  output$downloadPlot7 <- downloadHandler(
    filename = function() { "plotIndiv.png" },
    content = function(file) {
      ggsave(file, plot = plot7(), device = "png")
    })
  
plot8 = reactive({ 
    if(!is.null(spls_DA())){
      calls.splsda = spls_DA()
      return(
        plotVar(calls.splsda$calls.splsda,plot = T,abline = T,legend = T))
    }
    else(stop("Upload folder") )
    
  })

output$plot8 = renderPlot({ 
  print(plot8())
})
  output$downloadPlot8 <- downloadHandler(
    filename = function() { "plotVar.png" },
    content = function(file) {
      ggsave(file, plot = plot8(), device = "png")
    })

  

}) ## End of Server function!
  
  
  
  
