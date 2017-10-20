
library(readr)
library(gtools)
library(stringr)
library(stringi)
library(hash)

### ----- Get intial time ----- ###
start_time <- Sys.time()

### ----- Set working drectory ----- ###
setwd("~/Desktop/newInterface")


### ----- Get alphabet from file ----- ###

dataH1 = read_file("~/Desktop/appGit/alphabetFile.txt")

alphabetFromText = unlist(str_split(dataH1, ","))

tFile = read_file("TestVocalFile.csv")
testFile = unlist(str_split(tFile, "\n"))
noTab = gsub('[\t]', '_', testFile)


vocalizationFile = noTab[which(noTab!="_")]

### ------ Entropy Functions ------ ###

H1 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.1)){
    entropy = -1*as.double(Probabilities.1[call])*log2(as.double(Probabilities.1[call]))
    sumH = sumH + entropy
  }
  return(sumH)
}

#level1 = H1()

H2 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.2)){
    getFirst = unlist(str_split(call,"_"))[1]
    entropy = -1*as.double(Probabilities.1[getFirst])*as.double(Probabilities.2[call])*log2(as.double(Probabilities.2[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level2 = H2()

H3 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.3)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[call])*log2(as.double(Probabilities.3[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level3 = H3()

H4 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.4)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[call])*log2(as.double(Probabilities.4[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level4 = H4(sumH)

H5 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.5)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[paste(getC[1],getC[2],getC[3],getC[4],sep="_")])*as.double(Probabilities.5[call])*log2(as.double(Probabilities.5[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level5 = H5(sumH)

H6 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.6)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[paste(getC[1],getC[2],getC[3],getC[4],sep="_")])*as.double(Probabilities.5[paste(getC[1],getC[2],getC[3],getC[4],getC[5],sep="_")])*as.double(Probabilities.6[call])*log2(as.double(Probabilities.6[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level6 = H6(Probabilities.1,Probabilities.2, Probabilities.3, Probabilities.4, Probabilities.5, Probabilities.6)

H7 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.7)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[paste(getC[1],getC[2],getC[3],getC[4],sep="_")])*as.double(Probabilities.5[paste(getC[1],getC[2],getC[3],getC[4],getC[5],sep="_")])*as.double(Probabilities.6[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],sep="_")])*as.double(Probabilities.7[call])*log2(as.double(Probabilities.7[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level7 = H7(Probabilities.1,Probabilities.2, Probabilities.3, Probabilities.4, Probabilities.5, Probabilities.6, Probabilities.7)


H8 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.8)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[paste(getC[1],getC[2],getC[3],getC[4],sep="_")])*as.double(Probabilities.5[paste(getC[1],getC[2],getC[3],getC[4],getC[5],sep="_")])*as.double(Probabilities.6[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],sep="_")])*as.double(Probabilities.7[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],getC[7],sep="_")])*as.double(Probabilities.8[call])*log2(as.double(Probabilities.8[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level8 = H8(Probabilities.1,Probabilities.2, Probabilities.3, Probabilities.4, Probabilities.5, Probabilities.6, Probabilities.7,Probabilities.8)

H9 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.9)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[paste(getC[1],getC[2],getC[3],getC[4],sep="_")])*as.double(Probabilities.5[paste(getC[1],getC[2],getC[3],getC[4],getC[5],sep="_")])*as.double(Probabilities.6[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],sep="_")])*as.double(Probabilities.7[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],getC[7],sep="_")])*as.double(Probabilities.8[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],getC[7],getC[8],sep="_")])*as.double(Probabilities.9[call])*log2(as.double(Probabilities.9[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level9 = H9(Probabilities.1,Probabilities.2, Probabilities.3, Probabilities.4, Probabilities.5, Probabilities.6, Probabilities.7,Probabilities.8, Probabilities.9)

H10 = function(sumH){
  sumH = 0
  for(call in names(Probabilities.10)){
    getC = unlist(str_split(call,"_"))
    entropy = -1*as.double(Probabilities.1[getC[1]])*as.double(Probabilities.2[paste(getC[1],getC[2],sep="_")])*as.double(Probabilities.3[paste(getC[1],getC[2],getC[3],sep="_")])*as.double(Probabilities.4[paste(getC[1],getC[2],getC[3],getC[4],sep="_")])*as.double(Probabilities.5[paste(getC[1],getC[2],getC[3],getC[4],getC[5],sep="_")])*as.double(Probabilities.6[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],sep="_")])*as.double(Probabilities.7[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],getC[7],sep="_")])*as.double(Probabilities.8[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],getC[7],getC[8],sep="_")])*as.double(Probabilities.9[paste(getC[1],getC[2],getC[3],getC[4],getC[5],getC[6],getC[7],getC[8],getC[9],sep="_")])*as.double(Probabilities.10[call])*log2(as.double(Probabilities.10[call]))
    if (!is.nan(entropy)){
      sumH = sumH + entropy}
    
  }
  return(sumH)
}

#level10 = H9(Probabilities.1,Probabilities.2, Probabilities.3, Probabilities.4, Probabilities.5, Probabilities.6, Probabilities.7,Probabilities.8, Probabilities.9, Probabilities.10)
### ----- Get all alphabets for entropy analysis ----- ###
choice = 5
## Create variables based on user choice of entropy level ##
for (i in 1:choice){
  
  assign(paste("Alphabet.", i, sep = ""), permutations(n=length(alphabetFromText),r=i,v=alphabetFromText,repeats.allowed=T))
  assign(paste("Count.", i, sep = ""), list())
}

### ------ Count Function ------ ###
EntropyAllLevels = c()
for(level in 1:choice){
  print(level)
  getCalls = c() ## This vector will have all the possible calls for each entropy level 
  for(rowN in 1:nrow(get(paste("Alphabet.", level, sep = "")))){
    getCalls = c(getCalls, paste(get(paste("Alphabet.", level, sep = ""))[rowN,], collapse="_"))
    
    }
  ### Counts ###
  getCounts = c()
  for(call in getCalls){
    sumC = sum(stri_count_regex(vocalizationFile, call))
    getCounts = c(getCounts,sumC)
  }
  assign(paste("Count.", level, sep = ""),setNames(c(getCounts),getCalls)) # Set the names of count vector as the calls and values as call counts

  ### Probability ###
  getProb = c()
  getTotal = sum(get(paste("Count.", level, sep = "")))
  for(call in getCalls){
    Prob = get((paste("Count.", level, sep = "")))[call]/getTotal
    getProb = c(getProb,Prob)
  }
  assign(paste("Probabilities.", level, sep = ""),setNames(c(getProb),getCalls))
  sumH = 0
  entropy = do.call(paste("H",level, sep = ""), args = list(sumH))
  EntropyAllLevels = c(EntropyAllLevels,entropy)

    
}




### ----- Get finel time ----- ###
end_time <- Sys.time()

print(end_time-start_time)



