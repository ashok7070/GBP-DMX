###-------------------------
list.of.packages <- c("data.table","stringi","dplyr","stringr")
new.packages <- list.of.packages [!(list.of.packages %in% installed.packages()[,"Package"] )]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
rm(new.packages, list.of.packages)

library(dplyr)
library(stringi)
library(stringr)
library(data.table)

##---------------------------------------------

##---------------------------------------------

workDir <- "D://DMX_APP/Files"
csvFileListPath <- "Driver.csv"
colsSeparator <- "\\|"
outputFileName <- "result.csv"
outputFileNameHash <- "resultHash.csv"

##------------------------------------------
setwd(workDir);
##------------------------------------------

# read the file holding the list of other csv files
csvFileList <- read.csv(csvFileListPath, stringsAsFactors = F)

#Funcation to read the files

selectColsFromTypeB <- function(filePath,cols,header,footer){
  filePath <- dir(pattern= filePath,full.names = TRUE)
  fileData <- fread(filePath, sep= "auto",header =TRUE,stringsAsFactors =F,skip =header-1,nrows = (length(readLines(filePath))-footer),keepLeadingZeros=TRUE)
  selectedCols <- select(fileData, one_of(cols))
}  

selectColsFromTypeA <- function(filePath,cols){
  filePath <- dir(pattern= filePath,full.names = TRUE)
  fileData <- fread(filePath, sep= "auto",header =TRUE,stringsAsFactors =F,keepLeadingZeros=TRUE)
  selectedCols <- select(fileData, one_of(cols))
}  

selectHashFromTypeB <- function(filePath,Hash,header,footer){
  filePath <- dir(pattern= filePath,full.names = TRUE)
  fileData <- fread(filePath, sep= "auto",header =TRUE,stringsAsFactors =F,skip =header-1,nrows = (length(readLines(filePath))-footer),keepLeadingZeros=TRUE)
  selectedCols <- select(fileData, one_of(Hash))
}  

selectHashFromTypeA <- function(filePath,Hash){
  filePath <- dir(pattern= filePath,full.names = TRUE)
  fileData <- fread(filePath, sep= "auto",header =TRUE,stringsAsFactors =F,keepLeadingZeros=TRUE)
  selectedCols <- select(fileData, one_of(Hash))
} 


##------------

extractColumnsFromFiles <- function() {
  #list of all data frames built from the csvs
  
  allDfs <- list()
  
  #list of all file names
  
  allFileNames <- csvFileList$fileName
  
  #for each file
  for (row in 1:nrow(csvFileList)) {
    #fileName and columns
    fileNameAndCols <- csvFileList[row,]
    
    #file Name
    fileName <- csvFileList[row,]$fileName
    fileType <- csvFileList[row,]$fileType
    #list of columns is formed by splitting the string containing 
    # column names separated by | separator
    cols <- strsplit(csvFileList[row,]$cols, colsSeparator)
    #prepend the filename to each column name
    newColNames <- paste(fileName, unlist(cols),sep = ".")
    
    header_CN <- which(colnames(csvFileList)=="header")
    footer_CN <- which(colnames(csvFileList)=="footer")
    
    header <- csvFileList[row,header_CN]
    footer <- csvFileList[row,footer_CN]
    
    
    #select the required columns
    
    if (fileType %like% "TypeB"|fileType %like% "TypeA"|fileType %like% "TypeJ"|fileType %like% "TypeC"|fileType %like% "TypeD")
    {
      df <- selectColsFromTypeB (fileName,unlist (cols),header,footer)
    }
    
    else 
      df <- selectColsFromTypeA(fileName,unlist(cols))
    
    #update the column names
    colnames(df) <- newColNames
    #store all dataframes in a list
    allDfs[[row]] <- df
  }
  
  #merge all data frames
  
  DF <- data.frame()
  a <- data.frame()
  b <- data.frame()
  
  #for each data frame created from the corresponding csv file
  
  for (.df in allDfs) {
    #concate all columns
    DF <- bind_rows(DF,.df)
    
  }
  return(DF)
  
  
}

extractHashFromFiles <- function() {
  #list of all data frames built from the csvs
  
  allDfsHash <- list()
  #list of all file names
  allFileNames <- csvFileList$fileName
  
  #for each file
  for (row in 1:nrow(csvFileList)) {
    #fileName and columns
    fileNameAndHash <- csvFileList[row,]
    
    #file Name
    fileName <- csvFileList[row,]$fileName
    fileType <- csvFileList[row,]$fileType
    #list of columns is formed by splitting the string containing 
    # column names separated by | separator
    Hash <- strsplit(csvFileList[row,]$Hash, colsSeparator)
    #prepend the filename to each column name
    newHashNames <- paste(fileName, unlist(Hash),sep = ".")
    
    header_CN <- which(colnames(csvFileList)=="header")
    footer_CN <- which(colnames(csvFileList)=="footer")
    
    header <- csvFileList[row,header_CN]
    footer <- csvFileList[row,footer_CN]
    
    
    #select the required columns
    
    if (fileType %like% "TypeB"|fileType %like% "TypeA"|fileType %like% "TypeJ"|fileType %like% "TypeC"|fileType %like% "TypeD")
    {
      dfHash <- selectHashFromTypeB (fileName,unlist (Hash),header,footer)
    }
    
    else 
      dfHash <- selectHashFromTypeA(fileName,unlist(Hash))
    
    #update the column names
    colnames(dfHash) <- newHashNames
    #store all dataframes in a list
    allDfsHash[[row]] <- dfHash
  }
  
  #merge all data frames
  
  DFHash <- data.frame()
  
  #for each data frame created from the corresponding csv file
  
  for (.dfHash in allDfsHash) {
    #concate all columns
    DFHash <- bind_rows(DFHash,.dfHash)
    
  }
  return(DFHash)
  
  
}

#Generation of random string - uses library stringi // Creation of the conversion table (input values)

Random_String <- stri_rand_strings(1,36,pattern = "[A-Z]")
#Number <- c(0:9, LETTERS[seq(from = 1, to = 26)])
Number <- c('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')

#Hashing of the created random string into separte varibales (1 varibale per charcter of the string)// it will allow the hashing per character
Hash_Table <- NULL

for (i in 1:36){
  Hash_Table <- append(Hash_Table,substr(Random_String, i, i))
}

#Merging and conversion of the conversion table into a single data frame (creation of a input to output table)
Hash_Table <- data.frame(Hash_Table)
Hash_Table$Number <- Number
colnames(Hash_Table) <- c("Output","Input")

DF <- extractColumnsFromFiles()
DFHash <- extractHashFromFiles()

#create the resulting output file

dfColNames <- data.frame()
dfHashNames <- data.frame()
dfColNames <- colnames(DF)
dfHashNames <- colnames(DFHash)

for (i in 1:ncol(DFHash)) {
  for (n in 1:36) {
    DFHash[,i] <- gsub(Hash_Table$Input[n],Hash_Table$Output[n], DFHash[,i],ignore.case = TRUE)
    
  }
}

for(i in 1:ncol(DF)){
  for (n in 1:ncol(DFHash)) {
    
    if (dfHashNames[n]==dfColNames[i])
    {
      DF[,i] <- DFHash[,n]
    }
  }
}


# writing in out file

header_CN <- which(colnames(csvFileList)== "header")
footer_CN <- which(colnames(csvFileList)== "footer")

fileName_CN <- which(colnames(csvFileList)== "fileName")
fileType_CN <- which(colnames(csvFileList)== "fileType")

colSeparator_CN <- which(colnames(csvFileList)== "colSeparator")
lineSeparator_CN <- which(colnames(csvFileList)== "lineSeparator")
fileExtension_CN <- which(colnames(csvFileList)== "fileExtension")

r <- csvFileList[,fileName_CN]
r1 <- csvFileList[,fileType_CN]

colSeparator <- csvFileList[,colSeparator_CN]
header <- csvFileList[,header_CN]
footer <- csvFileList [,footer_CN]
lineSeparator <- csvFileList[,lineSeparator_CN]
fileExtension <- csvFileList[,fileExtension_CN]

for (m in 1:nrow(csvFileList)) {
  
  filename = paste('Masked_',r[m],sep ="")
  file1= r[m]
  
  if (r1[m] %like% "TypeA")
  {
    DFOUT <- DF %>% select(starts_with(file1))
    DFOUT <- subset(DFOUT,DFOUT[,1] !="" | DFOUT[,2] != "" |DFOUT[,3] != "")
    colnames(DFOUT) <- sub(".*\\.","",colnames(DFOUT))
    fwrite(DFOUT,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
    
  }
  else if (r1[m] %like% "TypeB")
  {
    DFOUT <- DF %>% select(starts_with(file1))
    DFOUT <- subset(DFOUT,DFOUT[,1] !="" | DFOUT[,2] != "" |DFOUT[,3] != "")
    colnames(DFOUT) <- sub(".*\\.","",colnames(DFOUT))
    r[m] <- dir(pattern = r[m],full.names = TRUE)
    Header <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, nrows = ((length(readLines(r[m])))- (length(readLines(r[m]))-(header[m]-1))))
    Footer <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, skip =  (nrows = length(readLines(r[m]))- footer[m]))
    fwrite(Header,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = FALSE)
    fwrite(DFOUT,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = TRUE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
    fwrite(Footer,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = TRUE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = FALSE)
  }
  
  else if (r1[m] %like% "TypeC")
  {
    DFOUT <- DF %>% select(starts_with(file1))
    DFOUT <- subset(DFOUT,DFOUT[,1] !="" | DFOUT[,2] != "" |DFOUT[,3] != "")
    colnames(DFOUT) <- sub(".*\\.","",colnames(DFOUT))
    r[m] <- dir(pattern = r[m],full.names = TRUE)
    Header <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, nrows = ((length(readLines(r[m])))- (length(readLines(r[m]))-(header[m]-1))))
    #Footer <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, skip =  (nrows = length(readLines(r[m]))- footer[m]))
    fwrite(Header,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = FALSE)
    fwrite(DFOUT,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = TRUE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
    #fwrite(Footer,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = TRUE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
  }
  
  else if (r1[m] %like% "TypeD")
  {
    DFOUT <- DF %>% select(starts_with(file1))
    DFOUT <- subset(DFOUT,DFOUT[,1] !="" | DFOUT[,2] != "" |DFOUT[,3] != "")
    colnames(DFOUT) <- sub(".*\\.","",colnames(DFOUT))
    r[m] <- dir(pattern = r[m],full.names = TRUE)
    #Header <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, nrows = ((length(readLines(r[m])))- length(readLines(r[m]))-(header[m]-1)))
    Footer <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, skip =  (nrows = length(readLines(r[m]))- footer[m]))
    #fwrite(Header,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
    fwrite(DFOUT,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
    fwrite(Footer,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = TRUE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = FALSE)
  }
  
  else { !(r1[m] %like% "TypeA")
    {
      DFOUT <- DF %>% select(starts_with(file1))
      DFOUT <- subset(DFOUT,DFOUT[,1] !="" | DFOUT[,2] != "" |DFOUT[,3] != "")
      colnames(DFOUT) <- sub(".*\\.","",colnames(DFOUT))
      #r[m] <- dir(pattern = r[m],full.names = TRUE)
      #Header <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, nrows = ((length(readLines(r[m])))- length(readLines(r[m]))-(header[m]-1)))
      #Footer <- fread(r[m], sep = "",header = FALSE, stringsAsFactors = F, skip =  (nrows = length(readLines(r[m]))- footer[m]))
      #fwrite(Header,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
      fwrite(DFOUT,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = FALSE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
      #fwrite(Footer,file=paste(filename,format(Sys.time(), "%d-%b-%Y%H.%M.%S"),fileExtension[m],sep = ""),append = TRUE,quote = FALSE,sep = colSeparator[m],eol = if(lineSeparator[m]=="LF")"\n" else "\r\n",dec=".",row.names = FALSE,col.names = TRUE)
  }}
}



