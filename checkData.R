

library(gdata)

##### LOAD CH DATA
loadCH <- function(){

    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/CH")  #set working directory
    dataTable<-matrix(ncol=1)
    
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        name<-basename(files[i])
        dataTable<-rbind(dataTable, c(name))  
    }
    dataTable<-as.matrix(dataTable[-1,])
    
    if(file.exists("test.csv")){
        lastData<-nrow(dataTable)
        dataTable<-as.matrix(dataTable[-lastData,])
    }
    
return(dataTable)
}

##### DATA MAG DATA
loadSWmag <- function(){
    
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/SW MAG")  #set working directory
    dataTable<-matrix(ncol=1)
    
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        name<-basename(files[i])
        dataTable<-rbind(dataTable, c(name))  
    }
    dataTable<-as.matrix(dataTable[-1,])
    
    if(file.exists("test.csv")){
        lastData<-nrow(dataTable)
        dataTable<-as.matrix(dataTable[-lastData,])
    }
        
return(dataTable)
}

##### DATA SWEPAM DATA
loadSWswepam <- function(){
    
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/SW SWEPAM")  #set working directory
    dataTable<-matrix(ncol=1)
    
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        name<-basename(files[i])
        dataTable<-rbind(dataTable, c(name))  
    }
    dataTable<-as.matrix(dataTable[-1,])
    
    if(file.exists("test.csv")){
        lastData<-nrow(dataTable)
        dataTable<-as.matrix(dataTable[-lastData,])
    }
        
return(dataTable)
}

##### LOAD DST DATA
loadDST <- function(){    
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/Dst")  #set working directory
    dst <- read.csv(file="test.csv", header=TRUE, sep=",")
    dst<-as.matrix(dst)
    dstTable<-matrix(ncol=1)
    
    for(i in 1:nrow(dst)){
        data<-strsplit(dst[i,1],"/")
        data<-sprintf("%s%s%s",data[[1]][1],data[[1]][2],data[[1]][3])
        data<-sprintf("%s.csv",data)
        dstTable<-rbind(dstTable, c(data))
    }
    dstTable<-as.matrix(dstTable[-1,])
    
    return(dstTable)        
}

##### DATA CME
loadCME <- function(){ 
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/CME")  #set working directory
    cme <- read.csv(file="test.csv", header=TRUE, sep=",")
    cme<-as.matrix(cme)
    dataTable<-matrix(ncol=1)
    
    for(i in 1:nrow(cme)){
        data<-strsplit(cme[i,1],"/")
        
        if(nchar(toString(data[[1]][1]))<2){
            tgl<-sprintf("0%s",data[[1]][1])
        }else{
            tgl<-toString(data[[1]][1])
        }
        
        if(nchar(toString(data[[1]][2]))<2){
            bln<-sprintf("0%s",data[[1]][2])
        }else{
            bln<-toString(data[[1]][2])
        }
        
        data<-sprintf("%s%s%s",data[[1]][3],tgl,bln)
        data<-sprintf("%s.csv",data)
        dataTable<-rbind(dataTable, c(data))
    }
    dataTable<-as.matrix(dataTable[-1,])
    
    return(dataTable)
}
         
##### CHECK DATA SOLAR WIND
#input param: "SWMAG", "SWSWEPAM", "DST", "CME"
checkData <-function(data){
    missingData<-0
    
    CH<-loadCH()
    if(data=="SWMAG"){
        data2<-loadSWmag()
    }else if(data=="SWSWEPAM"){
        data2<-loadSWswepam()
    }else if(data=="DST"){
        data2<-loadDST()
    }else if(data=="CME"){
        data2<-loadCME()
    }
    
    for (i in 1:nrow(CH)) {
        if(CH[i] != data2[i]){
            missingData<-missingData+1
            
            cat("Found missing data ! \n")
            cat("",CH[i])
            cat(" is missing from")
            cat("", data)
            cat("\n")
            break
        }else{
            #do nothing
        }
    }
    
    if(missingData==0){
        cat("All data complete\n")
    }
    #View(cbindX(CH,data2))
}
