

###### C M E   D A T A   P R O C E S S I N G
datasetCME <- function(){

    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/CME")  #set working directory
    
    file1<-"test.txt"
    file2<-"test.csv"
    if(file.exists(file1)){
        file.remove(file1)
    }else if(file.exists(file2)){
        file.remove(file2)
    }
    file.create(file1)
    cat("Date,CME Existance,Amount of Geoeffective CME [PA >= 180],Min Speed [km/s],Max Speed [km/s]", file="test.txt", append=TRUE, sep = "\n") #insert header into dataset file
    
    CMEProcess<-function(data){
        i<-nrow(data)
        dateTemp<-1
        monthYear<-substr(toString(data[i,2]),1,8)
        
        CME_existance<-"No"
        geo_CME<-0
        minSpeed<-0
        maxSpeed<-0
        
        repeat{
            date<-as.integer(substr(toString(data[i,2]),9,10))
           print(i)
           cat("\n")
            
            if(dateTemp == date){
                CME_existance<-"Yes"
                
                if(data[i,5] >= 180){ 
                    geo_CME<-geo_CME+1
                    minSpeed<-minSpeed+data[i,9]
                    maxSpeed<-maxSpeed+data[i,10]
                }
                i<-i-1 #decrease iteration
                if(i==0){
                    dateIn<-sprintf("%s%d",monthYear, dateTemp)
                    datasetIn<-sprintf("%s,%s,%d,%d,%d",dateIn, CME_existance, geo_CME, minSpeed, maxSpeed)
                    cat(datasetIn, file="test.txt", append=TRUE, sep = "\n") #input data into dataset file
                }
            }else{
                dateIn<-sprintf("%s%d",monthYear, dateTemp)
                datasetIn<-sprintf("%s,%s,%d,%d,%d",dateIn, CME_existance, geo_CME, minSpeed, maxSpeed)
                cat(datasetIn, file="test.txt", append=TRUE, sep = "\n") #input data into dataset file
                
                dateTemp<-dateTemp+1
                CME_existance<-"No"
                geo_CME<-0
                minSpeed<-0
                maxSpeed<-0
                
            }
            if (i == 0) break #stopping iteration    
        }
    }
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        t <- read.csv(files[i])
        CMEProcess(t)
        #print(t)
    }
    file.rename(from = file.path("test.txt"), to = file.path("test.csv"))
}








###### S O L A R   W I N D   M A G   D A T A   P R O C E S S I N G
datasetSWMAG <- function(){
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/SW MAG")  #set working directory
    
    #creata dataset file
    file1<-"test.txt"
    file2<-"test.csv"
    if(file.exists(file1)){
        file.remove(file1)
    }else if(file.exists(file2)){
        file.remove(file2)
    }
    file.create(file1)
    cat("Date, Solar Wind (IMF Bz) Min Approx [nT], Solar Wind (IMF Bz) Max Approx [nT]", file="test.txt", append=TRUE, sep = "\n") #insert header
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        t <- read.csv(files[i]) #read data
        t <-t[t$Bz != "-999.9", ] #delete all row with missing values
        
        dateIn<-sprintf("%d/%d/%d",t[1,1],t[1,2],t[1,3])
        minBz<-min(t[,10])
        maxBz<-max(t[,10])
        
        datasetIn<-sprintf("%s,%f,%f",dateIn, minBz, maxBz)
        cat(datasetIn, file="test.txt", append=TRUE, sep = "\n") #input data into dataset file
    }
    file.rename(from = file.path("test.txt"), to = file.path("test.csv"))
}








###### S O L A R   W I N D   S W E P A M   D A T A   P R O C E S S I N G
datasetSWSWEPAM <- function(){
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/SW SWEPAM")  #set working directory
    
    #create dataset file
    file1<-"test.txt"
    file2<-"test.csv"
    if(file.exists(file1)){
        file.remove(file1)
    }else if(file.exists(file2)){
        file.remove(file2)
    }
    file.create(file1)
    cat("Date, Solar Wind (Speed) Flux Before [Km/s], Solar Wind (Speed) Flux After [Km/s], Solar Wind (Density) Min Approx [/cc], Solar Wind (Density) Max Approx [/cc]", file="test.txt", append=TRUE, sep = "\n") #insert header
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        t <- read.csv(files[i]) #read data
        t <-t[t[,8] != "-9999.9", ] #delete all row with missing values
        t <-t[t[,9] != "-9999.9", ] #delete all row with missing values
        nt <- nrow(t)
        
        dateIn<-sprintf("%d/%d/%d",t[1,1],t[1,2],t[1,3])
        minDensity<-min(t[,8])
        maxDensity<-max(t[,8])
        beforeSpeed<-t[1,9]
        afterSpeed<-t[nt,9]
        
        datasetIn<-sprintf("%s,%f,%f,%f,%f",dateIn, beforeSpeed, afterSpeed, minDensity, maxDensity)
        cat(datasetIn, file="test.txt", append=TRUE, sep = "\n") #input data into dataset file
    }
    file.rename(from = file.path("test.txt"), to = file.path("test.csv"))
}








###### C O R O N A L   H O L E   D A T A   P R O C E S S I N G
datasetCH <- function(){
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/CH")  #set working directory
    
    #create dataset file
    file1<-"test.txt"
    file2<-"test.csv"
    if(file.exists(file1)){
        file.remove(file1)
    }else if(file.exists(file2)){
        file.remove(file2)
    }
    file.create(file1)
    cat("Date, Total CH Event, Total Area[Mm2]", file="test.txt", append=TRUE, sep = "\n") #insert header
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        t <- read.csv(files[i]) #read data
        
        ymd<-strsplit(toString(t[1,1]), "-")
        dateIn<-sprintf("%s/%s/%s",ymd[[1]][1],ymd[[1]][2],ymd[[1]][3])
        
        nCH<-nrow(t)
        totalArea<-sum(t[,4])
        totalArea<-round(totalArea/1000000)
    
        
        datasetIn<-sprintf("%s,%d,%d",dateIn, nCH, totalArea)
        cat(datasetIn, file="test.txt", append=TRUE, sep = "\n") #input data into dataset file
    }
    file.rename(from = file.path("test.txt"), to = file.path("test.csv"))
}









###### D s t   D A T A   P R O C E S S I N G 
datasetDST <- function(){
    setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/Dst")  #set working directory
    
    #creata dataset file
    file1<-"test.txt"
    file2<-"test.csv"
    if(file.exists(file1)){
        file.remove(file1)
    }else if(file.exists(file2)){
        file.remove(file2)
    }
    file.create(file1)
    cat("Date,Min Dst [nT],Judgement", file="test.txt", append=TRUE, sep = "\n") #insert header into dataset file
    
    DstProcess<-function(data){
        i<-0
        end<-nrow(data)
        
        repeat{
            i<-i+1
            year<-toString(substr(data[i,1],1,4))
            month<-toString(substr(data[i,1],5,6))
            date<-toString(substr(data[i,1],7,8))
            
            dateIn<-sprintf("%s/%s/%s",year,month,date)
            minDst<-data[i,26]
            judge<-data[i,27]
                            
            datasetIn<-sprintf("%s,%f,%s",dateIn, minDst, judge)
            cat(datasetIn, file="test.txt", append=TRUE, sep = "\n") #input data into dataset file
            
            
            if (i == end) break #stopping iteration    
        }
    }
    
    files <- list.files(pattern = ".csv")
    for (i in seq_along(files)) {
        t <- read.csv(files[i])
        DstProcess(t)
        #print(t)
    }
    file.rename(from = file.path("test.txt"), to = file.path("test.csv"))
}




