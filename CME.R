### Website Scrapping - Coronal Mass Ejection

#load Library
library(RCurl)
setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/CME")  #set working directory



#grouping CME data retrieved from website into table
createTable <- function(data){
    i<-27 #initiate iteration
    stop<-which(text_data==" ")-1 #last retrieve data index
    
    #initiate matrix table
    CME<-matrix(ncol=10)
    #naming header
    colnames(CME)<-c("CME no.","date","t0","dt0","pa","da","v","dv","minv","maxv") 
    
    repeat { 
        i<-i+1 #iteration
        
        #split string
        a<-unlist(strsplit(data[i], "\\|"))
        b<-paste(a, collapse = ' ')
        temp<-unlist(strsplit(b, "\\s+"))
        
        CME<-rbind(CME, c(temp[2],temp[3],temp[4],temp[5],temp[6],temp[7],temp[8],temp[9],temp[10],temp[11]))    #append data found to matrix table
        
        if (i == stop) break #stopping iteration
    }
    
    return(CME)
}


#input year of data that will be retrieved 
year<-2011
month<-1

#get data from website then store it into a new csv file
for(month in 1:12){
    start.time <- Sys.time()
    
    #check month and date string, cause it should be 2 characters long
    if(nchar(toString(month))<2){
        bln<-sprintf("0%d",month)
    }else{
        bln<-toString(month)
    }
    
    
    #date of data
    dataDate<-sprintf("%d/%s",year, bln)
    fileName<-sprintf("%d%s",year, bln)
    print(dataDate)
    
    #set URL link    
    url1<-"http://sidc.oma.be/cactus/catalog/LASCO/2_5_0/qkl/"
    url2<-dataDate;
    url3<-"/cmecat.txt"
    #concate url string
    url<-sprintf("%s%s%s",url1, url2, url3)
    print(url)
    
    #retrieving data	
    print("1. retrieving data..........")
    text_data <- readLines(file(url,open="r"))
    
    print("2. creating table..........")
    temp<-createTable(text_data)
    temp<-temp[-1,]
    temp2<-data.frame(temp)
    
    cat("3. creating file..........\n\n")
    fileName<-sprintf("%s.csv",fileName)
    write.csv(temp2, file=fileName, row.names = FALSE)
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    cat(time.taken)
}
