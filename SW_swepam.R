### Website Scrapping - Solar Winds SWEPAM

#load Library
library(RCurl)
setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/SW SWEPAM")  #set working directory



#grouping Solar Winds data retrieved from website into table
createTable <- function(data){
    i<-18 #initiate iteration
    
    #initiate matrix table
    SWEPAM<-matrix(ncol=10)
    #naming header
    colnames(SWEPAM)<-c("year","month","date","time(HHMM)","modified julian day","seconds of the day","S","proton density","bulk speed","ion temperature") 
    
    repeat { 
        i<-i+1 #iteration
        temp<-unlist(strsplit(data[[i]], "\\s+"))  #split string
        SWEPAM<-rbind(SWEPAM, c(temp[1],temp[2],temp[3],temp[4],temp[5],temp[6],temp[7],temp[8],temp[9],temp[10]))    #append data found to matrix table
        
        if (i == length(data)) break #stopping iteration
    }
    
    return(SWEPAM)
}


#input year of data that will be retrieved 
year<-2012
month<-1

#get data from website then store it into a new csv file
for(month in 1:12){
    date<-1
    
    for(date in 1:31){
        start.time <- Sys.time()
            
        #check month and date string, cause it should be 2 characters long
        if(nchar(toString(month))<2){
            bln<-sprintf("0%d",month)
        }else{
            bln<-toString(month)
        }
        
        if(nchar(toString(date))<2){
            tgl<-sprintf("0%d",date)
        }else{
            tgl<-toString(date)
        }
        
        
        
        
        #date of data
        dataDate<-sprintf("%d%s%s",year, bln, tgl) 
        print(dataDate)
        
        #set URL link    
        url1<-"ftp://sohoftp.nascom.nasa.gov/sdb/goes/ace/daily/"
        url2<-dataDate;
        url3<-"_ace_swepam_1m.txt"
        #concate url string
        url<-sprintf("%s%s%s",url1, url2, url3)
        print(url)
        
        #check whether url exists
        print("1. checking url link..........")
        check<-""
        try(check<- getBinaryURL(url, failonerror = TRUE))
        
        if (length(check) > 1) { #if url is exist
            print("2. retrieving data..........")
            text_data <- readLines(file(url,open="r"))
            
            print("3. creating table..........")
            temp<-createTable(text_data)
            temp<-temp[-1,]
            temp2<-data.frame(temp)
            
            cat("4. creating file..........\n\n")
            fileName<-sprintf("%s.csv",dataDate)
            write.csv(temp2, file=fileName, row.names = FALSE)
        }
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        cat(time.taken)
        
    }
}

