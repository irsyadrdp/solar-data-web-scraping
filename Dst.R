### Website Scrapping - Dst

#load XML library
library(XML)
setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/Dst")  #set working directory


#grouping Dst indices data retrieved from website into table
createTable <- function(data, dataDate){
    #initiate matrix table
    DST<-matrix(ncol=27)
    #naming header
    colnames(DST)<-c("date","Hour 1","Hour 2","Hour 3","Hour 4","Hour 5","Hour 6","Hour 7","Hour 8","Hour 9","Hour 10","Hour 11","Hour 12","Hour 13","Hour 14","Hour 15","Hour 16","Hour 17","Hour 18","Hour 19","Hour 20","Hour 21","Hour 22","Hour 23","Hour 24","Min Dst","Judgement")
    
    i<-0	#1st date of the month
    lastDate<-length(data)/25	#last date of the month
    numA<-1	#1st index data
    
    
    repeat{
        i<-i+1 #iteration
        
        #date element
        if(nchar(toString(i))<2){
            tgl<-sprintf("0%d",i)
        }else{
            tgl<-toString(i)
        }
        date<-as.integer(sprintf("%s%s", dataDate, tgl))
        
        numB<-numA+24	#data index of hour 24
        firstHour<-numA+1	#data index of hour 1
        
        minDst<-min(data[firstHour:numB])	#minimum Dst 
        if(minDst >= -25.4){
            storm<-"quiet"
        }else if(minDst <= -25.5 && minDst >= -44.9){
            storm<-"active"
        }else if(minDst <= -45 && minDst >= -79.2){
            storm<-"minor storm"
        }else if(minDst <= -79.3 && minDst >= -139.6){
            storm<-"moderate storm"
        }else if(minDst <= -139.7 && minDst >= -245.9){
            storm<-"major storm"
        }else if(minDst <= -246){
            storm<-"severe storm"
        }
        
        
        DST<-rbind(DST,c(date, data[firstHour:numB], minDst, storm))
        
        numA<-numB+1
        if (i == lastDate) break #stopping iteration
    }
    
    return(DST)
}



#input year of data that will be retrieved 
year<-2012
month<-1

#get data from website then store it into a new csv file
for(month in 1:12){
    start.time <- Sys.time()
    
    #check month string, cause it should be 2 characters long
    if(nchar(toString(month))<2){
        bln<-sprintf("0%d",month)
    }else{
        bln<-toString(month)
    }
    
    #date of data
    dataDate<-sprintf("%d%s",year, bln) 
    print(dataDate)
    
    #set URL link    
    if(year<=2013){
        url1<-"http://wdc.kugi.kyoto-u.ac.jp/dst_final/"
    }else if(year==2014 || year==2015){
        url1<-"http://wdc.kugi.kyoto-u.ac.jp/dst_provisional/"
    }else if(year==2016 || year==2017){
        url1<-"http://wdc.kugi.kyoto-u.ac.jp/dst_realtime/"
    }
    url2<-dataDate;
    url3<-"/index.html"
    
    #concate url string
    link<-sprintf("%s%s%s",url1, url2, url3)
    print(link)
    
    
    #read and parse HTML file
    url<-htmlTreeParse(link, useInternal = TRUE)
    
    #retrieve data	
    print("1. retrieving data..........")	
    data<-unlist(xpathApply(url, '//pre', xmlValue))
    data<-gsub('\\n', ' ', data)	# Replace all \n by spaces
    data<-paste(data, collapse = ' ') # Join all the elements of the character vector into a single character string, separated by spaces
    data<-as.matrix(strsplit(data, "\\s+")[[1]]) #split string by multiple spaces
    data<-as.matrix(data[-(1:39)]) #delete data description
    
    
    #checking unsplitted data
    print("2. checking unsplitted data..........")	
    for(i in 1:length(data)){
        if(nchar(data[i])>4){
            temp<-as.matrix(strsplit(data[i], "-")[[1]])
            
            temp <- apply(temp, 1:2, function(x) sprintf("-%s",x))
            temp <- apply(temp, 1:2, function(x) as.integer(x))
            
            if(substr(data[i],1,1)=="-"){
                temp<-as.matrix(temp[-(1:1)])
            }
            
            nextRow<-i+1
            data<-rbind(as.matrix(data[1:i,1]),temp, as.matrix(data[nextRow:length(data),1])) #insert new splitted data
            data<-as.matrix(data[-(i)]) #delete unsplitted data
            
        }
    }
    #convert all data to integer
    data<-apply(data, 1:2, function(x) as.integer(x))
    
    
    #insert data into table
    print("3. creating table..........")
    temp<-createTable(data, dataDate)
    temp<-temp[-1,]
    temp2<-data.frame(temp)
    
    cat("4. creating file..........\n\n")
    fileName<-sprintf("%s.csv",dataDate)
    write.csv(temp2, file=fileName, row.names = FALSE)
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    cat(time.taken)
    
}


