### Website Scrapping - Coronal Hole (CH)

#load rjson library
library(rjson) 
setwd("Z:/TEMP FD/SKRIPSI/Data/Scrap/CH")  #set working directory

  

#grouping Coronal Holes data retrieved from website into table
createTable <- function(data, date){
    i<-0    #initiate iteration
    
    CH<-matrix(ncol=4)                          #initiate matrix table
    colnames(CH)<-c("date","x_coord","y_coord","area") #naming header
    
    repeat { 
        i<-i+1 #iteration
        
        area<-data$result[[i]]$area_atdiskcenter    #get area data    
        coord<-data$result[[i]]$hpc_coord           #get coordinate data
            #split coordinate string
            coord<-unlist(strsplit(coord, "[ ]"))       
            coord[1]<-substr(coord[1],7,nchar(coord[1]))        
            coord[2]<-substr(coord[2],1,nchar(coord[2])-1)
        
        CH<-rbind(CH, c(date, coord[1], coord[2], area))    #append data found to matrix table
        
        if (i == length(data$result)) break #stopping iteration
    }
return(CH)
}

#input year of data that will be retrieved 
#n <- readline(prompt="Enter an integer: ")
year<-2016
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





        #start date of data
        startDate<-sprintf("%d-%d-%d",year, month, date) 
        dateFilename<-sprintf("%d%s%s",year, bln, tgl) 
        
        #end date of data
        if(month==12 && date==31){
            endDate<-sprintf("%d-%d-%d",year+1, 1, 1)
        }
        else if(date==31){
            endDate<-sprintf("%d-%d-%d",year, month+1, 1)
        }else{
            endDate<-sprintf("%d-%d-%d",year, month, date+1)    
        }
         
        #set URL link    
        url1<-"https://www.lmsal.com/hek/her?cosec=2&&cmd=search&type=column&event_type=ch&event_region=all&event_coordsys=helioprojective&x1=-5000&x2=5000&y1=-5000&y2=5000&result_limit=120&event_starttime="
        url2<-startDate;
        url3<-"T00:00:00&event_endtime="
        url4<-endDate;
        url5<-"T00:00:00&sparam0=ch.area_atdiskcenter&op0=%3E&value0=608735000"
        
        print(startDate)
        
        #concate link string
        url<-sprintf("%s%s%s%s%s",url1, url2, url3, url4, url5)
        print(url)
        
        #get data from website
        print("1. retrieving data..........")
        json_data <- fromJSON(paste(readLines(url), collapse=""))
        
        if(length(json_data$result)>0){  #check whether data exists
            print("2. creating table..........")
            temp<-createTable(json_data, startDate)
            temp<-temp[-1,]
            temp2<-data.frame(temp)
            
            cat("3. creating file..........\n\n")
            fileName<-sprintf("%s.csv",dateFilename)
            write.csv(temp2, file=fileName, row.names = FALSE)
        }
       
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        cat(time.taken)
        cat("\n\n")
        
    }
}


