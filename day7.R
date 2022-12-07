library(here)
library(tidyverse)
data <- readLines(here("day7.txt"))
data <- read.table(here("day7.txt"),sep = " ",fill = T, dec = ",", colClasses = "character")
userInput<-data
vfs<- data.frame(data,"path"=rep("NA",nrow(data)))
path<-c()

# processInput<-function(userInput){
  for (i in 1:nrow(userInput)) {
  
    if (userInput[i,]$V1 == "$"){
      if(userInput[i,]$V2 == "cd"){
        if(userInput[i,]$V3 == "/"){
          path<-c("~")
        } 
        else if(userInput[i,]$V3 == ".."){
          path<-path[-length(path)]
        } 
        else{
          path<-c(path, userInput[i,]$V3)
        } 
      }
      if(!is.null(path)){
        vfs[i,"path"]<-paste(path, collapse = "/")   
      }
    }
    # if (userInput[i,]$V1 == "dir"){
    #   
    #   
    # }
    if (!is.na(as.numeric(userInput[i,]$V1))){
      if(!is.null(path)){
        filename<-c(path,userInput[i,]$V2)
        vfs[i,"path"]<-paste(filename, collapse = "/")   
      }
    }
  }

res<-vfs[vfs$path != "NA",]
# res<-res[1:40,]
res$dirsize<-NA
for (i in 1:nrow(res)) {
  matches<- str_detect(res$path, res[i,"path"])
  if(all(!matches)){next}
  sumFileSize<-res[matches,]$V1
  sumFileSize<-sum(as.numeric(sumFileSize[sumFileSize != "$" & sumFileSize != "dir"]))
  res[i,"dirsize"]<-sumFileSize
}
out<-res[res$V2=="cd",]
out<-unique(out[,c("path","dirsize")])
out<-sum(out$dirsize[out$dirsize<=100000])
out
# doesn't work lol

