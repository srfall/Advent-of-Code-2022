library(here)
library(tidyverse)
data <- read.table(here("day4.txt"),sep = ",")
df<-data.frame(data,contains=NA)


for (i in 1:nrow(df)) {

  split<-str_split(df[i,]$V1,"-")
  seq1<-split[[1]][1]:split[[1]][2]
  
  split<-str_split(df[i,]$V2,"-")
  seq2<-split[[1]][1]:split[[1]][2]
  
  if (all(seq1 %in% seq2) || all(seq2 %in% seq1)){
    df[i,]$contains <- TRUE
  } else {
    df[i,]$contains <- FALSE
  }
}
sum(df$contains)


#########################

df<-data.frame(data,contains=NA)


for (i in 1:nrow(df)) {
  
  split<-str_split(df[i,]$V1,"-")
  seq1<-split[[1]][1]:split[[1]][2]
  
  split<-str_split(df[i,]$V2,"-")
  seq2<-split[[1]][1]:split[[1]][2]
  
  if (any(seq1 %in% seq2) || any(seq2 %in% seq1)){
    df[i,]$contains <- TRUE
  } else {
    df[i,]$contains <- FALSE
  }
}
sum(df$contains)