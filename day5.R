library(here)
library(tidyverse)
data <- readLines(here("day5.txt"),n=9)
df<-setNames(vector("list", 9),as.character(1:9))
names(df)<-as.character(1:9)
for (i in as.character(1:9)) {
  string<-c()
  for (j in 1:(length(data)-1)) {
    string <-c(string, str_sub(data[j],str_locate(data[9],i)))
    string <-string[!string %in% " "]
  }
  df[[i]]<-rev(string)
}

instructions <-read.table(here("day5.txt"),skip=10, sep=" ")

for (i in 1:nrow(instructions)) {
  for (num in 1:instructions[i,]$V2) {
    from<-as.character(instructions[i,]$V4)
    to<-as.character(instructions[i,]$V6)

    df[[to]] <- c(df[[to]], last(df[[from]]) )
    df[[from]] <- df[[from]][-length(df[[from]])]
  }
}
out<-c()
for (i in 1:length(df)) {
  out<-c(out, last(df[[i]]))
}
cat(out)

#########################
data <- readLines(here("day5.txt"),n=9)
df<-setNames(vector("list", 9),as.character(1:9))
names(df)<-as.character(1:9)
for (i in as.character(1:9)) {
  string<-c()
  for (j in 1:(length(data)-1)) {
    string <-c(string, str_sub(data[j],str_locate(data[9],i)))
    string <-string[!string %in% " "]
  }
  df[[i]]<-rev(string)
}

instructions <-read.table(here("day5.txt"),skip=10, sep=" ")

for (i in 1:nrow(instructions)) {
  from<-as.character(instructions[i,]$V4)
  to<-as.character(instructions[i,]$V6)

  index<-(length(df[[from]])-(instructions[i,]$V2-1)):length(df[[from]])

  df[[to]] <- c(df[[to]], df[[from]][index]  )
  df[[from]] <- df[[from]][-index]
}
out<-c()
for (i in 1:length(df)) {
  out<-c(out, last(df[[i]]))
}
cat(out)
