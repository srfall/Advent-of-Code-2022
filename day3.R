library(here)
library(tidyverse)
data <- read.table(here("day3.txt"),sep = "")
df<-data.frame(data,num=NA)
encrypt<-data.frame(char=c(letters,LETTERS),val=1:52)
getNumber<-function(char){
  return(encrypt[encrypt$char==char,]$val)
}
left<-vector("list", nrow(data))
right<-vector("list", nrow(data))


for (i in 1:nrow(df)) {
  nu<-c()
  for (j in 1:nchar(df[i,]$V1)) {
    nu<-c(nu,getNumber(substr(df[i,]$V1, j, j)))
  }
  l <- length(nu)/2
  left[[i]]<-nu[1:l]
  right[[i]]<-nu[(l+1):(l*2)]
}

for (i in 1:nrow(df)) {
  df[i,]$num<- intersect(left[[i]],right[[i]])
}
sum(df$num)


# 2
df<-data.frame(data,group=rep(1:(nrow(data)/3), each=3))
g<-vector("list", nrow(data)/3)


for (i in 1:nrow(df)) {
  nu<-c()
  for (j in 1:nchar(df[i,]$V1)) {
    nu<-c(nu,getNumber(substr(df[i,]$V1, j, j)))
  }
  g[[i]]<-nu
}

df$res<-NA
for (i in unique(df$group)) {
  index<-which(df$group==i)
  df[index,]$res <-intersect(intersect(g[[index[1]]],g[[index[2]]]),g[[index[3]]])
  
}
out<-df%>%
  group_by(group) %>%
  summarise(m=mean(res))
sum(out$m)
