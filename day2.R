library(here)
library(tidyverse)
data <- read.table(here("day2.txt"),sep = " ")
names(data)<-c("op","yu")
df <- data %>%
  mutate(op  = case_when(op == 'A' ~ 1,
                         op == 'B' ~ 2,
                         op == 'C' ~ 3),
         yu = case_when(yu == 'X' ~ 1,
                        yu == 'Y' ~ 2,
                        yu == 'Z' ~ 3))
df$res<-NA
res <- function(x,y){
  if ((x == 1 && y == 2) || (x == 2 && y == 3) || (x == 3 && y == 1))
    return(6)
  if (x == y)
    return(3)
  else
    return(0)
}

for (i in 1:nrow(df)) {
  df[i,]$res <- res(df[i,]$op,df[i,]$yu)
}
df$res<-df$res+df$yu
sum(df$res)



df <- data %>%
  mutate(op  = case_when(op == 'A' ~ 1,
                         op == 'B' ~ 2,
                         op == 'C' ~ 3),
         yu = case_when(yu == 'X' ~ 0,
                        yu == 'Y' ~ 3,
                        yu == 'Z' ~ 6))
df$res<-NA
win<-function(input){
  if(input==1)
    return(2)
  if(input==2)
    return(3)
  if(input==3)
    return(1)
}
loose<-function(input){
  if(input==1)
    return(3)
  if(input==2)
    return(1)
  if(input==3)
    return(2)
}


res <- function(x,y){
  if (y==0)
    return(loose(x))
  if (y == 3)
    return(x)
  else
    return(win(x))
}

for (i in 1:nrow(df)) {
  df[i,]$res <- res(df[i,]$op,df[i,]$yu)
}
df$res<-df$res+df$yu
sum(df$res)
