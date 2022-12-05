library(here)
library(tidyverse)
data <- readLines(here("day1.txt"))
df<-data.frame(val=NA,id=NA)
id<-1
for (i in 1:length(data)) {
  val <- as.numeric(data[i])
  if (is.na(val)){
    id<-id+1
    next
  } else {
    df[i,]<-c(val,id)
  }
}
df <- df %>%
  drop_na() %>%
  group_by(id) %>%
  summarise(sum=sum(val))
max(df$sum)
max_carry<-sum(sort(df$sum,decreasing = T)[1:3])
