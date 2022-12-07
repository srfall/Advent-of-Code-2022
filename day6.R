library(here)
library(tidyverse)
data <- readLines(here("day6.txt"))
stream<-str_extract_all(data, "", simplify = FALSE)[[1]]

markerLength = 4
checkStart <- function(chars){
  return(length(unique(chars)) == length(chars)) 
}

for (i in 1:length(stream)) {
  found = checkStart(stream[c(i:(i+markerLength-1))])
  if (found) { cat(i+markerLength-1); break}
}


##########
markerLength = 14
for (i in 1:length(stream)) {
  found = checkStart(stream[c(i:(i+markerLength-1))])
  if (found) { cat(i+markerLength-1); break}
}