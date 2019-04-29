library(XML)
library(RCurl)
library(tidyverse)

# Collecting players' data from basketball-reference
url <- paste0('https://www.basketball-reference.com/leagues/NBA_2004_totals.html')
data <- getURL(url)
data <- readHTMLTable(data,stringsAsFactors = FALSE)$totals_stats%>%
  filter(Player!="Salary")%>%
  filter(Player!="Player")
data[,c(-2,-3,-5)] <- apply(data[,c(-2,-3,-5)],2,as.numeric)
data <- cbind(data,rep(2004,nrow(data)))
colnames(data)[ncol(data)] <- c("Year")

for (year in 2005:2019) {
  url <- paste0('https://www.basketball-reference.com/leagues/NBA_',year,'_totals.html')
  data1 <- getURL(url)
  data1 <- readHTMLTable(data1,stringsAsFactors = FALSE)$totals_stats%>%
    filter(Player!="Salary")%>%
    filter(Player!="Player")
  data1[,c(-2,-3,-5)] <- apply(data1[,c(-2,-3,-5)],2,as.numeric)
  data1 <- cbind(data1,rep(year,nrow(data1)))
  colnames(data1)[ncol(data1)] <- c("Year")
  data <- data%>%
    bind_rows(data1)
}

# Binding the previous players' data to biographical data
bio <- read.csv("../data/raw/player_data.csv",as.is = TRUE)
convert_height <- function(df)
{
  result <- strsplit(df,"-")[[1]]
  result <- as.numeric(result[1])*30.48+as.numeric(result[2])*2.54
  return(result)
}
h <- sapply(bio$height,convert_height)
bio$height <- h

Players_bio <- data%>%
  left_join(bio,by=c("Player"="name"))

save(Players_bio,file="../data/tidy/Players_bio.RData")