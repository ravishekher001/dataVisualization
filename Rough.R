# Function to calculate base 7 of a given number

p7 <- function(n){
  print (paste("print the first ", n , " base 7 numbers"))
  
}

base10to7 <- function(x){
  
  if ((nchar(x) == 1) && (x <= 7)){
    
    a <- x %% 7
    return (a)
    
  } else if((nchar(x) == 1) && (x > 7)){
    
    b <- round(x / 7)
    a <- x %% 7
    
    return((b*10) + a)
    
  } else if((nchar(x) == 2) && (x <= (7*2))){
    
    a <- x %% 7
    b <- round(x/7)
    
  }
  
  return(0)
  
}

base10to7 <- function(x){
  
  quotient <- x %/%7
  remainder <- x %% 7
  
  while(quotient )
  
  return()
  
}


base10to7<-function(x){
  i=0
  sum=0
  while(x%/%7!=0){
    sum<-sum+((x%%7)*(10^i))
    i=i+1
    x<-x%/%7
  }
  sum<-sum+((x%%7)*(10^i))
  return(sum)
}
base10to7(100)

?%/%

6 %/% 7
6 %% 7

8%%7
10%%7
6%%7

n <- nchar(x)

0*7^0
1*7^0
2*7^0
3*7^0
4*7^0
5*7^0
6*7^0

n*7^n-1



?read_csv
library("readr")
?read_csv
gdp_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
edu_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

columnNames <- c("CountryID", "Ranking", "Economy", "MillionsOfUSDollars")
gdp <- read_csv(gdp_url, col_names = columnNames, skip=5, n_max = 232)
gdp <- read_csv(gdp_url, col_names = columnNames, col_types = "ci_cc", skip=5, n_max = 232)

edu_url <- read_csv(edu_url)

str(edu_url)
head(edu_url)
tail (edu_url)

head(gdp)
tail(gdp)
names(gdp)
class(gdp)
dim(gdp)
str(gdp)

library("dplyr")
glimpse(gdp)
glimpse(edu_url)
        
library("data.table")


# Try downloading the file locally and then use the utils/readr/gdata/data.table packages.

getwd()
ls
list.files()


read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "GDP.csv")

?read_csv
