# base 10 to base 7

base10to7 <- function(x){
  
  # number will hold the base7 number
  # power of 10 is used to calculate the base7 number
  number <- 0
  power <- 0
  
  while ((x %/% 7) != 0){
    number <- number + (x %% 7) * (10)^power
    power <- power + 1
    x <- x %/% 7
  }
  
  number <- number + (x %% 7) * (10)^power
  
  return(number)
  
}

base10to7(100)
base10to7(8)
base10to7(7)


p7 <- function(x){
  # calculate how many numbers are required
  base10s <- seq(0, x-1, by=1)
  
  # call base10to7 on each element of the sequence vector
  sapply(base10s, base10to7)
}

p7(10)
p7(15)
p7(52)

base7to10 <- function(x){
  
  # Get the quotient and remainder
  number <- 0
  power <- 0
  
  while((x %/% 10) != 0){
    number <- number + (x %% 10) * (7^power)
    power <- power + 1
    x <- x %/% 10
  }
  
  number <- number + (x %% 10) * (7^power)
  return(number)
  
}

base7to10(202)
base7to10(14)
base7to10(102)

# From base 10 to any base
base10tok <- function(x, k=2){
  
  number <- 0
  power <- 0
  
  while( (x %/% k) != 0 ){
    number <- number + (x %% k) * (10^power)
    power <- power + 1
    x <- x %/% k
  }
  
  number <- number + (x %% k) * (10^power)
  return(number)
  
}

base10tok(100, k=7)
base10to7(100)
base10tok(140, k=8)
base10tok(110, k=16)

# Generate x-number of base k numbers
pk <- function(x, k=2){
  # calculate how many numbers are required
  base10s <- seq(0, x-1, by=1)
  
  # call base10to7 on each element of the sequence vector
  sapply(base10s, base10tok, k)
}

pk(10, 7)
pk(10, 3)
pk(10, 10)
pk(50, 5)
pk(50)

# From any base to base 10
basekto10 <- function(x, k=2){
  
  number <- 0
  power <- 0
  
  while( (x %/% k) != 0 ){
    number <- number + (x %% k) * (k^power)
    power <- power + 1
    x <- x %/% k
  }
  
  number <- number + (x %% k) * (k^power)
  return(number)
  
}


