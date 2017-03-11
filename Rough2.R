#Question 1:

# This function returns the first x number of base7 numbers.
p7 <- function(x){
  # calculate how many numbers are required
  base10s <- seq(0, x-1, by=1)
  
  # call base10to7 on each element of the sequence vector
  sapply(base10s, base10to7)
}

# Generate first 10 base 7 numbers
p7(10)

#Output:
#> p7(10)
#[1]  0  1  2  3  4  5  6 10 11 12

#Question 2:

# Function to convert a base 10 number to base 7 number
base10to7 <- function(x){
  
  # number will hold the base7 number
  # power of 10 is used to calculate the base7 number
  number <- 0
  power <- 0
  
  # As long as quotient is non-zero, get the remainder and build the base-7 number.
  while ((x %/% 7) != 0){
    number <- number + (x %% 7) * (10)^power
    power <- power + 1
    x <- x %/% 7
  }
  
  number <- number + (x %% 7) * (10)^power
  
  return(number)
  
}

# Convert 100 from base 10 to base 7
base10to7(100)


#Output:
#> base10to7(100)
#[1] 202
#> base10to7(8)
#[1] 11
#> base10to7(7)
#[1] 10


#Question 3:

# Function to convert a base 7 number to base 10 number
base7to10 <- function(x){
  
  # number will hold the base10 number
  # power of 7 is used to calculate the base10 number
  number <- 0
  power <- 0
  
  # As long as quotient is non-zero, get the remainder and build the base-10 number.
  while((x %/% 10) != 0){
    number <- number + (x %% 10) * (7^power)
    power <- power + 1
    x <- x %/% 10
  }
  
  number <- number + (x %% 10) * (7^power)
  return(number)
  
}

# Convert 202 from base 7 to base 10
base7to10(202)

#Output:
#> base7to10(202)
#[1] 100
#> base7to10(14)
#[1] 11
#> base7to10(102)
#[1] 51


#Question 4:
#Answer: Yes all the 3 functions can be generalized to take any base k. This can be accomplished by having a second argument which will represent the base. Default is k=2 (binary).

# Generate the first x-number of base k numbers
pk <- function(x, k=2){
  # calculate how many numbers are required
  base10s <- seq(0, x-1, by=1)
  
  # call base10tok on each element of the sequence vector
  sapply(base10s, base10tok, k)
}

# Generate first 10 base 7 numbers
pk(10, 7)
# Generate first 10 base 5 numbers
pk(10, 5)
# Generate first 10 base 3 numbers
pk(10, 3)

#> pk(10, 7)
#[1]  0  1  2  3  4  5  6 10 11 12
#> pk(10, 5)
# [1]  0  1  2  3  4 10 11 12 13 14
#> pk(10, 3)
# [1]   0   1   2  10  11  12  20  21  22 100


# From base 10 to any base k
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

# Convert 100 from base 10 to base k
base10tok(100, k=7)

# Convert 140 from base 10 to base k
base10tok(140, k=8)

#> base10tok(100, k=7)
#[1] 202

#> base10tok(140, k=8)
#[1] 214


# From any base k to base 10
basekto10 <- function(x, k=2){
  
  number <- 0
  power <- 0
  
  while( (x %/% 10) != 0 ){
    number <- number + (x %% 10) * (k^power)
    power <- power + 1
    x <- x %/% 10
  }
  
  number <- number + (x %% 10) * (k^power)
  return(number)
  
}

# Convert 202 from base k to base 10
basekto10(202, k=7)

#> basekto10(202, k=7)
#[1] 100