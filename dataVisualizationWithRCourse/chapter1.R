# Load the robustbase package
library(robustbase)

# Set up the side-by-side plot array
par(mfrow = c(1, 2))

# First plot: brain vs. body in its original form
plot(Animals2$body, Animals2$brain)

# Add the first title
title("Original representation")

# Second plot: log-log plot of brain vs. body
plot(Animals2$body, Animals2$brain,
     log = "xy")

# Add the second title
title("Log-log plot")

?data()
data(package="insuranceData")
library(insuranceData)
library(MASS)
data(dataCar)

index16 <- which(ChickWeight$Time == 16)
weights <- ChickWeight$weight[index16]
truehist(weights)


# Intro to par function
# Assign the return value from the par() function to plot_pars
plot_pars <- par()

# Display the names of the par() function's list elements
names(plot_pars)

# Display the number of par() function list elements
length(par())



# Compute max_hp
max_hp <- max(Cars93$Horsepower, mtcars$hp)

# Compute max_mpg
max_mpg <- max(Cars93$MPG.city, Cars93$MPG.highway,
               mtcars$mpg)

# Create plot with type = "n"               
plot(Cars93$Horsepower, Cars93$MPG.city,
     type = "n", xlim = c(0, max_hp),
     ylim = c(0, max_mpg), xlab = "Horsepower",
     ylab = "Miles per gallon")

# Add open circles to plot
points(mtcars$hp, mtcars$mpg, pch = 1)

# Add solid squares to plot
points(Cars93$Horsepower, Cars93$MPG.city,
       pch = 15)

# Add open triangles to plot
points(Cars93$Horsepower, Cars93$MPG.highway,
       pch = 6)

x <- read.csv(url(www.users.miamioh.edu/hughesmr/sta333/baseballsalaries.txt))

library(repmis)
?repmis
x <- read.table("http://www.users.miamioh.edu/hughesmr/sta333/baseballsalaries.txt")


library(repmis)
site = "http://www.users.miamioh.edu/hughesmr/sta333/baseballsalaries.txt"
download.file(site,destfile= "./baseballsalaries.txt")
list.files()
#baseball<-read.table(file.choose(),header=TRUE) 
baseball<-read.table("baseballsalaries.txt",header=TRUE) 
head(baseball)
dim(baseball)
names(baseball)
?git_stamp


git_stamp(repo = getwd())

?InstallOldPackages


# Install old versions of the e1071 and gtools packages.
Names <- c("car")
Vers <- c("2.1-3")
InstallOldPackages(pkgs = Names, versions = Vers)


