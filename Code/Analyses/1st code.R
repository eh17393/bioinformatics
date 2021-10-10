##Create a vector of 100 random numbers between 0 and 50, n=100, min=0, max=50
a <- c(runif(n=100, min=0, max=50))

##Sort these by order of their value (from largest to smallest)
sorted_vec <- sort(a, decreasing=TRUE)

##look at sorted_vec
sorted_vec

##Write a function which calculates the logarithm (base 10) of a vector it is given
##subtracts this from the original vector
##and returns the new vector of values
new_function <- function(x){
  ##use the existing functions `log10()` which calculates log of a vector
  ## subtract this from the original vector
  new_vec <- x-(log10(x))
  ## the `return()` - see below
  return(new_vec)
}

##function output from sorted_vec saved as new object
b <- new_function(sorted_vec)

##look at b
b

##create standard error function 
se <- function(x){
  ##we will use the existing functions `sd()` which calculates standard deviation
  ##`sqrt()` which calculates the squareroot of numeric values
  ## and `length()` which we know from earlier gives us the length of the vector (i.e. number of data points)
  std_er<-sd(x)/sqrt(length(x))
  ## the `return()` - see below
  return(std_er)
}

##calculate the mean, sd and se of the new object
my_vec <- c(mean(b), sd(b), se(b))
my_vec

##save mean, sd and se into a single vector where each of the objects are named
named_vec <- c("x" = mean(b), "y" = sd(b), "z" = se(b))
named_vec

##assign names to the vector using the names() function
names(named_vec) <- c("mean", "sd", "se")
named_vec

##create a sequence of numbers from 15 to 100
f <- runif(n=10, min=15, max=100)

##calculate mean of the numbers in this vector which are greater than 20 and less than 60
mean(f[f > 20 & f < 60])

##calculate the sum of the numbers in this vector which are greater than 48
sum(f[f > 48])

##create function which returns the min and max values of a vector
mm <- function(x){
  ##sort vector from smallest to largest
  ##retrieve first and last value in vector
  sort(x)[c(1,length(x))]
}

##test mm function
mm(f)
