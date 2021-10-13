c(1, 4, 2, 1, 7)
c(3, 2)
##combine two vectors together 
c(c(1, 4, 2, 1, 7), (3, 2))

##where the first argument is the data to be included in the matrix. 
##Here we are using the seq() function which generates regular sequences (see ?seq() for more information)
##and the second and third arguments specify the shape of the matrix (number of columns, and number of rows). 
matrix(seq(from = 1, to = 6, by = 1), ncol = 3, nrow = 2)

##creating a blank matrix full of NAs
matrix(NA, ncol = 3, nrow = 4)

##you can also shape a vector into a matrix using the dim() function:
b <- seq(from = 1, to = 6, by = 1)
##set the dimensions of the matrix (nrows and then ncolumns)
dim(b) <- c(2, 3)
##look at the matrix
b

?names()

##the dimensions of the matrix - returned as the number of rows in the matrix first, and the number of columns second.
##here we make a matrix and look at the dimensions
dim(matrix(1:6, ncol = 3, nrow = 2))

?rnorm()


##produce 80 random numbers with normal distribution in matrix 
c <- matrix(rnorm(80, mean = 12, sd = 2), ncol = 2, nrow = 4)

##specify object by using its position in vector
seq(from = 1, to = 10, by = 1)[5]

##make a matrix and look at it
our_matrix <- matrix(6:1, ncol = 3, nrow = 2)
our_matrix
##return the value of the matrix in the first row and the second column
our_matrix[1, 2]
##return the values in the first row from the first and second column:
our_matrix[1,c(1,2)]
##return the values in the first row
our_matrix[1,]

## a matrix of values:
init_mat <- matrix(1:6, ncol=3, nrow=2)
##add 1 to all of the values
init_mat+1


##Make a matrix. Here we are using rep() to replicate the numbers 1, 2, and 3 five times. 
##Trying running: rep(1:3, each = 5)
rep(1:3, each = 5)
##We are then filling our matrix "byrow =TRUE" - which you can see makes a matrix 
mat_rep <- matrix(rep(1:3, each = 5), nrow = 3, ncol = 5, byrow = TRUE)
mat_rep

## a vector of the numbers 1 to 5
vec_seq <- 1:5
vec_seq

##multiple the matrices using element-wise multiplication
mat_rep * vec_seq


##where the first argument is the data to be included in the matrix, and the second and third arguments specify the shape of the matrix. 
mat_seq <- matrix(seq(from = 1, to = 20, length.out = 6), ncol = 3, nrow = 2)
mat_seq
##a vector to multiply by
vec_seq <- seq(from = 10, to = 4, length.out = 3)
vec_seq
##multiply the matrices using element-wise multiplication
mat_seq %*% vec_seq


##make a matrix 
mat_seq <- matrix(seq(1, 20, length.out = 6), ncol=3, nrow=2)
mat_seq
## display the logical operator of this matrix for values greater than 10
mat_seq > 10
##return the values which are greater than 10
mat_seq[mat_seq > 10]


this_is_an_array <- array(1:24, dim=c(3,4,2))
this_is_an_array

array(1:24, dim=c(3,2,4))

still_an_array <- array(1:24, dim = c(3,2,2,4))
dim(still_an_array)
still_an_array

##array containing 100 random numbers drawn from a uniform distribution with at least 3 groups
array(runif(100, min = 5, max = 30), dim = c(4, 3, 3))


##make a data frame with information on whether a Species was seen (1 = yes, 0 = no), on a particular Day:
our_data <- data.frame("Day" = rep(1:3, each = 3), 
                       "Species" = rep(letters[1:3], each = 3),
                       "Seen" = rbinom(n = 9, size = 1, prob = 0.5))

##look at the Day column
our_data["Day"]

##access the Day column
our_data$Day

##add a new column called location if given single value will repeat for every row of the column automatically
our_data$location <- "United Kingdom"
our_data

## some simple data
simple_data  <- data.frame( "a" = runif(10, 0, 1), 
                            "b" =  rnorm(10, 3, 5))
simple_data
## example calculations
simple_data$calc <- (simple_data$a * simple_data$b) - simple_data$b
simple_data

##create data frame
my_data <- data.frame("Name" = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
                      "Score" = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
                      "Questions" = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
                      "Qualify" = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes'))
##display structure of data frame
str(my_data)
##Add a column to this data frame which is the mean score per question
my_data$mean_score <- my_data$Score/my_data$Questions
my_data



##make a numeric matrix
num_mat <- matrix(rep(1:3, each = 5), 
                  nrow = 3, 
                  ncol = 5, 
                  byrow = TRUE)
num_mat
##and a vector of letters
let_vec <- LETTERS[4:16]
let_vec
##and a data.frame of species information:
species_dat <- data.frame("Species" = c("a", "b"), 
                          "Observed" = c(TRUE, FALSE))
species_dat
##save them into a list using the list() function
our_list <- list(num_mat, 
                 let_vec,
                 species_dat, 
                 5)
##view the list
our_list

##extract the first object
our_list[[1]]

##extract the first row of the 3rd object in the list:
our_list[[3]][1,]

##save them into a list using the list() function with names
our_list <- list("numbers_vec" = num_mat, 
                 "letters" = let_vec,
                 "spp_pres" = species_dat, 
                 "number" = 5)
our_list
##display the names of the objects:
names(our_list)
##view the list
our_list$spp_pres

##make a new list with "data" split into two different sites - site 1 and site 2:
our_second_list <- list("site_1" = our_list,
                        "site_2" = our_list)

##display the list
our_second_list

##letters in site 1
our_second_list$site_1$letters



##use the install packages function to install the package "devtools" which we will use in a moment. dependencies = TRUE tells R to install any other packages that devtools relies on which you haven't already installed (I would always suggest keeping dependencies = TRUE when you are installing packages)
install.packages("devtools", dependencies = TRUE)
##load the devtools package
library("devtools")
##install the "vroom" package
##The arguement for install_github takes the username and repository name where the package resides 
##if we look at the vroom url: https://github.com/r-lib/vroom
##you can see that we just use the bit after github.com/:
install_github("r-lib/vroom")
## tell R to use the "vroom()" function from the vroom package (see below)
vroom::vroom()

##read in the wader data set
wad_dat <- vroom("wader_data.csv")

##look at the top of the data
head(wad_dat)

##read in the covid data set from workshop 3
covid_dat <- vroom("Bioinformatics_data-master/Workshop 3/time_series_covid19_deaths_global.csv")
##look at the top of the data
head(covid_dat)


##you can ignore this code for the moment if you want
##but to briefly summarise it is reading in some data included in base R
##and then splitting it into 3 differnt data.frame style objects based on the values in one of the columns ("cyl")
mt <- tibble::rownames_to_column(mtcars, "model")
purrr::iwalk(
  split(mt, mt$cyl),
  ##save this split files in to the default directory
  ~ vroom_write(.x, glue::glue("mtcars_{.y}.csv"), "\t")
)
##load in some RData
load("my_data/pathway/my_data.RData")



##write out a .csv file
vroom_write(my_data, "wader_data.csv")


##load the tidyverse
library(tidyverse)


##what class is the object
class(covid_dat)
##look at the data
covid_dat
##change the first two names of our data frame
names(covid_dat)[1:2] <- c("Province.State", "Country.Region")
covid_dat


##so this says take our data frame called covid_dat
covid_long <- covid_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c( Province.State, 
                          Country.Region, 
                          Lat, 
                          Long))
##cols = to specify the columns we want pivot_longer to use to pivot the data around.
covid_long

##our data frame
covid_long <- covid_dat %>%
  ##and then apply this function 
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")
covid_long
##change long to wide
covid_long %>% 
  pivot_wider(names_from = Date,
              values_from = Deaths)



