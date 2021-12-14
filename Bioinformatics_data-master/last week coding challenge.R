help <- function(word){
    print("help")
    #your function
  }
help()

random_vector <- c(8,9,9,8,5,1,8,5,6,2,3,5,9,9,8)

sum(random_vector == 5)

counting <- function(x){
  sum(random_vector == x)
}
counting(5)
counting(9)


employee <- c('John Doe','Peter Gynn','Jolie Hope', 'Brian')
salary <- c(5, 4, 3, 5)

employ.data <- data.frame(employee, salary)
employ.data

employ.data[employ.data==5] <- NA 
employ.data

input_NA <- function(data_frame, character){
  data_frame[data_frame == character] <- NA
  return(data_frame)
}
input_NA(employ.data, 5)


##Use the commands if and else to return the even numbers only from the given vector. If you are unsure how to use if else, you can check this link
evens <- function(x){
  vector <- c()
  for (i in 1:length(x)){
    if (x[i] %% 2 == 0){ 
      vector <- c(vector, x[i]) 
    }
  }
  vector
}
evens(c(1,3,4,5,6,7))

odds_even <- c(0,2,3,5,6,8,9)

even_numbers <- function(vector){
  even <- c()
  for (i in vector) 
  if (i %% 2 == 0){
    even <- append(even, i)
  }
}

for ()
  if 


?append()

library(tidyverse)
library(vroom)
library(usethis)
library(devtools)
plant_index <- vroom("https://raw.githubusercontent.com/PolCap/Teaching/master/Bristol/R%20Course/data/LivingPlanetIndex.csv")
head(plant_index)

##Take the plant_index data 
is.na(plant_index) <- plant_index == "NULL"
plant_index_long <- plant_index %>%
  ##and then apply this function 
  pivot_longer(cols = -c(ID:System),
               names_to = "Year",
               values_to = "Abundance",
               values_drop_na = T)
plant_index_long

Uk_plant <- plant_index_long %>%
  filter(Country == "United Kingdom")
Uk_plant
##earliest date in UK
min(Uk_plant$Year)


library(lubridate)
plant_index_long$Year <- as.numeric(plant_index_long$Year)
plant_index_long

max(Uk_plant$Year)

time_series <- plant_index_long %>%
  group_by(Country, ID) %>%
  summarise(ncount = length(ID))
time_series

max(time_series$ncount)
time_series %>%
filter(ncount == 66)
