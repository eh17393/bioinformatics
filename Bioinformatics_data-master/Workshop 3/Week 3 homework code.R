library(tidyverse)
library(vroom)
library(devtools)

##read in the to_sort_pop_1 data set
pop_1_dat <- vroom("Bioinformatics_data-master/Workshop 3/to_sort_pop_1.csv")
##read in the to_sort_pop_2 data set
pop_2_dat <- vroom("Bioinformatics_data-master/Workshop 3/to_sort_pop_2.csv")

?join()
##join two data frames together in full into one vector
combined_pop_data <- full_join(pop_1_dat, pop_2_dat, by = NULL, match = "all")

##ID what class data is
class(combined_pop_data)
combined_pop_data

##take data frame combined_pop_data to convert to long format
long_pop_data <- combined_pop_data %>%
  pivot_longer(cols = -c( species,
                          primary_threat,
                          secondary_threat,
                          tertiary_threat))
##look at vector
long_pop_data

##rename observation column 
long_pop_data <- combined_pop_data %>%
  pivot_longer(cols = -c( species:tertiary_threat),
               names_to = "date",
               values_to = "abundance")

##look at vector
long_pop_data

##split column 5 into population type and date 
library("tidyr")
finished_data <- long_pop_data %>%
  separate(date, 
           c("population", "date"), sep = 6)

##look at vector
finished_data


##could all be done in one block of code 
long_spp <- combined_pop_data %>%
  pivot_longer(cols = -c( species,
                          primary_threat,
                          secondary_threat,
                          tertiary_threat),
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = F,
               values_to = "abundance")
long_spp








  