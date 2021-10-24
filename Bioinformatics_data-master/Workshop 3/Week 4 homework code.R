library(tidyverse)
library(vroom)
library(devtools)

##read in the to_sort_pop_1 data set
pop_1_dat <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%203/to_sort_pop_1.csv")
##read in the to_sort_pop_2 data set
pop_2_dat <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%203/to_sort_pop_2.csv")

##join two data frames together in full into one vector
combined_pop_data <- full_join(pop_1_dat, pop_2_dat, by = NULL, match = "all")

##coonvert data to long form, rename observation columns, split pop and date column into two and remove any NA values:
long_spp <- combined_pop_data %>%
  pivot_longer(cols = -c( species,
                          primary_threat,
                          secondary_threat,
                          tertiary_threat),
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = F,
               values_to = "abundance")
##visualise data
long_spp


##find max value of years in date column
max(long_spp$date)
 ##max date was 2019-01-01

## filter the data to include data from the year 2019 only:
pop_2019 <- long_spp %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date == "2019-01-01")
pop_2019

## filter the data to include data from the year 2009 only:
pop_2009 <- long_spp %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date == "2009-01-01")
pop_2009

## filter the data to include data from the year 1999 only:
pop_1999 <- long_spp %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date == "1999-01-01")
pop_1999
##gives insight into how threats have changed over past 20 years


##just look at the data from Schistidium helveticum:
long_spp %>% filter(species == "Schistidium helveticum")
##just look at the data from for habitat destruction:
habitat_destruction <- long_spp %>% filter(primary_threat == "Habitat destruction")


library(ggplot2)
##visualise ggplot object histogram of primary threats for 2019 spp
p1 <- ggplot(data = pop_2019) +
  geom_bar(aes(x = primary_threat, color=I("black"), fill=I("red")))
p1
##visualise ggplot object histogram of secondary threats for 2019 spp
p2 <- ggplot(data = pop_2019) +
  geom_bar(aes(x = secondary_threat, color=I("black"), fill=I("yellow")))
p2
##visualise ggplot object histogram of tertiary threats for 2019 spp
p3 <- ggplot(data = pop_2019) +
  geom_bar(aes(x = tertiary_threat, color=I("black"),fill=I("orchid")))
p3
    ##visualised the ggplot for 2009 and 1999 but were identical to the 2019 plots therefore irrelevant 
##place two plots horizontally 
library(patchwork)
library(devtools)
library(ggplot2)
##place three plots vertically 
p1 + p2 + p3 + plot_layout(ncol=1)








##stacked barplot in base r 
barplot(table(pop_2019$primary_threat))






pop_new <- gather(pop_2019, key = "threat_type", values = "threat_level", primary_threat, secondary_threat, tertiary_threat) 

?gather



barplot(df)

?barplot

df<- data.frame(pop_2019$primary_threat, pop_2019$secondary_threat, pop_2019$tertiary_threat)
df
library(Hmisc)

hist(df)
  ##messing around with the data 


##experimenting with subset strings
##return only strings that contain a patter match: primary_threat
subset_1 <- str_subset(pop_2019$primary_threat, "Habitat", negate = FALSE)
subset_1
##return only strings that contain a patter match: secondary_threat
subset_2 <- str_subset(pop_2019$secondary_threat, "Habitat", negate = FALSE)
subset_2
##return first pattern match found in each string, as a vector
str_extract(pop_2019$primary_threat, "[Habitat]")

##return first pattern match found in each string, as a vector:
subset_4 <- str_subset(pop_2019$primary_threat & pop_2019$secondary_threat, "Habitat", negate = FALSE)
  ##doesnt work



##return first pattern match in each string as matrix with column for each group in patter
match <- str_match(pop_2019$primary_threat, "(Habitat) ([^+])")
   


##just look at the data from for threats involving habitat:
habitat <- pop_2019 %>% filter(primary_threat == "Habitat destruction" | primary_threat == "Habitat loss" | primary_threat == "Habitat fragmentation")

##visualise in barplot:
library(ggplot2)
theme_set(theme_bw())
ggplot(habitat)

library(stringr)
##add primary threat to each entry in primary_threat column
pop_2019$primary_threat <- paste("'", pop_2019$primary_threat, sep = " ")

?mutate()

##show first 10 and last 10 entries in primary_threat
head(unique(pop_2019$primary_threat), 10); tail(unique(pop_2019$primary_threat), 10)

## make a new data.frame from the old long_spp data.frame
spp_threat <- long_spp %>% 
  ## we want to calculate the number of 
  ##deaths in each country and at each date:
  group_by(species, primary_threat) %>% 

spp_threat

## have a look at the data.frame that is produced:
covid_country

##count the number of matches in a string
str_count(pop_2019$primary_threat, "Habitat")













##add secondary threat to each entry in secondary_threat column
pop_2019$secondary_threat <- paste("secondary_threat", pop_2019$secondary_threat, sep = " ")
##add tertiary threat to each entry in tertiary_threat column
pop_2019$tertiary_threat <- paste("tertiary_threat", pop_2019$tertiary_threat, sep = " ")

##separate threat level and threat columns
primary_pop <- pop_2019 %>%
  separate(primary_threat, 
           c("threat_level", "threat_type"), sep = 15)
secondary_pop <- primary_pop %>%
  separate(secondary_threat, 
           c("threat_level2", "threat_type2"), sep = 17)
tertiary_pop <- secondary_pop %>%
  separate(tertiary_threat, 
           c("threat_level3", "threat_type3"), sep = 16)

##combine all threat types into one column

## filter the data to include data from the year 2019 only:
pop_recent <- long_spp %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date == "2019-01-01")
pop_recent
##create ggplot 
ggplot(pop_recent, aes(x = prim))


?str_count()

pop_2019 %>% group_by(species, primary_threat)

nrow(pop_2019$primary_threat)

DF <- pop_2019(group = c("pop_1", "pop_2"),
                 primary_threat = c(subset_1),
                 secondary_threat = c(subset_2))
DFtall <- DF %>% gather(key = threat, value = Value, primary_threat:secondary_threat)
DFtall






combined_pop_data










