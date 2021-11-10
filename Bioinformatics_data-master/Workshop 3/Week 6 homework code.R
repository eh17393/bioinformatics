library(tidyverse)
library(vroom)
library(usethis)
library(devtools)
library(lubridate)
library(multcomp)


##read in the tokyo 2021 olympic medal data from github
medal_data <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%205/Tokyo%202021%20medals.csv")
medal_data

##add in position number
medal_data$Position <- 1:nrow(medal_data)

##add GDP column for all countries 
library(wbstats)

##extract the GDP data per capita for all countries for 2020 (no 2021 data) 
GDP_data <- wb_data(indicator = "NY.GDP.PCAP.CD", 
                    start_date = 2020,
                    end_date = 2020)
GDP_data

##convert it to a tibble
GDP_data <- as_tibble(GDP_data)
GDP_data  

##ensure all countries are identified as the correct country before merging with world data 
library(countrycode)  
  ##add a column to medal data containing the country code
medal_data$code <- countrycode(medal_data$Country, 
                               origin = "country.name",
                               destination = "iso3c")
medal_data
##look at head of new dataset
head(medal_data, 3)

##correct china
medal_data$code[2] <- "CHN"
##look at head of new dataset
head(medal_data, 3)

##compare that to the values in the WB data
GDP_data %>% filter(iso3c == "USA")
 ##works fine 


##extract GDP data for all countries for 2020 (no 2021 data)
all_GDP_data <- wb_data(indicator = "NY.GDP.MKTP.CD",
                        start_date = 2020,
                        end_date = 2020)
all_GDP_data

##rename all_GDP_data column 5 to GDP
names(all_GDP_data)[5] <- "GDP"
all_GDP_data
##select just iso3c and GDP column
##use dplyr::select as conflict with MASS package
head(all_GDP_data %>% dplyr::select( iso3c, GDP ))
##join two data sets 
medal_GDP <- left_join(medal_data, 
                         all_GDP_data %>% dplyr::select(iso3c, GDP),
                         by = c("code" = "iso3c"))
##visualise new data set:
medal_GDP

##drop rows with NA values
medal_GDP <- medal_GDP %>% drop_na()

##visualise the data
ggplot(medal_GDP, aes(x=GDP, y=Position)) + 
  geom_point() + 
  theme_bw()

##look at a plot where the data are logged
ggplot(medal_GDP, aes(x=GDP, y=Position)) + 
  geom_point() + 
  scale_y_continuous(trans='log10') + 
  scale_x_continuous(trans='log10') + 
  theme_bw() +
  ggtitle("Logged data")
 
##plot ranking against country GDP
p1 <- ggplot(medal_GDP, aes(x = GDP,
                             y = Position)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Ranking") +
  xlab("GDP ($)")
##add the loess smoothing:
p1 <- p1 + geom_smooth(method="loess")
## add a title
p1 <- p1 + ggtitle("Country GDP versus Ranking at Tokyo 2021")
p1


##fit a model 
mod_position_1 <- glm(Position ~ GDP,
                  ##specify the data
                  data = medal_GDP,
                  )
plot(mod_position_1)

##fit a model where both x and y are logged 
mod_position_2 <- glm(log10(Position) ~ log10(GDP), data=medal_GDP)
plot(mod_position_2)

##summarise model 2
summary(mod_position_2)

##display class of model object
class(mod_gold_1)
##plot diagnostic plots
plot(mod_gold_1)
##summarise the model outputs 
summary(mod_gold_1)
  ##significant impact of GDP on gold medals won: higher GDP correlates with higher gold medal count


## fit a glm with a poisson distribution
mod_gold_2 <- glm(Gold ~ GDP,
                  data = medal_GDP,
                  family = "poisson")
plot(mod_gold_2)
  ##not good fit normal Q-Q plot very ill-fitting
  ##points are outside 1 lines on residuals vs leverage 




##load the iris data set
data("iris")

##visualize the data
iris1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(col = Species)) +
  theme_bw()
iris1
## add in some simple linear regessions to visualise possible effects
##specifying col = "Species" means you will plot a differnt LM for each species (as colour is grouped)
iris1 + geom_smooth(aes(col = Species), method="lm")

##initially fit with gaussian model as data non-integer 
mod_petal_1 <- glm(Petal.Width ~ Petal.Length*Species, data = iris)
##set a 2x2 plot area, so we get a single pannel with 4 plots:
par(mfrow = c(2, 2))
plot(mod_petal_1)

##trying different data transformations:
##mod with log
mod_petal_2 <- glm(log(Petal.Width) ~ Petal.Length*Species, data = iris)
par(mfrow = c(2, 2))
plot(mod_petal_2)
  ##made it much worse
##mod with square root
mod_petal_3 <- glm(sqrt(Petal.Width) ~ Petal.Length*Species, data = iris)
plot(mod_petal_3)
  ##looks much better 
##look at summary of mod 3
summary(mod_petal_3)











































































##plot the sepal.width data by species 
ggplot( iris, aes( x = Species, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  theme_bw()

##check if data normally distributed and overlapping
ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  ## bin width determines how course the histogram is
  ## the alpha determines the transparency of the bars
  ## position allows you to determine what kind of histogram you plot (e.g. stacked vs overlapping). try changing to position="stack"
  geom_histogram(binwidth = .1, alpha = .5, position="identity")

##fit a glm()
##glm(first term is y/response ~ x/predictor)
mod_iris <- glm(Sepal.Width ~ Species,
                ##specify the data
                data = iris,
                ##specify the error structure
                family = "gaussian")
mod_iris

##display the class of the model object
class(mod_iris)
##display the class of the model object to determine if any values need to be removed etc.
plot(mod_iris)


##summarise the model outputs
summary(mod_iris)

##plot the sepal.width data by sepal.length and species 
ggplot( iris, aes( x = Petal.Length, y = Sepal.Width)) +
  geom_jitter(aes(col = Species)) +
  theme_bw()

##fit a glm()
##glm(first term is y/response ~ x/predictor)
mod_petal <- glm(Sepal.Width ~ Petal.Length*Species,
                ##specify the data
                data = iris,
                ##specify the error structure
                family = "gaussian")
mod_petal
##plot model
plot(mod_petal)
##summarise model outputs
summary(mod_petal)


##transform response variable Sepal.Width with log10()
log_sepal_width <- log(iris$Sepal.Width)
##add log_sepal_width column to dataframe
iris <- iris %>% mutate(log_sepal_width)

##plot the log_sepal.width data by sepal.length and species 
ggplot( iris, aes( x = Petal.Length, y = log_sepal_width)) +
  geom_jitter(aes(col = Species)) +
  theme_bw()

##fit a glm()
mod_log <- glm(log_sepal_width ~ Petal.Length*Species,
                 data = iris,
                 family = "gaussian")
mod_log
##plot model
plot(mod_log)
##summarise model outputs
summary(mod_log)
  ##more significant than previous glm showing versicolor and and virginica affected by sepal width however no overall effect of petal.length 






