##example of histogram 
hist(rnorm(500, mean = 161.6, sd = 8.8))


library(tidyverse)
library(vroom)
library(usethis)
library(devtools)
library(multcomp)
library(lubridate)


##load the iris data set
data("iris")
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


## load the multcomp pack
library(multcomp)

## run the multiple comparisons, and look at the summary output:
summary(glht(mod_iris, mcp(Species="Tukey")))


##read in the to_sort_pop_1 data set
pop_1_dat <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%203/to_sort_pop_1.csv")
##read in the to_sort_pop_2 data set
pop_2_dat <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%203/to_sort_pop_2.csv")
##join two data frames together in full into one vector
combined_pop_data <- full_join(pop_1_dat, pop_2_dat, by = NULL, match = "all")

##convert data to long form, rename observation columns, split pop and date column into two and remove any NA values:
long_spp <- combined_pop_data %>%
  pivot_longer(cols = -c( species,
                          primary_threat,
                          secondary_threat,
                          tertiary_threat),
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = T,
               values_to = "abundance")
long_spp
print(long_spp)

library(lubridate)
##set the date column to date format 
long_spp$date.corrected <- as_date(long_spp$date)

##filter the data to include data for Trichocolea tomentella only 
single_spp <- long_spp %>% 
  filter(species == "Trichocolea tomentella")
single_spp

##make the plot
p1 <- ggplot(single_spp, aes(x=date.corrected, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("Year")
##add the loess smoothing:
p1 + geom_smooth(method="loess")


## calculate a new column (`standardised_time`) which is the difference between the
## starting date of the time series and each other date in weeks (see ?difftime)
## we will set this to a numeric vector
single_spp <- single_spp %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(date.corrected),
                                                 min(as.Date(date.corrected)),
                                                 units = "weeks")))

print(single_spp[,c("abundance", "date.corrected", "standardised_time")], 30)

##fit a glm()
mod1 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "gaussian")
mod1
##return the predicted (response) values from the model
##and add them to the single species tibble:
single_spp$pred_gaussian <- predict(mod1,
                                    type="response")

?resid()
##return the residual values from the model 
##and add them to the single species tibble:
single_spp$resid_gaussian <- resid(mod1)


## plot the abundances through time
p2 <- ggplot(single_spp, aes(x = standardised_time,
                             y = abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")
p2
##add in a line of the predicted values from the model
p2 <- p2 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian),
                     col = "dodgerblue",
                     size = 1)
p2
## we can also add in vertical blue lines which show the redsidual error of the model
## (how far the observed points are from the predicted values).
## in geom_segement we specify where we want the start and end of the segments (lines)
## to be. Without any prompting ggplot assumes that we want the start of the lines to
## be taken from the x and y values we are plotting using the ggplot() function
## i.e. standardised_time and abundance, so we just need to specify the end points of
## the lines:
p2 <- p2 +
  geom_segment(aes(xend = standardised_time,
                   yend = pred_gaussian),
               col="lightblue")
p2
## add a title
p2 <- p2 + ggtitle("Fitted model (gaussian with identity link)")
##print the plot
p2

##plot a histogram of the residuals from the model using geom_histogram()
p3 <- ggplot(single_spp, aes(x = resid_gaussian)) +
  geom_histogram(fill="goldenrod") +
  theme_minimal() +
  ggtitle("Histogram of residuals (gaussian with identity link)")
## print the plot
p3

##plot the residuals versus the predicted values 
p4 <- ggplot(single_spp, aes(x = pred_gaussian,
                             y = resid_gaussian)) +
  geom_point() +
  theme_minimal() +
  xlab("Predicted values") +
  ylab("residuals") +
  ggtitle("Predicted vs residual (gaussian with identity link)") +
  ##using geom_smooth without specifying the method (see later) means geom_smooth()
  ##will try a smoothing function with a formula y~x and will try to use a loess smoothing
  ##or a GAM (generalised additive model) smoothing depending on the number of data points
  geom_smooth(fill="lightblue", col="dodgerblue")

##print it
p4


##plot the qq plot for the residuals from the model assuming a normal distribution,
## and add the straight line the points should fall along:
qqnorm(single_spp$resid_gaussian); qqline(single_spp$resid_gaussian)


## fit a glm with a poisson distribution
mod2 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "poisson")
mod2
## fit a glm with a gaussian distribution with a log link
mod3 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "log"))
## we could also try a guassian model with an inverse link
mod4 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "inverse"))
##compare the models
AIC_mods <- AIC(mod1,
                mod2,
                mod3,
                mod4)

## rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AIC),]
##mod3 best fit 

##check if mod3 really best fit 
##return the predicted (response) values from the model and add them to the single species tibble:
single_spp$pred_gaussian_log <- predict(mod3,
                                        type="response")
##return the model residuals and add to the single species tibble:
single_spp$resid_gaussian_log <- resid(mod3)
##first off let's plot the data again, and add in the predicted values from the model as a line. We can modify the plot we started earlier:
p5 <- ggplot(single_spp, aes(x=standardised_time, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")
##add in a line of the predicted values from the model
p5 <- p5 + geom_line(aes(x = standardised_time,
                         y = pred_gaussian_log),
                     col = "dodgerblue",
                     size = 1)
## we can also add in lines showing the distance of each observation from
## the value predicted by the model (i.e. these lines visualise the residual error)
p5 <- p5 + geom_segment(aes(xend = standardised_time,
                            yend = pred_gaussian_log),
                        col="lightblue")
## add a title
p5 <- p5 + ggtitle("Fitted model (gaussian with log link)")
##print the plot
p5


##plot the diagnostic graphics for model 3
plot(mod3)
##summarise the model outputs
summary(mod3)


## first off let's plot the data again
p6 <- ggplot(single_spp, aes(x=standardised_time, y=abundance)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Abundance") +
  xlab("standardised_time")
## unlike earlier here we are specifying the model type (glm), the formula,
## and the error structure and link
p6 <- p6 + geom_smooth(data=single_spp,
                       method="glm",
                       method.args = list(family = gaussian(link="log")),
                       formula = y ~ x,
                       col = "dodgerblue",
                       fill = "lightblue")
##print the plot
p6






