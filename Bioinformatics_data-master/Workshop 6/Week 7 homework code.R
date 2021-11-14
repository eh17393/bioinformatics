library(tidyverse)
library(vroom)
library(usethis)
library(devtools)
library(lubridate)
library(multcomp)
library(scales)
library(glmmTMB)
library(DHARMa)
library(fitdistrplus)
library(MuMIn)
library(MASS)
library(viridisLite)
library(viridis)
library(ggplot2)

##read in dataset 1
dataset_1 <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%206/data%201.csv")
##read in dataset 2
dataset_2 <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%206/data%202.csv")
##read in dataset 3
dataset_3 <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%206/data%203.csv")
##read in dataset 4
dataset_4 <- vroom("https://raw.githubusercontent.com/eh17393/bioinformatics/main/Bioinformatics_data-master/Workshop%206/data%204.csv")


##visualise dataset 1 with x1
ggplot(dataset_1, aes(x = y, y = x1)) +
  geom_point() +
  geom_smooth(method = "lm")

##visualise dataset 1 with x2 
ggplot(dataset_1, aes(x = y, y = x2)) +
  geom_point() +
  geom_smooth(method = "lm")
  ##potentially be a positive correlation - increase in y = increase in x2
##fit a glm with gaussian distribution
mod1_x2 <- glm(x2 ~ y, data = dataset_1, family = "gaussian")
plot(mod1_x2)
summary(mod1_x2)
##fit glm with poisson distribution
mod2_x2 <- glm(x2 ~ y, data = dataset_1, family = "poisson")
  ##lots of warnings
##fit a glm with a gaussian distribution with a log link
mod3_x2 <- glm(x2 ~ y,
                   data = dataset_1,
                   family = gaussian(link = "log"))
##fit glm with a guassian model with an inverse link
mod4_x2 <- glm(x2 ~ y,
                   data = dataset_1,
                   family = gaussian(link = "inverse"))
##compare models with AIC
library(gamlr)
##compare the models 
AIC_mods <- data.frame(model = c("mod1_x2", "mod2_x2", "mod3_x2", "mod4_x2"),
                       AICc = c(AICc(mod1_x2), AICc(mod2_x2), AICc(mod3_x2), AICc(mod4_x2)))

## rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AICc),]
  ##mod 1 the best therefore gaussian error distribution 

##visualise dataset 1 with x3
ggplot(dataset_1, aes(x = y, y = x3)) +
  geom_point() +
  geom_smooth(method = "lm")
mod_data1_x3 <- glm(x3 ~ y, data = dataset_1, family = "gaussian")
summary(mod_data1_x3)

##visualise dataset 1 with x4
ggplot(dataset_1, aes(x = y, y = x4)) +
  geom_point()

##look at a data where x1 is logged
ggplot(dataset_1, aes(x=y, y=x1)) + 
  geom_point() + 
  scale_y_continuous(trans='log10') + 
  theme_bw() +
  geom_smooth(method = "lm") +
  ggtitle("Logged data")


##initially fit with gaussian model as data non-integer 
mod_data1_1 <- glm(x1 ~ y, data = dataset_1, family = "gaussian")
##set a 2x2 plot area, so we get a single pannel with 4 plots:
par(mfrow = c(2, 2))
plot(mod_data1_1)
  ##Q-Q plot not very normally distributed 
summary(mod_data1_1)
##fit glm with poisson distribution
mod_data1_2 <- glm(x1 ~ y, data = dataset_1, family = "poisson")
plot(mod_data1_2)
  ##lots of warnings
##fit a glm with a gaussian distribution with a log link
mod_data1_3 <- glm(x1 ~ y,
            data = dataset_1,
            family = gaussian(link = "log"))
plot(mod_data1_3)
  ##not normally distributed 
##fit glm with a guassian model with an inverse link
mod_data1_4 <- glm(x1 ~ y,
            data = dataset_1,
            family = gaussian(link = "inverse"))
plot(mod_data1_4)
summary(mod_data1_4)
##compare models with AIC
library(gamlr)
##compare the models 
AIC_mods <- data.frame(model = c("mod_data1_1", "mod_data1_2", "mod_data1_3", "mod_data1_4"),
                       AICc = c(AICc(mod_data1_1), AICc(mod_data1_2), AICc(mod_data1_3), AICc(mod_data1_4)))

## rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AICc),]


##visualise dataset_2
dataset_2

##visualise dataset_2 with x1
ggplot(dataset_2, aes(x = y, y = x1)) +
  geom_point() +
  geom_smooth(method = "lm")

##visualise dataset_2 with x2 
ggplot(dataset_2, aes(x = y, y = x2)) +
  geom_point() +
  geom_smooth(method = "lm")

##visualise dataset_2 with x3
ggplot(dataset_2, aes(x = y, y = x3)) +
  geom_point() +
  geom_smooth(method = "lm")
  ##potentially correlation with y 

##fit a glm with gaussian distribution
mod1_x3 <- glm(x3 ~ y, data = dataset_2, family = "gaussian")
plot(mod1_x3)
summary(mod1_x3)
##fit glm with poisson distribution
mod2_x3 <- glm(x3 ~ y, data = dataset_2, family = "poisson")
##lots of warnings
##fit a glm with a gaussian distribution with a log link
mod3_x3 <- glm(x3 ~ y,
               data = dataset_2,
               family = gaussian(link = "log"))
plot(mod3_x3)
##fit glm with a guassian model with an inverse link
mod4_x3 <- glm(x3 ~ y,
               data = dataset_2,
               family = gaussian(link = "inverse"))
plot(mod4_x3)
##compare models with AIC
library(gamlr)
##compare the models 
AIC_mods_2 <- data.frame(model = c("mod1_x3", "mod2_x3", "mod3_x3", "mod4_x3"),
                       AICc = c(AICc(mod1_x3), AICc(mod2_x3), AICc(mod3_x3), AICc(mod4_x3)))

## rank them by AIC using the order() function
AIC_mods_2[order(AIC_mods$AICc),]
  ##mod 4 is best fit therefore gaussian error distribution with inverse link


##visualise dataset_3 
dataset_3

##visualise dataset_3 with x1
ggplot(dataset_3, aes(x = y, y = x1)) +
  geom_point()
##plot histogram
ggplot(dataset_3, aes(x= x1)) +
  geom_histogram(binwidth = .1, alpha = .5, position="stack")
  ##not very normally distributed 

##visualise dataset_3 with x2
ggplot(dataset_3, aes(x = y, y = x2)) +
  geom_point()
##plot histogram
ggplot(dataset_3, aes(x= x2)) +
  geom_histogram(binwidth = .1, alpha = .5, position="stack")
  ##not normally distributed 




##visualise dataset_4
dataset_4

##visualise dataset_4 with x1
ggplot(dataset_4, aes(x = y, y = x1)) +
  geom_point() +
  geom_smooth(method = "lm")
  ##potentially positive correlation with y

##fit a glm with gaussian distribution
mod1_d4x1 <- glm(x1 ~ y, data = dataset_4, family = "gaussian")
plot(mod1_d4x1)
summary(mod1_d4x1)
##fit glm with poisson distribution
mod2_d4x1 <- glm(x1 ~ y, data = dataset_4, family = "poisson")
plot(mod2_d4x1)
  ##residuals vs leverage better than gaussian
summary(mod2_d4x1)
  ##not significant therefore log x1
mod2log_d4x1 <- glm(log10(x1) ~ y, data = dataset_4, family = "poisson")
plot(mod2log_d4x1)
summary(mod2log_d4x1)
  ##x1 is significant with poisson error distribution and log link function

##visualise dataset_4 with x2
ggplot(dataset_4, aes(x = y, y = x2)) +
  geom_point() +
  geom_smooth(method = "lm")
  ##clear positive correlation with y 

##fit a glm with gaussian distribution
mod1_d4x2 <- glm(x2 ~ y, data = dataset_4, family = "gaussian")
plot(mod1_d4x2)
summary(mod1_d4x2)
##fit glm with poisson distribution
mod2_d4x2 <- glm(x2 ~ y, data = dataset_4, family = "poisson")
plot(mod2_d4x2)
summary(mod2_d4x2)
  ##residuals vs leverage within 0.5
mod2log_d4x2 <- glm(log10(x2) ~ y, data = dataset_4, family = "poisson")
plot(mod2log_d4x2)
summary(mod2log_d4x2)
  ##much better fit for normal Q-Q plot
##x2 is significant with poisson error distribution and log link function


##visualise dataset_4 with x3
ggplot(dataset_4, aes(x = y, y = x3)) +
  geom_point() +
  geom_smooth(method = "lm")
  ##no correlation





