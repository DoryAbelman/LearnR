###### STA221 Tutorials Week 4
# Authors: Dory Abelman
# With inspiration from Katherine Daignault

# This script will cover an introduction to multiple linear regression. It is intended to be presented live with an instructor sharing 
# their screen on Rstudio to a class of students. The instructor should take frequent pauses, ask questions to engage students,
# and answer students questions as they arise.

# First set the working directory 

setwd("~/Documents/R")


# Lesson 1: Import and explore the dataset (review from last week with updates) ---------------------------------------------------

# let's first load our data (infant_mortality.csv, attached seperately)

my_data <- read.csv(file.choose(), header=T)

#let's look at our dataset - always good to view it first before getting into analysis
head(my_data)


# About what the dataset means: 

        # Infant.mort = infant mortality rate
        # CDR = child death rate
        # HS.drop = high school drop out %
        # low.BW = percent low birth weight
        # Teen.Births = # teen births per 100000
        # Teen.Deaths = # teen deaths per 100000
colnames(my_data) <- c("Infant mortality", "Child death rate","Test123")

# let's look at the size, structure, and summary of the dataset
# looking at the size, structure and summary are great ways to learn more about the data we are working with

dim(my_data)  # this is the dimensions of the dataset showing legth and width
str(my_data)  # this is another way of seeing the dimensions, which also shows the structure of the dataset
summary(my_data)  # here we can see a summary of each variable in the dataset


# suppose we want to fit a multiple linear model for: Infant.mort based on HS.drop, low.BW and Teen.Births

# Let's heck the scatterplots for these variables to see if a linear model is appropriate, and to
#   observe the relationship between the variables:

#par(mfrow=c(1, 3)) <- if you want to view the 3 plots next to each other, activate this line

# Let's plot infant mortality vs highschool dropout:
plot(my_data$Infant.mort ~ my_data$HS.drop, main="Mortality vs High School Drop Out",
     xlab="High School Drop Out Rate", ylab="Mortality Rate")

plot(my_data$HS.drop, my_data$Infant.mort, main="Mortality vs High School Drop Out",
     xlab="High School Drop Out Rate", ylab="Mortality Rate")

# Let's plot infant mortality vs low birthweight:
plot(my_data$Infant.mort ~ my_data$low.BW, main="Mortality vs Low Birth Weight %",
     xlab="Low Birth Weight %", ylab="Mortality Rate")

# # Let's plot infant mortality vs teen births:
plot(my_data$Infant.mort ~ my_data$Teen.Births, main="Mortality vs Teen Birth Rate",
     xlab="Teen Birth Rate", ylab="Mortality Rate")



# Lesson 2: Fitting multiple linear regression models  (review with new concepts) ------------------------------------------


# Next, let's fit a linear model with these 3 predictors:
linear_model_Hs_drop_vs_mortality <- lm(Infant.mort ~ HS.drop, data = my_data)

summary(linear_model_Hs_drop_vs_mortality)

linear_model_3_predictors <- lm(Infant.mort ~ HS.drop + low.BW + Teen.Births, data = my_data)
summary(linear_model_3_predictors)

# What does this summary tell us?

# Recall: In multiple linear regression, the coefficient shows us the average effect on the response 
#  for a 1 unit increase in the predictor, when the other predictors are held fixed at some value. 


# Let's also think about our coefficients. Does a linear model seem appropriate?


# Let's make a scatterplot of residuals vs fitted values:
plot(linear_model_3_predictors$residuals ~ linear_model_3_predictors$fitted.values, main="Residuals vs Fitted Values", xlab="Fitted", ylab="Residuals")
abline(h=0, col="red", lwd=3, lty=3) # viewing the y=0 line


# Thinking about linearity, equal variance and independence: 
#   do these graphs show no pattern (ie, do they look like they correlate in some way, 
#    or are they just randomly scattered around the line y=0? Do points clump in one specific region)? 
#      do the plots have any bends, fans, or obvious groupings?

# Review: linearity: the relationship between x and y is linear 
#         independence: errors are mutually independent (appear random)
#         equal variance: errors are equally scattered across x-axis 
#         normality: the population is normal 



# Let's make a scatterplot of residuals vs each predictor:
#par(mfrow=c(1, 3)) <- if you want to view the 3 plots next to each other, activate this line

plot(linear_model_3_predictors$residuals ~ my_data$HS.drop, main="Residuals vs High School Drop Out", 
     xlab="High School Drop Out", ylab="Residuals")
abline(h=0, col="red", lwd=3, lty=3) # viewing the y=0 line


plot(linear_model_3_predictors$residuals ~ my_data$low.BW, main="Residuals vs Low Birth Weight", 
     xlab="Low Birth Weight", ylab="Residuals")
abline(h=0, col="red", lwd=3, lty=3) # viewing the y=0 line


plot(linear_model_3_predictors$residuals ~ my_data$Teen.Births, main="Residuals vs Teen birth rate", 
     xlab="Teen birth rate", ylab="Residuals")
abline(h=0, col="red", lwd=3, lty=3) # viewing the y=0 line


# Let's check normality:

hist(linear_model_3_predictors$residuals, breaks = 50) # do we see a bell-curve like shape?

qqnorm(linear_model_3_predictors$residuals) # a better way to check than hist is a qqplot 
qqline(linear_model_3_predictors$residuals)

# What are your thoughts on the qqplot? Is the data normally distributed? 




# Lesson 3: Examining multiple linear regression models -------------------


# Let's look at the individual simple linear models for each predictor:
linear_model_mort_vs_HS_drop <- lm(Infant.mort ~ HS.drop, data=my_data)
summary(linear_model_mort_vs_HS_drop)


# What does this show us? 

# Recall: 
#  This shows us that when mortality increases by one unit, high school dropout changes by X units.
#  This does not consider the other factors our multiple regression model adressed, but looks at high school
#   dropout alone. It does not hold other variables constant as was done in the multiple regression model.


# Again, here is the plot for reference: 

plot(my_data$Infant.mort ~ my_data$HS.drop, main="Mortality vs High School Drop Out",
     xlab="High School Drop Out Rate", ylab="Mortality Rate")
abline(a = linear_model_mort_vs_HS_drop$coefficients[1], b = linear_model_mort_vs_HS_drop$coefficients[2], col="red", lwd=2, lty=3)


# Let's create models looking at other variables: 
linear_model_mort_vs_low_birthweight <- lm(Infant.mort ~ low.BW, data=my_data)
summary(linear_model_mort_vs_low_birthweight)

linear_model_mort_vs_teen_births <- lm(Infant.mort ~ Teen.Births, data=my_data)
summary(linear_model_mort_vs_teen_births)


# Which model is the best?

# Low birth weight seems to give us the best simple linear model (based on R squared value)




# What if we were interested in looking at the effect of high school drop out on infant mortality 
#   after adjusting for the other two variables? - use a partial regression plot:

# First we fit linear model of mortality on the other predictors
linear_model_mort_vs_others <- lm(Infant.mort ~ low.BW + Teen.Births, data=my_data)

# Here we are accounting for the effects of low birthweight and teen births on mortality

# Then we fit a linear model of high school drop out against other predictors
linear_model_HS_drop_vs_others <- lm(HS.drop ~ low.BW + Teen.Births, data=my_data)
summary(linear_model_HS_drop_vs_others)
# Here we are accounting for the effects of low birthweight and teen births on height

# Then we plot the residuals against each other:
#       Here we are saying, 'plot the effect of low birthweight and teen births on mortality 
#       against the effects of low birthweight and teen births on high school dropout
#         The difference between these point should be - take a guess! 

plot(linear_model_mort_vs_others$residuals ~ linear_model_HS_drop_vs_others$residuals, main="Partial Regression Plot for High School Drop Out",
     xlab="High School Drop Out residuals", ylab="Mortality residuals")

# We can add a regression line to summarize the relationship of highschool dropout on infant mortality:
linear_model_residuals <- lm(linear_model_mort_vs_others$residuals ~ linear_model_HS_drop_vs_others$residuals)
summary(linear_model_residuals)

abline(a = linear_model_residuals$coefficients[1], b = linear_model_residuals$coefficients[2], col="red", lwd=2, lty=3)


# Let's fit a different model:

linear_model_3_predictors <- lm(Infant.mort ~ HS.drop + low.BW + Teen.Births, data = my_data)
summary(linear_model_3_predictors)

# What are your thoughts on this result? What does this tell us?


# Recall: 
#    The slope of the relationship between the residuals will always equal the coefficient 
#    of that variable in the multiple linear regression (i.e. -0.10848 for high school drop outs) 

#       Here we are saying, 'plot the effect of low birthweight and teen births on mortality 
#       against the effects of low birthweight and teen births on high school dropout
#         The difference between these point should be - take a guess! 
#         The effect of height on infant mortality, taking account  the other two variables


# Congratulations on finishing this week's tutorial!

