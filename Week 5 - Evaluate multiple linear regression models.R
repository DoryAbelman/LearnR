###### STA221 Tutorials Week 6
# Author: Dory Abelman
# With inspiration from Katherine Daignault


# This script will cover evaluating multiple linear regression models. It is intended to be presented live with an instructor sharing 
# their screen on Rstudio to a class of students. The instructor should take frequent pauses, ask questions to engage students,
# and answer students questions as they arise.


# Lesson 1: Import and explore the dataset ---------------------------------------------------

# let's first load our data (infant_mortality.csv on Quercus)
my_data <- read.csv(file.choose(), header=T)

#let's look at our dataset
head(my_data)

# About what the dataset means: 

        # Murder = Homocide rate per 100,000 people per year
        # HSGrad = High school graduation
        # Income = Median monthly household income
        # Illiteracy = Percentage of population that is illiterate 
        # Life.exp = Life expectancy in years


# let's look at the size, structure, and summary of the dataset
# looking at the size, structure and summary are great ways to learn more about the data we are working with

dim(my_data)  # this is the dimensions of the dataset showing legth and width
str(my_data)  # this is another way of seeing the dimensions, which also shows the structure of the dataset
summary(my_data)  # here we can see a summary of each variable in the dataset



# Lesson 2: Fitting multiple linear regression models  (review from previous weeks) ------------------------------------------

# Let's fit a linear model for life expectancy using all predictors
full_mod <- lm(Life.exp ~ Murder + HSGrad + Income + Illiteracy, data = my_data)

model_life_income <- lm(Life.exp ~ Income, data = my_data)

summary(full_mod)
summary(model_life_income)

# Let's plot some of these variables against each other to see what the relationship between them
#   looks like:

plot(my_data$Life.exp ~ my_data$Murder, main="Life expectancy vs Murder rate",
     xlab="Murder", ylab="Life expectancy")

plot(my_data$Life.exp ~ my_data$HSGrad, main="Life expectancy vs High School Graduation",
     xlab="High School Graduation", ylab="Life expectancy")

plot(my_data$Life.exp ~ my_data$Income, main="Life expectancy vs Income",
     xlab="Income", ylab="Life expectancy")

plot(my_data$Life.exp ~ my_data$Illiteracy, main="Life expectancy vs Illiteracy",
     xlab="Illiteracy", ylab="Life expectancy")

# What are your thoughts so far? Does the relationship between x and y seem linear?
#   (ie, does it look exponentioal, flat or quadratic?)


# Let's check the model assumptions

# Thinking about linearity, equal variance and independence: 
#   do these graphs show no pattern (ie, do they look like they correlate in some way, 
#    or are they just randomly scattered around the line y=0? Do points clump in one specific region)? 
#      do the plots have any bends, fans, or obvious groupings?

# Review: linearity: the relationship between x and y is linear 
#         independence: errors are mutually independent (appear random)
#         equal variance: errors are equally scattered across x-axis 
#         normality: the population is normally distributed 



# By creating plots, we can examine the model assumptions and see how they hold:

#par(mfrow=c(2,3)) <- if you want to view the all the plots next to each other, activate this line

plot(residuals(full_mod) ~ full_mod$fitted.values, main="Residuals vs Fitted",
     xlab="Fitted Values", ylab="Residuals")
abline(h=0)

plot(residuals(full_mod) ~ my_data$Murder, main="Residuals vs Murder rate",
     xlab="Murder", ylab="Residuals")

plot(residuals(full_mod) ~ my_data$HSGrad, main="Residuals vs High School Graduation",
     xlab="High School Graduation", ylab="Residuals")

plot(residuals(full_mod) ~ my_data$Income, main="Residuals vs Income",
     xlab="Income", ylab="Residuals")

plot(residuals(full_mod) ~ my_data$Illiteracy, main="Residuals vs Illiteracy",
     xlab="Illiteracy", ylab="Residuals")


# Let's check normality

# We can checl normality using a histogram. Look for a bell shape:
hist(full_mod$residuals, breaks = 50) # do we see a bell-curve like shape?

# We can also check normality by using a qqplot. This is called a 'quartile-quartile' plot. 
#  Look for points that are on the diagonal line. For more information on evaluating these plots, 
#   see this link: https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot
qqnorm(residuals(full_mod))
qqline(residuals(full_mod))

# are there any model violations?



# Let's make a prediction for mean life expectancy when we have a state with a murder rate of 5, 
#   a high school graduation rate of 60, a per-capita income of 5000 and an illiteracy rate of 1:

new <- data.frame(Murder = 5, HSGrad = 60, Income = 5000, Illiteracy = 1)
predict(full_mod, newdata=new, interval="confidence", level=0.95)

# What does this tell us? 



# Lesson 3a: Evaluating multiple linear regression models (new), part 1 ------------


# Can we drop Income from our model? We can use ANOVA:
summary(full_mod)
anova(full_mod)

#  What does the summary tell us? Notice the p-value. What does this mean?

#   Recall: 
#     1 - the coefficients measure the average effect on the response of a one 
#     unit change in the predictor, but conditional on certain values of the other 
#     predictors in the model. This is because multiple linear regression looks at 
#     the combined effect on the response of all the predictors simultaneously. Therefore:
#
#     2 - If a p-value in a multiple regression model is high, it means a predictor 
#     contributes nothing to the model above what the other predictors have contributed.
#     The coefficient of x depends as much on the other predictors as it does on its own 
#     predictor.


# Let's fit a new model to observe.
no_income <- lm(Life.exp ~ Murder + HSGrad + Illiteracy, data = my_data) # fit the model
summary(no_income) # get summary of model 
anova(no_income, full_mod) # compare differences in model

# What do the residual sum of squares tell us? 
#  Can we drop this value?


# Recall: 
#  1 - SSresiduals tells us the amount of variability in the response that is 
#  not explained by the model (or equivalently that the SSregression tells us the 
#  variability that is explained by the model).
#
#  2 - Since both the full/complete model and the reduced model are still regression 
#  models, we can find a SSresiduals and SSregression for both of them. If we look at
#  how different these are, it will tell us how much variability we miss out on
#  explaining if we were to drop those predictors from the model. 


# Helpful resurce to learn more: https://stats.stackexchange.com/questions/172157/how-to-interpret-an-anova-table-comparing-full-vs-reduced-ols-models


# Can we now drop Illiteracy?
no_illiteracy <- lm(Life.exp ~ Murder + HSGrad + Income, data = my_data)
summary(no_illiteracy)
anova(no_illiteracy, full_mod)

# What do the residual sum of squares tell us? 
#  Can we drop this value?


# Can we drop both the predictors Income and Illiteracy together?

# Let's start by fitting the reduced model:
reduced_mod <- lm(Life.exp ~ Murder + HSGrad, data=my_data)
summary(reduced_mod)

# Let's use ANOVA test to test the set of predictors
anova(reduced_mod, full_mod)


# What can we conclude from this?
#  What do the residual some of squares tell us? The P value?



# Notice how similar the adjusted R^2 is for all these models...



# Now that we modified our model, we need to check again to see if there are any model violations

# Look at residual plots for model violations in the reduced model:

#par(mfrow=c(2,2)) <- if you want to view all the plots next to each other, activate this line


plot(residuals(reduced_mod) ~ reduced_mod$fitted.values, main="Residuals vs Fitted",
     xlab="Fitted Values", ylab="Residuals")

plot(residuals(reduced_mod) ~ my_data$Murder, main="Residuals vs Murder rate",
     xlab="Murder", ylab="Residuals")

plot(residuals(reduced_mod) ~ my_data$HSGrad, main="Residuals vs High School Graduation",
     xlab="High School Graduation", ylab="Residuals")


# Checking normality 

# We can checl normality using a histogram. Look for a bell shape:
hist(residuals(reduced_mod), breaks = 50)

# We can also check normality by using a qqplot. This is called a 'quartile-quartile' plot. 
#  Look for points that are on the diagonal line. For more information on evaluating these plots, 
#   see this link: https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot

qqnorm(residuals(reduced_mod))
qqline(residuals(reduced_mod))


# Are any assumptions violated?





# Lesson 3b: Evaluating multiple linear regression models (new), part 2 ------------


# What if we only dropped one of the variables? Let's say Income:

reduced_mod_no_income <- lm(Life.exp ~ Murder + HSGrad + Illiteracy, data=my_data)
summary(reduced_mod_no_income)


# Which model is preferred: the reduced model without income or the smaller one?
#   where both income and illiteracy were dropped?

anova(reduced_mod, reduced_mod_no_income)



# Are the model assumptions better in the new reduced model where only income is dropped?

# par(mfrow=c(2,3)) <- if you want to view all the plots next to each other, activate this line

plot(residuals(reduced_mod2) ~ reduced_mod2$fitted.values, main="Residuals vs Fitted",
     xlab="Fitted Values", ylab="Residuals")

plot(residuals(reduced_mod2) ~ my_data$Murder, main="Residuals vs Murder rate",
     xlab="Murder", ylab="Residuals")

plot(residuals(reduced_mod2) ~ my_data$HSGrad, main="Residuals vs High School Graduation",
     xlab="High School Graduation", ylab="Residuals")

plot(residuals(reduced_mod2) ~ my_data$Illiteracy, main="Residuals vs Income",
     xlab="Income", ylab="Residuals")


# Checking mormality
hist(residuals(reduced_mod2), breaks = 50)

qqnorm(residuals(reduced_mod2))
qqline(residuals(reduced_mod2))



# Let's take a closer look at that normality across both models:

par(mfrow=c(1,2)) # activate to see plots next to each other

qqnorm(residuals(reduced_mod2))
qqline(residuals(reduced_mod2))
qqnorm(residuals(reduced_mod))
qqline(residuals(reduced_mod))

# it appears Normality looks slightly better in the new model (w/ illiteracy),
#   even though the ANOVA said we could throw away the Illiteracy variable.
# This could be one reason to prefer the bigger model.


# Congratulations on finishing this week's tutorial!
