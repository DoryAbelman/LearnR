### STA221 W2021 Tutorial Week 2
# Author: Dory Abelman
# With inspiration from Katherine Daignault

# This script will cover scatterplots, simple linear regression, and importing data into R. It is intended to be presented live with an 
# instructor sharing their screen on Rstudio to a class of students. The instructor should take frequent pauses, ask questions to engage 
# students, and answer students questions as they arise.


# Lesson 1: Making scatterplots + review from last week  -------------------------------------------

# Review from last week: loading dataset, dataset properites 

data(mtcars)  # this loads the dataset "mtcars", which contains information on 32 cars from 
# a 1974 magazine, including fuel consumption and 10 aspects of 
# automobile design and performance 

?mtcars
mtcars

head(mtcars)    # here we are viewing the 'head', or top of the dataset 
?mtcars

# remember if we are ever unsure of something, we can ask for help 

?head         #either option works
help("head")

?mtcars
help("mtcars")

# let's save the "mtcars" dataset into a variable called my_data
my_data <- mtcars
my_data

# look at the size, structure, and summary of the dataset
# looking at the size, structure and summary are great ways to learn more about the data we are working with

dim(my_data)  # this is the dimensions of the dataset showing legth and width
str(my_data)  # this is another way of seeing the dimensions, which also shows the structure of the dataset
summary(my_data)  # here we can see a summary of each variable in the dataset

?str # remember the help function if want more information 

# We can make many different kinds of plots in R. 
# An important plot for today is a scatterplot

?plot   # shows us plots available

# Let's see: what does the relationship between weight and horsepower look like?
plot(my_data$wt, my_data$hp)

# Adding more commands
plot(my_data$wt, my_data$hp, 
     main = "Scatterplot of weight vs horsepower",
     xlab = "Car weight", ylab = "Horsepower",
     col = "blue")

# What does this plot tell us? Are you surprised by its findings? 

#What about weight vs miles per gallon?
plot(my_data$wt, my_data$mpg, 
     main = "Scatterplot of weight vs mpg",
     xlab = "Weight (1000's of pounds)", ylab = "Fuel efficiency in miles per gallon",
     col = "blue")

# What does this plot tell us?


# Manipulating dataframes: Lets convert miles per gallon into litres per 100km 

my_data$lp100km <- 235.215/my_data$mpg # here we are saying: 
#  1 - make a new column called 'lp100km'
#  2 - for each row, input the 235.215 / (the mpg value) at this column

# Lets now see if  weight is related to litres per 100km
plot(my_data$wt, my_data$lp100km, 
     main = "Scatterplot of litres per 100km vs weoght",
     xlab = "Weight (1000's of pounds)", ylab = "Fuel efficiency in litres per 100km",
     col = "blue")


# What does this plot tell us? Are you surprised by its findings? 
# What type of association do we see?

# Optional - try make your own scatterplot showing time to drive a quarter mile vs horsepower! 



# Lession 2: Correlation & Simple Linear Regression in R ------------------


# Let's look to see if car fuel efficiency is correlated with weight
# can look at the correlation of fuel efficiency and horsepower
cor(my_data$wt, my_data$lp100km)
?cor
?lm
# let's check if a linear model is appriopriate
# we can estimate using a scatterplot (see above)
# or we can use residuals in plot:
model <- lm(wt ~ lp100km, data=my_data)
residuals <- model$residuals
plot(residuals ~ my_data$lp100km, main = "Residuals vs Predictor", xlab = "Fuel efficiency in lp100km", ylab="Residuals")

# we can also use residuals against fitted values plot:
fitted <- model$fitted.values
plot(residuals ~ fitted, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab="Residuals")

# Let's find the coefficient of determination using correlation:
cor(my_data$wt, my_data$lp100km)^2

# Does this agree with the model output?
summary(model)
?summary

# Let's see what the coefficient of determination shows us?
deviations_y <- my_data$wt - mean(my_data$wt)
residuals_column <- model$residuals
deviations <- c(deviations_y, residuals_column)
what <- c(rep("Weight", length(deviations_y)), rep("Residuals", length(residuals_column)))
boxplot(deviations~what, col=c("green", "yellow"))


# Let's interpret the intercept and slope for these data:
summary(model)


# If we want to buy a car that burns 10l/100km, how much should it weigh?
new <- data.frame(lp100km = 10)
predict(model, newdata = new)

# If we want want a car that burns 7l/100km, how much should it weigh?
new <- data.frame(lp100km = 7)
predict(model, newdata = new)

# Based on this information you guess why some countries with more expensive
#  fuel prices have much higer ownership of small cars? Or why American cars are 
#  considered big? (Hint: Americans have among the lowest fuel prices in the world)


# Let's check to see if independence holds:
plot(residuals ~ my_data$lp100km, main = "Residuals vs Predictor", xlab = "lp100km", ylab="Residuals")

# we can have groupings of cars that use <10l/100km and cars >10l/100km

# Let's look at if there are differences between these groups and weight
data_over_under <- ifelse(my_data$lp100km < 10, "<10L/100km", ">10L/100km")
boxplot(my_data$wt ~ data_over_under, main = "Boxplot of Weight vs efficiency", xlab="Fuel consumption",
        ylab="Weight(1000's of pounds)")

#There seems to be clear groupings in the data


# Let's check the equal variance assumption
plot(residuals ~ fitted, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab="Residuals")
# What does this tell us?

# Let's check normality
hist(residuals, main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals)
qqline(residuals)

# What does this tell us? 




# Lesson 3: Importing data into R -----------------------------------------

# Next week we will be importing a dataset into R. There are several ways to do this.
# The easiest way we will use is shown below: 

data_import <- read.csv(file.choose(), header=T) # This will work for a csv file 

# There are other commands for other file types, such as read.table for a tsv file



# Congrats on finishing your second tutorial!


# Supplementary information -----------------------------------------------

print("Hello there")

test <- c(1:22, "X")
print(test)
