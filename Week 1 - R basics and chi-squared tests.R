# Introduction ------------------------------------------------------------

### STA221 W2022 Tutorial Week 1
# Author: Dory Abelman
# With inspiration from Katherine Daignault

# This script will cover the basics of R and chi-squared tests. It is intended to be presented live with an instructor sharing 
# their screen on Rstudio to a class of students. The instructor should take frequent pauses, ask questions to engage students,
# and answer students questions as they arise.

# How R works ------------------------------------------------------------

# Try running the following commands:

3+5
(11+7)/(5-2)
3*4
5/2

# When we write code in R, it does what we tell it to! 
# Anything after a '#' is not run by R, but just treated as commented text

log(1000, base =10)
factorial(3)
sqrt(4)
4^2

# We can use R as a super calculator: 

(4+8/2)/(log(10000, base = 10))*(99/33)


# An amazing quality of R is that we can use it to store results of calculations for future use

x = 3+5 

x        # by writing a stored variable, we ask R to print the result of it automatically
print(x) # we can also specify that we would like it printed by using the print() command

# A better way to store variable in R is to use a '<-'

y <- 3+5

print(y)

# Let's see if '<-' did the same thing as '=' by using a logical operator, '=='

x == y    # '==' tells you if something is true or not

# let's test the logical operator: 
5 == 5    # '==' will always only print a true or false command 
1 == 5
10/2 == 5


# Let's use a stored variable to make a sentence
my_name <- "Dory"   # write your name here!

print(my_name)

print(paste("Hello, my name is", my_name))


# If we are ever unsure of what a function does, we can get help in the following ways
?paste
help("paste")


# Congrats on writing your first R code! 


# Dataframes in R ---------------------------------------------------------

# Let's learn about using dataframes in R
# R has many build in datasets. We can load one to practice its features 

data(mtcars)  # this loads the dataset "mtcars", which contains information on 32 cars from 
# a 1974 magazine, including fuel consumption and 10 aspects of 
# automobile design and performance 

head(mtcars)    # here we are viewing the 'head', or top of the dataset 

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



# another way to learn about the properties of a dataset is by making a histogram

hist(mtcars)   # this yeilds an error, because we have to tell R exactly what we want it to plot

# '$' tells us to look at a specific column of a dataset
hist(mtcars$hp)  # here we are loading a histogram of horsepower in the dataset
hist(mtcars$wt)  # here we are loading a histogram of weight in the dataset
hist(mtcars$mpg) # here we are loading a histogram of miles per gallon in the dataset

# Congrats! You just made your first plot in R!

# We can get more specific and customize our plot with more commands

?hist # let's see the commands

# Using the commands, let's make a histogram with more customization:
hist(mtcars$hp,
     main = "Histogram of horespower in cars from 1974",
     xlab = "Horsepower",
     ylab = "Number of cars",
     col = "purple")

# Congrats on making your first customized R plot! Let's save it using export in the plots window.



# We can make many different kinds of plots in R. For example, a scatterplot

?plot

# What does the relationship between weight and horsepower look like?
plot(my_data$wt, my_data$hp)

# Adding more commands
plot(my_data$wt, my_data$hp, 
     main = "Scatterplot of weight vs horsepower",
     xlab = "Car weight", ylab = "Horsepower",
     col = "blue")

# What does this plot tell us?

# What does this plot tell us? Are you surprised by its findings? 

#What about horsepower vs miles per gallon?
plot(my_data$hp, my_data$mpg, 
     main = "Scatterplot of mpg vs horsepower",
     xlab = "Horsepower", ylab = "Fuel efficiency in miles per gallon",
     col = "blue")

# What does this plot tell us?



# Manipulating dataframes: Lets convert miles per gallon into litres per 100km 

my_data$lp100km <- 235.215/my_data$mpg # here we are saying: 
#  1 - make a new column called 'lp100km'
#  2 - for each row, input the 235.215 / (the mpg value) at this column

# Lets now see how horsepower is related to litres per 100km
plot(my_data$hp, my_data$lp100km, 
     main = "Scatterplot of litres per 100km vs horsepower",
     xlab = "Horsepower", ylab = "Fuel efficiency in litres per 100km",
     col = "blue")

# What does this plot tell us? Are you surprised by its findings? 

# Optional - try make your own scatterplot showing time to drive a quarter mile vs horsepower! 




# Chi-squared tests -------------------------------------------------------

# Let's try now make some chi-squared tests
#  Remember that Chi-squared tests help us see if there is a statistically significant difference
#  between two categorical variables


# There are several categorical variables in mt_cars such vs (engine, V-shaped or straight),
#  am (transmission, automatic or manual), gear (number of gears) and carb (number of carburetors)
#  (a careburetor mixes air and fuel into the engine to ensure it has the appropriate air-fuel ratio)


# let's first table of the engine variable
table_engine <- table(my_data$vs)
table_engine

# let's visualize our table by making a barplot (0 is V shaped, 1 is straight)
barplot(table_engine, 
        main="Barplot of engine types", 
        xlab="Engine types (0 is V-shaped, 1 is straight)", 
        ylab = "Number of cars")

# Is the plot showing the same as the table?

# let's make a table of the transmission variable
table_transmission <- table(my_data$am)
table_transmission

# make a barplot of the transmission variable
barplot(table_transmission, main="Barplot of transmission type", 
        xlab="Tranmission (0 is automatic, 1 is manual)",
        ylab = "Number of cars")


# let's make a contingency table of fate and class

con.table_engine_transmission <- xtabs(~my_data$vs + my_data$am)
con.table_engine_transmission

?xtabs # for more info on command

# We could also do: 
con.table_engine_transmission <- table(my_data$vs, my_data$am) # just uses table command
con.table_engine_transmission

?table # for more info on command

# what does this table show us?



# If we want to test if there are an equal number of cars with each engine type,
# what test would we use? -------> goodness-of-fit

# only interested in engine type variable, so can use the table we already made
table_engine

# run the test
chisq.test(table_engine)

# what is the conclusion?


# what if we test if 25% were V8's versus 75% were straight type?
chisq.test(table_engine, p = c(0.25, 0.75))

# what is the conclusion?



# Now let's test if we have the same number of cars with each transmission type.
table_transmission
chisq.test(table_transmission)

# check the residuals
chisq.test(table_transmission)$residuals




# If we wanted to test if the chances of having a V8 engine were the same for every transmission type, 
# what test would we use? ------> independence

# run the test
chisq.test(con.table_engine_transmission)

# what do we conclude here?


# look at the residuals
chisq.test(con.table_engine_transmission)$residuals

# What does this tell us? 


# Optional: try run a chi squared test on engine cylinders vs carborateurs! 


# Using simulated data ----------------------------------------------------


# In lecure, a contingency table is made by inputting simulalted data into R
#   we could do the same thing by specifying the information we want to include. 
# The only difference here is we are using real-world data, instead of telling R what we want the dataset to have

# Here is an example of using simulated data we tell R to assign to the list 'car_brand':
#  Let's say that this is the number of different cars from each make currently on the road (made up data)
#  Here we are making a list, 'car brand', and telling it how much of each brand to include

car_brand <- c(rep("Ford", times = 26), rep("Chrysler", times = 29), 
               rep("Toyota", times = 18), rep("Honda", times = 23))


# Now let's make another list of the number of SUVs and Sedans each brand offers (simulated):

car_type <- c(rep("SUV", times = 14), rep("Sedan", times = 12), rep("SUV", times = 18), rep("Sedan", times = 11), 
              rep("SUV", times = 8), rep("Sedan", times = 10), rep("SUV", times = 9), rep("Sedan", times = 14))

# We can make a contingency table in the same method we used in the mtcars dataset

con.table_car_brands_type <- xtabs(~car_brand + car_type)

con.table_car_brands_type

# We can also perform a chi-squared test on this table 

chisq.test(con.table_car_brands_type)


# Congrats on completing this tutorial! :)
