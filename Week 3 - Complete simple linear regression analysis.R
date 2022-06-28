### STA221 Tutorial Week 3
# Author: Dory Abelman
# With inspiration from Katherine Daignault


# This script will cover the general steps to perform a complete simple linear regression analysis. It is intended to be presented 
# live with an instructor sharing their screen on Rstudio to a class of students. The instructor should take frequent pauses, ask 
# questions to engage students, and answer students questions as they arise.


# Lesson 1: Import and explore the dataset (review from last week with updates) ---------------------------------------------------


# let's first load our data (grades.csv on Quercus)
my_data <- read.csv(file.choose(), header=T)

#let's look at our dataset
head(my_data)

# let's look at the size, structure, and summary of the dataset
# looking at the size, structure and summary are great ways to learn more about the data we are working with

dim(my_data)  # this is the dimensions of the dataset showing legth and width
str(my_data)  # this is another way of seeing the dimensions, which also shows the structure of the dataset
summary(my_data)  # here we can see a summary of each variable in the dataset

?str # remember the help function if want more information 


# another way to learn about the properties of a dataset is by making a histogram

hist(my_data)   # this yeilds an error, because we have to tell R exactly what we want it to plot

# '$' tells us to look at a specific column of a dataset
hist(my_data$Midterm.1)  # here we are loading a histogram of horsepower in the dataset
hist(my_data$Midterm.2)  # here we are loading a histogram of weight in the dataset
hist(my_data$Homework) # here we are loading a histogram of miles per gallon in the dataset


# We can get more specific and customize our plot with more commands

?hist # let's see the commands

# Using the commands, let's make a histogram with more customization:
hist(my_data$Midterm.1,
     main = "Histogram of midterm 1 grades",
     xlab = "Grades",
     ylab = "Number of students",
     col = "purple")

# New: another interesting thing we can do with graphs in R is overlay them with lines.
#  Let's say we want to plot the mean of the dataset on out histogram. We can do this with 
#  the abline() command. This will overlay on a plot we already have loaded

?abline

abline(v = mean(my_data$Midterm.1), col="red", lwd=3, lty=2) # plots the mean with a red dotted line

# lwd is the line width. Larger numbers make it wider (optional)
# lty is the line type. We can see this by doing ?par (optional)
#   The options are: (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) 

?par # to learn more about customization options in R

# Optional (advanced) - google these functions to learn how they work to add text to a graph:
text((mean(my_data) - 7), 18, paste("Mean:", format(round(mean(my_data$Midterm.1), 2), nsmall = 2)))



# Lesson 2: Correlation (review with updates)  --------------------------------------------

# let's use the correlation value to see if midterm grades are correlated. Do you expect them to be? 
?cor
cor(my_data$Midterm.1, my_data$Midterm.2)   # get the correlation

# Based on this correlation value, do you think they are correlated strongly?

# let's make a scatterplot to see what this relationship looks like
plot(my_data$Midterm.1, my_data$Midterm.2, main="Midterm 2 versus Midterm 1 grades",
     xlab="Midterm 1 grades", ylab="Midterm 2 grades")

# Optional (advanced): try change the point type by adding the pch=() command! Try numbers between 1-20)
#    good source with info on these point types - http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r


# Does it look like a linear model will be appropriate?


# Let's fit the data to a simple linear model
# first try midterm 2 grades versus midterm 1 grades
linearmodel_midterm1_vs_2 <- lm(Midterm.2 ~ Midterm.1, data=my_data)
summary(linearmodel_midterm1_vs_2)

# Let's visualize this by replotting it with a new line showing the slope and intercept our model gave us
plot(my_data$Midterm.1, my_data$Midterm.2, main="Midterm 2 versus Midterm 1 grades",
     xlab="Midterm 1 grades", ylab="Midterm 2 grades")

# Using the ab line command: (a is intercept, b is slope)
abline(12.0054,0.7210, lwd=2, lty=3, col="black")

# Instead of typing in the intercept and slope directly, 
#  we can also retrieve them using the coefficients directly from the model:

abline(linearmodel_midterm1_vs_2$coefficients[1],linearmodel_midterm1_vs_2$coefficients[2], lwd=2, lty=1, col="red")
#      notice how these lines are the same, we just pulled the first and second 
#      coefficients directly from the summary of the model


# Now let's see of midterm grades are correlated with homework
cor(my_data$Midterm.2, my_data$Homework)

# Let's plot to see the relationship
plot(my_data$Homework, my_data$Midterm.2, main="Midterm 2 versus Homework grades",
     xlab="Homework grades", ylab="Midterm 2 grades")

# Let's now try make a linear model for the midterm 2 versus homework grades
linearmodel_homework_vs_midterm2 <- lm(Midterm.2 ~ Homework, data=my_data)
summary(linearmodel_homework_vs_midterm2)

# Let's add a line to see how the data fits around it:

plot(my_data$Homework, my_data$Midterm.2, main="Midterm 2 versus Homework grades",
     xlab="Homework grades", ylab="Midterm 2 grades")
abline(linearmodel_homework_vs_midterm2$coefficients[1],linearmodel_homework_vs_midterm2$coefficients[2], lwd=2, lty=1, col="red")


# It is important to think about slope and intercept to help us understand the relationship between X and Y
# based on the R^2 values, which model seems to fit the data better?



# Lesson 3: Checking model assumptions (review from last week) ------------------------------------


# Let's check our model assumptions to see if it fits the data well

# Thinking about linearity, equal variance and independence: 
#   do these graphs show no pattern (ie, do they look like they correlate in some way, 
#    or are they just randomly scattered around the line y=0? Do points clump in one specific region)? 

# Review: linearity: the relationship between x and y is linear 
#         independence: errors are mutually independent (appear random)
#         equal variance: errors are equally scattered across x-axis 
#         normality: the population is normal 

plot(my_data$Homework, linearmodel_homework_vs_midterm2$residuals, main="Residuals versus Homework grades",
     xlab="Homework grades", ylab="Residuals")
abline(h=0, col ="blue")

plot(linearmodel_homework_vs_midterm2$fitted.values, linearmodel_homework_vs_midterm2$residuals, main="Residuals versus Fitted Values",
     xlab="Model fitted values", ylab="Residuals")
abline(h=0)

# Normality: is the data mostly on the diagonal line?
hist(linearmodel_homework_vs_midterm2$residuals, breaks = 25)
qqnorm(linearmodel_homework_vs_midterm2$residuals)
qqline(linearmodel_homework_vs_midterm2$residuals)

# Optional - good resource on qqplots: https://towardsdatascience.com/q-q-plots-explained-5aa8495426c0

# It appears that the top and bottom points on the qq plot do not fit the line! Thus, they are not equally distributed. 
# We should be a suspicious of our linear regression inference because not all assumptions satisfied.



# Lesson 4: Determining statistical significance (new) --------------------------

# Let's look at whether the regression line is statistically significant using ANOVA
anova(linearmodel_homework_vs_midterm2)

# Further, let's get confidence interval for the slope and intercept of the regression line from our model:
confint(linearmodel_homework_vs_midterm2, level=0.95)

# The 2.5% and 97.5% means: 
#   If you repeated this experiment many times, the true parameter value will be below the CI in 2.5% 
#   of cases and above it in another 2.5% of cases - and the CI will cover it in 95% of cases.
# Optinal: good explanation of the confidence interval command in R - https://stats.stackexchange.com/questions/472527/what-are-the-values-of-the-confint-function-in-r


# Now let's predict the average midterm 2 grade for students with a homework grade of 85
new <- data.frame(Homework = 85)
predict(linearmodel_homework_vs_midterm2, newdata=new, interval="confidence", level=0.95)

# let's see what happens when we change the confidence level to 90%
predict(linearmodel_homework_vs_midterm2, newdata=new, interval="confidence", level=0.90)

# does it make sense that the 90% interval is narrower than the 95%?
# (for the 95%, 95% of values predicted should be within that range. For the 90%, only 90% of values need to be)




# Lesson 5: Determining outliers (new) -----------------------------------------

# Now let's check for outliers

plot(my_data$Homework, my_data$Midterm.2, main="Midterm 2 versus Homework grades",
     xlab="Homework grades", ylab="Midterm 2 grades")
abline(linearmodel_homework_vs_midterm2$coefficients[1],linearmodel_homework_vs_midterm2$coefficients[2], lwd=2, lty=1, col="red")

# Do you see any outliers? 

# Recall: leverage points are data points whose x value is very far from the average x value. Do you see any of these?
#         influential points are data points that will change where the regression line is placed through the data

# the homework grade near 20 seems like a leverage outlier. Let's remove it 
new_grades <- my_data[which(my_data$Homework > 30),] # here we are telling R: only keep the values where the homework grade is more than 30
                                                     # and save it in a dataframe called 'new_grades'


# (Optinal) what if we want to see who this student is?

which(my_data$Homework < 30) # this tells us the row numbers of data values where homework is less than 30 
                             # it essentially tells R: 'return us row numbers in the dataset when the homework column value is less than 30

my_data[18,] # this command tells us to bring up all the columns from the data with row number 18
              #  we see the student with a homework grade less than 30 is Timothy!

# (Optional) another way to remove a row in R is with the '-' symbol. Now that we know the outlier is row number 18, we can remove it like this: 
new_grades <- my_data[-18,]  # here we are telling R: remove all column values from row number 18.



# Now let's fit a new linear model without this point
linearmodel_homework_vs_midterm2_homework_over_30 <- lm(Midterm.2 ~ Homework, data = new_grades)

summary(linearmodel_homework_vs_midterm2_homework_over_30) # and get a summary of the model
summary(linearmodel_homework_vs_midterm2)

# is the point with the homework grade under 20 this a leverage or influential point, or both?
plot(my_data$Midterm.2 ~ my_data$Homework)
abline(a = linearmodel_homework_vs_midterm2$coefficients[1], b = linearmodel_homework_vs_midterm2$coefficients[2], lwd=2, lty=1, col="red")
abline(a = linearmodel_homework_vs_midterm2_homework_over_30$coefficients[1], b = linearmodel_homework_vs_midterm2_homework_over_30$coefficients[2], lwd=2, lty=2, col="blue")
legend("topleft", legend=c("original", "HW >30"), col=c("red", "blue"), lty=c(1,2))

# The point is also influential because moves the line a bit, bit does not have a very strong influence. 

plot(new_grades$Midterm.2 ~ new_grades$Homework)
abline(a = linearmodel_homework_vs_midterm2_homework_over_30$coefficients[1], b = linearmodel_homework_vs_midterm2_homework_over_30$coefficients[2], lwd=2, lty=2, col="blue")

# Congrats on finishing the week 3 tutorial! 

