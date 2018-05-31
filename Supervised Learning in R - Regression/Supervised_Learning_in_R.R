# See also: http://r-statistics.co/Linear-Regression.html
# Code a simple one-variable regression
# 
# For the first coding exercise, you'll create a formula to define a one-variable modeling task, and then fit a linear model to the data. You are given the rates of male and female unemployment in the United States over several years (Source).
# 
# The task is to predict the rate of female unemployment from the observed rate of male unemployment. The outcome is female_unemployment, and the input is male_unemployment.
# 
# The sign of the variable coefficient tells you whether the outcome increases (+) or decreases (-) as the variable increases.
# 
# Recall the calling interface for lm() is:

# unemployment is loaded in the workspace
summary(unemployment)

# Define a formula to express female_unemployment as a function of male_unemployment
fmla <- female_unemployment ~ male_unemployment

# Print it
as.formula(fmla)

# Use the formula to fit a model: unemployment_model
unemployment_model <- lm(fmla, data=unemployment)


#Print the model. Is the coefficent for male unemployment consistent with what you would expect? Does female unemployment increase as male unemployment does?
unemployment_model

#####

# Examining a model
# 
# Let's look at the model unemployment_model that you have just created. There are a variety of different ways to examine a model; each way provides different information. We will use summary(), broom::glance(), and sigr::wrapFTest().


# broom and sigr are already loaded in your workspace
# Print unemployment_model
print(unemployment_model)

# Call summary() on unemployment_model to get more details
#In addition to the coefficient values, you get standard errors on the coefficient estimates, and some goodness-of-fit metrics like R-squared.
summary(unemployment_model)

# Call glance() on unemployment_model to see the details in a tidier form
#Call glance() on the model to see the performance metrics in an orderly data frame. Can you match the information from summary() to the columns of glance()?
glance(unemployment_model)

# Call wrapFTest() on unemployment_model to see the most relevant details
wrapFTest(unemployment_model)


#####

#  Predicting from the unemployment model
#  
#  In this exercise, you will use your unemployment model unemployment_model to make predictions from the unemployment data,
# and compare predicted female unemployment rates to the actual observed female unemployment rates on the training data, 
# unemployment. You will also use your model to predict on the new data in newrates, which consists of only one observation,
# where male unemployment is 5%.
#  
#  The predict() interface for lm models takes the form
#  
#  predict(model, newdata)
#  
#  You will use the ggplot2 package to make the plots, so you will add the prediction column to the unemployment data frame.
# You will plot outcome versus prediction, and compare them to the line that represents perfect predictions (that is when the
# outcome is equal to the predicted value).
#  
#  The ggplot2 command to plot a scatterplot of dframe$outcome versus dframe$pred (pred on the x axis, outcome on the
# y axis), along with a blue line where outcome == pred is as follows:
# 
#   
#   ggplot(dframe, aes(x = pred, y = outcome)) + 
#   geom_point() +  
#   geom_abline(color = "blue")

newrates = as.data.frame(5)
colnames(newrates) = "male_unemployment"

# unemployment is in your workspace
summary(unemployment)

# newrates is in your workspace
newrates

# Predict female unemployment in the unemployment data set
unemployment$prediction <-  predict(unemployment_model)

# load the ggplot2 package
library(ggplot2)

# Make a plot to compare predictions to actual (prediction on x axis)
ggplot(unemployment, aes(x = prediction, y = female_unemployment)) + 
  geom_point() +
  geom_abline(color = "blue")

# Predict female unemployment rate when male unemployment is 5%
pred <- predict(unemployment_model, newdata =newrates)
# Print it
pred


#######
# 
# Multivariate linear regression (Part 1)
# 
# In this exercise, you will work with the blood pressure dataset (Source), 
# and model blood_pressure as a function of weight and age.

# bloodpressure is in the workspace
summary(bloodpressure)

# Create the formula and print it
fmla <- blood_pressure ~ age + weight
fmla

# Fit the model: bloodpressure_model
bloodpressure_model <- lm(fmla , bloodpressure)

# Print bloodpressure_model and call summary() 
bloodpressure_model
summary(bloodpressure_model)
 
# Now you will make predictions using the blood pressure model bloodpressure_model that you fit in the previous exercise.
# 
# You will also compare the predictions to outcomes graphically. ggplot2 is already loaded in your workspace. Recall the plot command takes the form:

# bloodpressure is in your workspace
summary(bloodpressure)

# bloodpressure_model is in your workspace
bloodpressure_model

# predict blood pressure using bloodpressure_model :prediction
bloodpressure$prediction <- predict(bloodpressure_model)

# plot the results
ggplot(bloodpressure, aes(x=prediction, y= blood_pressure)) + 
  geom_point() +
  geom_abline(color = "blue")

###########################################
#next chapter
#############################################

# Graphically evaluate the unemployment model
# 
# In this exercise you will graphically evaluate the unemployment model, unemployment_model, that you fit to the 
# unemployment data in the previous chapter. Recall that the model predicts female_unemployment from male_unemployment.
# 
# You will plot the model's predictions against the actual female_unemployment; recall the command is of the form
# 
# ggplot(dframe, aes(x = pred, y = outcome)) + 
#        geom_point() +  
#        geom_abline()
# 
# Then you will calculate the residuals:
# 
# residuals <- actual outcome - predicted outcome
# 
# and plot predictions against residuals. The residual graph will take a slightly different form: you compare 
# the residuals to the horizonal line x=0
# (using geom_hline()) rather than to the line x=y. The command will be provided.


# unemployment is in the workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Make predictions from the model
unemployment$predictions <- predict(unemployment_model)

# Fill in the blanks to plot predictions (on x-axis) versus the female_unemployment rates
ggplot(unemployment, aes(x = predictions, y = female_unemployment)) + 
  geom_point() + 
  geom_abline()

# Calculate residuals
unemployment$residuals <- unemployment$female_unemployment - unemployment$predictions

# Fill in the blanks to plot predictions (on x-axis) versus the residuals
ggplot(unemployment, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("residuals vs. linear model prediction")


#####
# The gain curve to evaluate the unemployment model
# 
# In the previous exercise you made predictions about female_unemployment and visualized the predictions and the 
# residuals. Now, you will also plot the gain curve of the unemployment_model's predictions against actual 
# female_unemployment using the WVPlots::GainCurvePlot() function.
# 
# For situations where order is more important than exact values, the gain curve helps you check if the model's 
# predictions sort in the same order as the true outcome.
# 
# Calls to the function GainCurvePlot() look like:
#   
#   GainCurvePlot(frame, xvar, truthvar, title)
# 
# where
# 
# frame is a data frame
# xvar and truthvar are strings naming the prediction and actual outcome columns of frame
# title is the title of the plot
# 
# When the predictions sort in exactly the same order, the relative Gini coefficient is 1. When the model sorts poorly, 
# the relative Gini coefficient is close to zero, or even negative.

#Do the model's predictions sort correctly?

# unemployment is in the workspace (with predictions)
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Load the package WVPlots
library(WVPlots)

# Plot the Gain Curve
GainCurvePlot(unemployment, "predictions", "female_unemployment", "Unemployment model")

#####

#Root Mean Squared Error (RMSE)

#residual vector
#err = residual vector = pred - actual
#then  RMSE = sqrt(mean(err^2))

#test or evaluate RMSE, compare it to SD (Standard Deviation) value
#RMSE vs sd(actual)

# unemployment is in the workspace
summary(unemployment)

# For convenience put the residuals in the variable res
res <- unemployment$predictions - unemployment$female_unemployment

# Calculate RMSE, assign it to the variable rmse and print it
rmse <- sqrt(mean(res^2))

# Calculate the standard deviation of female_unemployment and print it
sd_unemployment <- sd(unemployment$female_unemployment)


###

#R^Squared

#R^2 = 1 - (RSS/SStot)
#RSS = SUM(actual - predict)^2 = SUM(Residual vector)^2; the residual sum of squares
#SStot = SUM(actual - mean)^2; total sum of squares







