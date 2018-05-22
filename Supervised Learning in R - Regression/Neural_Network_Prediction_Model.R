
#Fitting a neural network in R; neuralnet package
#source
#https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
#https://datascienceplus.com/fitting-neural-network-in-r/
#https://gist.github.com/mick001/49fad7f4c6112d954aff

#https://www.youtube.com/watch?v=LTg-qP9iGFY

#install.packages('neuralnet')

suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(neuralnet))


set.seed(500)
Dataframe <- Boston
help("Boston")
str(Dataframe)

#fit for Linear Model (LM)


#with 13 variables
#prediction target goal: Dataframe$medv

#intital review
hist(Dataframe$medv)
dim(Dataframe)
head(Dataframe,10)

#examine ranges for each variable
apply(Dataframe,2, range)
        #crim  zn indus chas   nox    rm   age     dis rad tax ptratio  black lstat medv
#[1,]  0.00632   0  0.46    0 0.385 3.561   2.9  1.1296   1 187    12.6   0.32  1.73    5
#[2,] 88.97620 100 27.74    1 0.871 8.780 100.0 12.1265  24 711    22.0 396.90 37.97   50

#capture the min and max values
maxVal <- apply(Dataframe,2, max)
minVal <- apply(Dataframe,2, min)

DataFrameScaled <- as.data.frame(
                                scale(Dataframe, 
                                      center = minVal, 
                                      scale = maxVal - minVal)
                                )
#random sample index set for training
sample_size <- 400
indices <- sample(1:nrow(DataFrameScaled), sample_size)
#split for training DF and test DF
trainDF <- DataFrameScaled[indices,]
testDF <- DataFrameScaled[-indices,]




##Lets take some configuration for neuraL network
##say 13-4-2-1
##So number of hidden Layes=2
##input Layer had 10 units

##We need this as formuLa
##medv ~ crim + zn + indus • chas + nox + rm + age + dis + rad + tax + ptratio • black + lstat

allVarnames <- colnames(DataFrameScaled)

goalVarName <- "medv"
#exclude goalname, use the others as predictor variables
predictorVarNames <- allVarnames[!allVarnames %in% goalVarName]
predictorVarNames_col_size <- length(predictorVarNames) #13
#int this case 13 variabels

#create forumala!
(formula.nn = as.formula(
                     paste0(goalVarName, " ~ ", 
                                                paste0(predictorVarNames,collapse = ' + ')
                            )
                    )
)


neuralModel <- neuralnet(formula = formula.nn,
                         data = trainDF,
                         #hidden layers
                         hidden = c(5,3), 
                         linear.output = TRUE)

plot(neuralModel)
#black arrows = bias
#blue arrows  = weights

#The black lines show the connections between each layer and the weights on each connection while the blue lines show the bias term added in each step. The bias can be thought as the intercept of a linear model.
#The net is essentially a black box so we cannot say that much about the fitting, the weights and the model. Suffice to say that the training algorithm has converged and therefore the model is ready to be used.



#Predict for test data set
#predictions <- compute(neuralModel, testDF[,1:13])
predictions <- compute(neuralModel, testDF[,1:predictorVarNames_col_size])
str(predictions)

#unpacking
#unscaling_method = (max(Boston$medv) - min(Boston$medv)) + min(Boston$medv)
unscaling_method = (max(Dataframe$medv) - min(Dataframe$medv)) + min(Dataframe$medv)
#unscaling_method = (max(DataFrameScaled$medv) - min(DataFrameScaled$medv)) + min(DataFrameScaled$medv)
#unscaling_method = (max(testDF$medv) - min(testDF$medv)) + min(testDF$medv)

predictions.nn <- predictions$net.result * unscaling_method
actualValues <-              testDF$medv * unscaling_method

#mean squared error (MSE) as a measure of how much our predictions are far away from the real data.
the_residual <- actualValues - predictions.nn
(MSE.nn <- sum((the_residual)^2)/nrow(testDF))


(RMSE <- sqrt(mean(the_residual^2)))

#------------------------------------

#make simple linear model
# Train-test random splitting for linear model
#indices <- sample(1:nrow(Dataframe),round(0.75*nrow(Dataframe)))
train_ <- Dataframe[indices,]
test_ <- Dataframe[-indices,]

#make simple linear model
lm.fit <- glm(medv~., data=train_)
summary(lm.fit)
pr.lm <- predict(lm.fit, test_)
(MSE.lm <- sum((pr.lm - test_$medv)^2)/nrow(test_))

#-----------------------

par(mfrow=c(1,2))

plot(actualValues,
     predictions.nn,
     col='red',
     main='Boston medv \n Real vs Predicted Neural Network Model',
     pch=18,cex=0.7,
     xlab = "Actual",
     ylab = "Predicted NN")
abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red', bty='n')


#note: no scaling here
plot(test_$medv,
     pr.lm,
     col='blue',
     main='Boston medv \n Real vs Predicted Linear Model',
     pch=18, cex=0.7,
     xlab = "Actual",
     ylab = "Predicted LM")
abline(0,1,lwd=2)
#legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

###
par(mfrow=c(1,1))
plot(testDF$medv*unscaling_method,
     predictions.nn,
     col='red',
     main='Boston medv \n Real vs Predicted Neural Network Model & Linear Model',
     pch=18,cex=0.7,
     xlab = "Actual",
     ylab = "Predictions")
points(test_$medv,       
       pr.lm,
       col='blue',
       pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

#The data set is partitioned in 2 parts: Training set and Testing set.
#then the model, derived from the training set, predicts the target variable for testing set.

#lets doo Cross validation for LM first
library(boot)
set.seed(200)

# Linear model cross validation
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

#--------------------------
# Neural net cross validation
#K-fold cross validation
set.seed(450)
cv.error <- NULL
k <- 10

# Initialize progress bar
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  #sample function does the random picking of indexes
  index <- sample(1:nrow(Dataframe),round(0.9*nrow(Dataframe)))
  train.cv <- DataFrameScaled[index,]
  test.cv <- DataFrameScaled[-index,]
  
  nn <- neuralnet(formula.nn,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*unscaling_method
  
  test.cv.r <- (test.cv$medv)*unscaling_method
  
  #collect iterations of MSE
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

# Average MSE
mean(cv.error)
summary(cv.error)

# MSE vector from CV
cv.error

# Visual plot of CV results
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)




#https://www.youtube.com/watch?v=p5rDg4OVBuA
