
#Fitting a neural network in R; neuralnet package
#source
#https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
#https://www.youtube.com/watch?v=LTg-qP9iGFY
#https://rpubs.com/julianhatwell/annr

#install.packages('neuralnet')

suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(neuralnet))


#set.seed(500)
Dataframe <- Boston
help("Boston")
str(Dataframe)

#with 13 variables
#prediction target goal: Dataframe$medv

#intital review
hist(Dataframe$medv)
dim(Dataframe)
head(Dataframe,3)

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
#rand sample index set for training
ind <- sample(1:nrow(DataFrameScaled), 400)
#split for training DF and test DF
trainDF <- DataFrameScaled[ind,]
testDF <- DataFrameScaled[-ind,]




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
predictorVarNames_col_size <- length(predictorVarNames)
#int this case 13 variabels

#create forumala!
(formula = as.formula(
                     paste0(goalVarName, " ~ ", 
                                                paste0(predictorVarNames,collapse = ' + ')
                            )
                    )
)


neuralModel <- neuralnet(formula = formula,
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
unscaling_method = (max(DataFrameScaled$medv) - min(DataFrameScaled$medv)) + min(DataFrameScaled$medv)

predictions.nn <- predictions$net.result * unscaling_method
actualValues <-              testDF$medv * unscaling_method

#mean squared error (MSE) as a measure of how much our predictions are far away from the real data.
the_residual <- actualValues - predictions.nn
(MSE.nn <- sum((the_residual)^2)/nrow(testDF))


(RMSE <- sqrt(mean(the_residual^2)))


#make simple linear model

lm.fit <- glm(medv~., data=trainDF)
summary(lm.fit)
pr.lm <- predict(lm.fit, testDF)
(MSE.lm <- sum((pr.lm - testDF$medv)^2)/nrow(testDF))


par(mfrow=c(1,2))

plot(testDF$medv,
     predictions.nn,
     col='red',
     main='Boston medv \n Real vs Predicted Neural Network Model',
     pch=18,cex=0.7,
     xlab = "Actual",
     ylab = "Predicted NN")
abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red', bty='n')


plot(testDF$medv,
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
plot(testDF$medv,
     predictions.nn,
     col='red',
     main='Boston medv \n Real vs Predicted Neural Network Model & Linear Model',
     pch=18,cex=0.7,
     xlab = "Actual",
     ylab = "Predictions")
points(testDF$medv,       
       pr.lm,
       col='blue',
       pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


#cross validation on NN in R
#https://www.youtube.com/watch?v=p5rDg4OVBuA
