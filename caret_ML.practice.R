#install.packages("caret")
library(caret)

#get the dataset
dataset <- iris

####do feature engineering on the data set here


#####

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]


##########exploratory data analysis############
sapply(dataset, class)

#numeric col for paris multiplot
my_pairs = dataset[,1:4]
#factor color colum
factor_color = dataset[,5]


#pairs multi-scatterplot for numerics 
pairs(my_pairs)


featurePlot(x=my_pairs, 
            y=factor_color, 
            plot = "ellipse")  #cluster ellipse

featurePlot(x=my_pairs, 
            y=factor_color, plot="box")

#Kernel Probability Density plot
my_scales <-list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=my_pairs, 
            y=factor_color, plot="density", scales = my_scales)

#yet another Kernel Probability Density Plot!!!!!!!!!!!!!!!!!
install.packages("GGally")
library(GGally)
ggpairs(my_pairs, aes(alpha = 0.4))

##########################
#ML caret R model buidling

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


#view ML results
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)

# summarize Best Model
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
