#Preparing the data

#Unlike prior chapters, where we prepared the data for you for unsupervised learning, 
#the goal of this chapter is to step you through a more realistic and complete workflow.

#Recall from the video that the first step is to download and prepare the data.


url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

wisc.df <- read.csv(url)
#convert into numerical matrix using col 3 to 32
wisc.data <- as.matrix(wisc.df[,3:32])

row.names(wisc.data) <- wisc.df$id

diagnosis <- as.numeric(wisc.df$diagnosis == 'M')




#Exploratory##################
#how many cols have '_mean'?
length(grep("_mean", colnames(wisc.df)))

#how many diagnosis are Malignant?
table(diagnosis)

####################
#Performing PCA

#The next step in your analysis is to perform PCA on wisc.data.

#You saw in the last chapter that it's important to check if the data need to be scaled before performing PCA. 
#Recall two common reasons for scaling data:

#The input variables use different units of measurement.
#The input variables have significantly different variances.

#review for PCA
my_col_means <- colMeans(wisc.data)

#std dev
my_std_dev <- apply(X = wisc.data, 2, sd)

#notice how spread the colmeans are, therefore we should scale when applying PCA.
#plot(my_col_means, my_std_dev, xlab = "averages", ylab = "std dev") 


#create PCA
wisc.pr <- prcomp(wisc.data, center = TRUE, scale = TRUE)

summary(wisc.pr)

#create biplot of PCA
biplot(wisc.pr)

###################

#Interpreting PCA results

#Now you'll use some visualizations to better understand your PCA model. You were introduced to one of these visualizations,
#the biplot, in an earlier chapter.

#You'll run into some common challenges with using biplots on real-world data containing a non-trivial number of observations
#and variables, then you'll look at some alternative visualizations. You are encouraged to experiment with additional 
#visualizations before moving on to the next exercise.

#Create a biplot of the wisc.pr data. What stands out to you about this plot? Is it easy or difficult to understand? Why?
#Execute the code to scatter plot each observation by principal components 1 and 2, coloring the points by the diagnosis.

plot(wisc.pr$x[, c(1, 2)],  #PCA 1 and 2
     col = (diagnosis + 1), #note c(0,1) binary is white & black
                            #therefore c(0,1) + 1, changes the colors to red & black
     xlab = "PC1", ylab = "PC2")


plot(wisc.pr$x[, c(1, 3)],  #PCA 1 and 3
     col = (diagnosis + 1), #note c(0,1) binary is white & black
     #therefore c(0,1) + 1, changes the colors to red & black
     xlab = "PC1", ylab = "PC3")

# Do additional data exploration of your choosing below (optional)
plot(wisc.pr$x[, c(1, 25)],  #PCA 1 and 25
     col = (diagnosis + 1), #note c(0,1) binary is white & black
     #therefore c(0,1) + 1, changes the colors to red & black
     xlab = "PC1", ylab = "PC25")

#ANS: Because principal component 2 explains more variance in the original data than principal component 3,
#you can see that the first plot has a cleaner cut separating the two subgroups. 


########produce Scree plots########

#Variance explained

#In this exercise, you will produce scree plots showing the proportion of variance explained as the number of principal 
#components increases. The data from PCA must be prepared for these plots, as there is not a built-in function in R to 
#create them directly from the PCA model.

#As you look at these plots, ask yourself if there's an elbow in the amount of variance explained that might lead you to 
#pick a natural number of principal components. If an obvious elbow does not exist, as is typical in real-world datasets, 
#consider how else you might determine the number of principal components to retain based on the scree plot.

# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

#What is the minimum number of principal components needed to explain 80% of the variance in the data?
#Scree plot elbow is at 4, so the answer is 4 PCA needed to explain 80%

#######################


#Communicating PCA results
#This exercise will check your understanding of the PCA results, in particular the loadings and variance explained. 
#The loadings, represented as vectors, explain the mapping from the original features to the principal components. 
#The principal components are naturally ordered from the most variance explained to the least variance explained.

#The variables you created before—wisc.data, diagnosis, wisc.pr, and pve—are still available.

#For the first principal component, what is the component of the loading vector for the feature concave.points_mean? 
#What is the minimum number of principal components required to explain 80% of the variance of the data?

wisc.pr$rotation["concave.points_mean", "PC1"]
#ANS: -0.2608538
#ANS: 5

#########Hierarchical clustering of case data###########

#The goal of this exercise is to do hierarchical clustering of the observations. Recall from Chapter 2 that this type of
#clustering does not assume in advance the number of natural groups that exist in the data.

#As part of the preparation for hierarchical clustering, distance between all pairs of observations are computed. 
#Furthermore, there are different ways to link clusters together, with single, complete, and average being the most common linkage methods.

# Scale the wisc.data data: data.scaled
data.scaled <- scale(wisc.data)

# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust <- hclust(data.dist, method = "complete")

######Results of hierarchical clustering

#Let's use the hierarchical clustering model you just created to determine a height (or distance between clusters)
#where a certain number of clusters exists. 
par(mfrow = c(1, 1))
plot(wisc.hclust)
#Using the plot() function, what is the height at which the clustering model has 4 clusters?
#ANS:20

###Selecting number of clusters

# In this exercise, you will compare the outputs from your hierarchical clustering model to the actual diagnoses. 
# Normally when performing unsupervised learning like this, a target variable isn't available. We do have it with this 
# dataset, however, so it can be used to check the performance of the clustering model.
# 
# When performing supervised learning—that is, when you're trying to predict some target variable of interest and that
# target variable is available in the original data—using clustering to create new features may or may not improve the
# performance of the final model. This exercise will help you determine if, in this case, hierarchical clustering provides 
# a promising new feature.

# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters <- cutree(wisc.hclust, h=20)


# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)

#Four clusters were picked after some exploration. Before moving on, you may want to explore how different numbers of clusters
#affect the ability of the hierarchical clustering to separate the different diagnoses.


#########k-means clustering and comparing results#####

#As you now know, there are two main types of clustering: hierarchical and k-means.

#In this exercise, you will create a k-means clustering model on the Wisconsin breast cancer data and compare the results
#to the actual diagnoses and the results of your hierarchical clustering model. Take some time to see how each clustering model
#performs in terms of separating the two diagnoses and how the clustering models compare to each other.

# Create a k-means model on wisc.data: wisc.km
wisc.km <- kmeans(scale(wisc.data), center=2 , nstart = 20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)
#How well does k-means separate the two diagnoses?

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)
#Looking at the second table you generated, it looks like clusters 1, 2, and 4 from the hierarchical clustering model can be 
#interpreted as the cluster 1 equivalent from the k-means algorithm, and cluster 3 can be interpreted as the cluster 2 equivalent. 


###########Clustering on PCA results

#In this final exercise, you will put together several steps you used earlier and, in doing so, you will experience some of 
#the creativity that is typical in unsupervised learning.

#Recall from earlier exercises that the PCA model required significantly fewer features to describe 80% and 95% of the 
#variability of the data. In addition to normalizing data and potentially avoiding overfitting, PCA also uncorrelates the variables, sometimes improving the performance of other modeling techniques.

#Let's see if PCA improves or degrades the performance of hierarchical clustering.


# Create a hierarchical clustering model: wisc.pr.hclust
#targe at least 90% of variabliity of the data

summary(wisc.pr)
#we see that in cumulative Proportion of Variance is for ~ 90% is PC1 though PC7
#thus make a vector col of wisc.pr$x [ , c(1,7)]

wisc.pr.hclust <- hclust(dist(wisc.pr$x[, c(1,7)]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k =4)

# Compare to actual diagnoses
table(wisc.pr.hclust.clusters, diagnosis)

# Compare to k-means and hierarchical
table(wisc.pr.hclust.clusters, wisc.km$cluster)

