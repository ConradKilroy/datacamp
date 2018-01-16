#Unsupervised Learning in R
#via Datacamp


#kMeans clustering
my_data1 <- data.frame(x = rnorm(100, mean = 2),
                       y = rnorm(100, mean =2))

my_data2 <- data.frame(x = rnorm(100, mean = -1),
                       y = rnorm(100, mean = -1))

my_data <- rbind(my_data1, my_data2)
rm(my_data1, my_data2)

plot(my_data)

########
set.seed(1) #VERY IMPORTANT for repeatability, cluster colors wont change!!!!
# Create the k-means model: km.out
km.out <- kmeans(x = my_data, centers = 3, nstart = 20)

# Inspect the result
#summary(km.out)

# Print the cluster membership component of the model
print(km.out$cluster)

# Print the km.out object
print(km.out)


###Visualize Data
plot(my_data, col=km.out$cluster)
plot(my_data, col=km.out$cluster, main = "k-means with 3 clusters", xlab ="", ylab="")

#######

#run it 6 times, visualize them
# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(1)

#forloop for the 2 x 3 plotting grid
for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(my_data, 3, nstart = 1)
  
  # Plot clusters
  plot(my_data, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}

#UNKNOWN number of clusters apriori?
#what if you don't the number clusters apriori?
#here's how you determine it.
#do iteration of num of clusters x Total sum of squares plot
#its called a scree plot
#notice the elbow trend, that's when you've determined the number of clusters.

# Initialize total within sum of squares error: wss
wss <- 0

#lets run up to 15 clusters
# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(my_data, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
par(mfrow = c(1, 1))
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares",
     main = "Scree Plot")



###SCREE PLOT, the point when the number of clusters
