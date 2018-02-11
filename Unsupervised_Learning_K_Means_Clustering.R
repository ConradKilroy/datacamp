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
#number of centers
#number of iterations
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



#---------------------------------
#Pokemon practice
#datafile: pokemon.csv
pokemon <- read.csv("data/pokemon.csv", header = TRUE, sep = ",")
# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#plot the slope of the scree plot to see the elbow better
x <- as.data.frame(cbind(c(1:15), wss))
#x$asdf <- x$V1
len_x <- dim(x)[1]
#x[len_x,] - x[len_x - 1,] 

a <- 0
for (i in len_x:2)
{
  y <- (x[i,] - x[i-1,])
  a <- rbind(a, y)
}
a <- a[2:len_x,]
#remove V1 colum
a <- within(a, rm(V1))
a$asdf <- row.names(a)

# Produce a scree plot differential slope
plot(a$asdf, a$wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Slope of Within groups sum of squares")

#now that we know what's the optimal cluster value, use it, in this case its 2

# Select number of clusters
k <- 2

# Build model with k clusters: km.out
km.out <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")

#########################
#Hierarchical clustering
#######################

#define x
#x <-

# Create hierarchical clustering model: hclust.out
hclust.out <- hclust(dist(x))

# Inspect the result
summary(hclust.out)

############
#cutting at a height (h) or by number of clusters (k)

# Cut by height
cutree(hclust.out, h = 7)

# Cut by number of clusters
cutree(hclust.out, k = 3)

########complete the hcluster by method, automatically#############
# Cluster using complete linkage: hclust.complete
hclust.complete <- hclust(dist(x), method = "complete")
# Cluster using average linkage: hclust.average
hclust.average <- hclust(dist(x), method = "average")
# Cluster using single linkage: hclust.single
hclust.single <- hclust(dist(x), method = "single")

# Plot dendrogram of hclust.complete
plot(hclust.complete, main = "Complete")
# Plot dendrogram of hclust.average
plot(hclust.average, main = "Average")
# Plot dendrogram of hclust.single
plot(hclust.single, main = "Single")

#Whether you want balanced or unbalanced trees for your hierarchical clustering
#model depends on the context of the problem you're trying to solve. 
#Balanced trees are essential if you want an even number of observations assigned 
#to each cluster. On the other hand, if you want to detect outliers, for example,
#an unbalanced tree is more desirable because pruning an unbalanced tree can result
#in most observations assigned to one cluster and only a few observations assigned to other clusters. 


#######################

#let return to Pokemon data
#sometimes the date has a high deviation variation spread from the average mean. At times its necessary to scale the data appropriately.

# View column means
colMeans(pokemon)

# View column standard deviations
apply(pokemon, 2, sd)

# Scale the data!!!!
pokemon.scaled <- scale(pokemon)

# Create hierarchical clustering model: hclust.pokemon
hclust.pokemon <- hclust(dist(pokemon.scaled), method = "complete")
#notice how standard deviation varies highly around the means(avg), so we scaled it!

##################################
#remember in hieraacle clustering trees, linkages that create the most balance trees are "complete" & "average"

# Apply cutree() to hclust.pokemon: cut.pokemon
#km.pokemon
#hclust.pokemon

cut.pokemon <- cutree(hclust.pokemon, 3)

# Compare methods
table(km.pokemon$cluster, cut.pokemon)

# Looking at the table, it looks like the hierarchical clustering model 
#assigns most of the observations to cluster 1, while the k-means algorithm 
#distributes the observations relatively evenly among all clusters. 
#It's important to note that there's no consensus on which method produces
#better clusters. The job of the analyst in unsupervised clustering is to 
#observe the cluster assignments and make a judgment call as to which method 
#provides more insights into the data. Excellent job! 

