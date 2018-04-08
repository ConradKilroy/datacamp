url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

wisc.df <- read.csv(url)
#convert into numerical matrix using col 3 to 32
wisc.data <- as.matrix(wisc.df[,3:32])

row.names(wisc.data) <- wisc.df$id

diagnosis <- as.numeric(wisc.df$diagnosis == 'M')

#Exploratory
#how many cols have '_mean'?
length(grep("_mean", colnames(wisc.df)))

#how many diagnosis are Malignant?
table(diagnosis)
