# high dimensional analysis
library(dplyr)
library(ggplot2)
library(Rtsne)

setwd("C:/Users/Owner/Downloads/Kelly Training Data/Workout Files/AllFIT")
garmin.data <- readRDS("GarminDataClean1.2.rda")#5932430 obs of 54 variables
# smaller, NA free dataset
garmin.subset <- garmin.data[,c(2,4,7,9:15)]
garmin.subset <- garmin.subset[unique(complete.cases(garmin.subset)),]

# T-SNE

## for plotting
colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)

## Executing the algorithm on curated data
garmin.tsne <- Rtsne(garmin.subset, dims = 2, perplexity=40, verbose=TRUE, max_iter = 200, check_duplicates = FALSE)

## Plotting
plot(garmin.tsne$Y, t='n', main="tsne")
text(garmin.tsne$Y, labels=train$label, col=colors[train$label])



########## K-means
#it's rare I just have numerical data like this...
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster) 

# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results
summary(fit) # display the best model 

# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster) 
