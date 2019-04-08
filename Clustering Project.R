# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
winedf <- wine[-1]
scale(winedf)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(winedf, nc=15, seed=1234){
	              wss <- (nrow(winedf)-1)*sum(apply(winedf,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(winedf, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(winedf)

# Exercise 2:
#   * How many clusters does this method suggest? 
#         2 clusters
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(winedf, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest? 2 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(winedf, 2)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(fit.km$cluster, wine$Type)

# Since the number of clusters determined was based on the number of criteria taken into account,
# two clusters took into account the most criteria. Wine types 2 and 3 fall mostly into 
# the second cluster, indicating that the criteria accounted for in these two clusters finds that
# wine types 2 and 3 share many similarities. But there are
# also other criterias that can cluster the data differently. I am not sure if I would say this is
# good clustering as it lumps the majority of two types of wine into one cluster, without
# possibly taking into account the differences that set those wines apart.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine, fit.km$cluster)

# The clusplot explains 57.38% of the point variability, which is okay.
# There is a lot of overlap between the points.