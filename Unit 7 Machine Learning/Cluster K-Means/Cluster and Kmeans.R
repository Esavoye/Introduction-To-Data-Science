# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine_df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine_df)

# Exercise 2:
#   * How many clusters does this method suggest?
        # This method would suggest 3 clusters due to the bend/sharp decrease from clusters 1 to 3.
#   * Why does this method work? What's the intuition behind it?
        # After 3 clusters, the decrease drops off suggesting a 3 cluster solution. The lesser 
        # decrease after cluster 3 implies adding more clusters may not be useful even if WSS is 
        # slightly lower. The method works because it shows how adding more clusters reduces the SS within each cluster.
#   * Look at the code for wssplot() and figure out how it works
        # The code plots a function that specifies the max number of clusters and sets a seed for
        # reproducing the plot. Next, the formula for calculating within-group SS for each number of clusters is defined. 
        # Finally, thethe actual k-means method of assigning clusters is coded followed by the graph and formatting of the graph.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest? 3 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

set.seed(1234)
fit.km <- kmeans(wine_df, centers = 3, nstart = 25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(fit.km$cluster)
table(wine$Type)

# while there is more variation in wine type, I would say it is a pretty good fit

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library("cluster")
clusplot(wine_df, fit.km$cluster, main = "clusplot")

# yes. The data appears to fall within the 3 clusters with very little overlap or escape.