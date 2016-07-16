
# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine$Type <- NULL
wine_scaled <- scale(wine[-1])

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

wssplot(wine_scaled)

# Exercise 2:
#   * How many clusters does this method suggest?

#Khaled: the plot using wssplot(wine) shows a larg drop from 1-3 clusters and then the decreases slows down. I would recommend starting with 3 clusters. We could also possibly try 5 clusters since there is almost no drop off after 5 clusters, but I would begin with 3 as mentioned above. 

#   * Why does this method work? What's the intuition behind it?

# It suggests that the number of clusters that explain the sum of squares within the groups meaning that there is a smaller error with in the cluster than you would have at say using only 1 or 2 clusters.

#   * Look at the code for wssplot() and figure out how it works



# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_scaled, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# Khaled: this method suggests according to the majority rule, the best number of clusters is  3 as well similar to method 1; 15 proposed 3 as the best number of clusters 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_scaled, 3)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type, fit.km$cluster)
# overall this is great clustering with only 6 of the 178 wines mislabeled; ~96% correct. It's interesintg how all 6 were actually wine type 2.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library("cluster")
clusplot(fit.km$cluster)
clusplot(wine_scaled,as.factor(kmeans(wine_scaled,3)$cluster))

# great clustering
