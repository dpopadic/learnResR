# Unsupervised Learning in R
# --------------------------


# 1. Unsupervised Learning
# ------------------------

# Three major types of ML: 
# 1) unsupervised learning (finding structure in unlabeled data) 
# 2) supervised learning (making predictions) 
# 3) reinforcement learning
# focus here on unsupervised learning..

# 2 major goals: 
#   a. finding homogeneous subgroups within a larger group (clustering)
#   b. find patterns in the features of the data (dimensionality reduction & pre-processing before supervised learning)

# challenges and benefits:
# - no single goal of analysis
# - requires more creativity
# - much more unlabeled data available than cleanly labeled data


# k-means clustering algorithm
# ---
# breaks observations into pre-defined number of clusters
# 2 parameters: k centers (starting clusters) & random component to run algo multiple times to improve odds of best model (nstart=20)


x <- cbind(rnorm(300, 0, 1), rpois(300, lambda = 10) * (rbinom(300, 1, 0.5) * 2 - 1))
plot(x)
# k-means model:
km_out <- kmeans(x, centers = 3, nstart = 20)
summary(km_out)
# get cluster membership component..
print(km_out$cluster)
# print the km.out object..
print(km_out)
plot(x, col = km_out$cluster, main = "k-means with 3 clusters", xlab = "", ylab = "")

# determining # clusters:
# ---
# if you don't know # clusters beforehand, there's a heuristic method to determine # clusters 
# beforehand: total within SSE vs # clusters ("scree-plot")
wss <- 0
# for 1 to 15 cluster centers
for (i in 1:15) {
  km_out <- kmeans(x, centers = i, nstart = 20)
  # save total within sum of squares to wss variable
  wss[i] <- km_out$tot.withinss
}

# plot total within sum of squares vs. number of clusters..
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
# set k equal to the number of clusters corresponding to the elbow location..
k <- 2

# real-world example..
pokemon <- read.csv("data/Pokemon.csv")
pokemon <- pokemon[ ,c(6:11)]

# challenges: 
# - selecting the variables to cluster upon
# - scaling the data
# - determining the number of clusters
# - visualize the results for interpretation

wss <- 0
# look over 1 to 15 possible clusters
for (i in 1:15) {
  km_out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50) 
  # iter.max is repeating over and over until some stopping criterion is met (default # iterations is 10 -> change to higher in practice)
  wss[i] <- km_out$tot.withinss
}
# scree plot..
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
# select number of clusters..
k <- 3
# build model with k clusters: km.out
km_out <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)
# plot of Defense vs. Speed by cluster membership..
plot(pokemon[, c("Defense", "Speed")], 
     col = km_out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", 
     ylab = "Speed")






# 2. Hierarchical Clustering
# --------------------------

# numer of clusters isn't known ahead of time
# 2 approaches: bottom-up & top-down (this course focuses on bottom-up)
# bottom-up: start with # clusters = # observations & group closest clusters together until just one cluster left
# similarity is calculated as euclidean distance between observations: dist(x) -> hclust(dist(x)) returns hierarchical clustering model

# create hierarchical clustering model: hclust.out
hclust_out <- hclust(dist(x))
summary(hclust_out)

# selecting number of clusters
# ---
# distance between 2 clusters is represented by the height of the dendogram
plot(hclust_out)
abline(h = 6, col = "red")
# cut tree by height..
cutree(hclust_out, h = 6)
# cut by number of clusters..
cutree(hclust_out, k = 3)

# linking clusters in hierarchical clustering
# How is distance between clusters determined? Rules? -> 4 methods
# a. complete: pairwise similarity between all observations in cluster 1 & cluster 2, and uses largest of similarities
# b. single: same as a. but uses smallest of similarities
# c. average: same as a. but uses average of similarities
# d. centroid: find centroid of cluster 1 and centroid of cluster 2, and uses similarity between 2 centroids
# -> complete & average most commonly used (single can produce unbalanced trees & centroid can create inversions which is undesired)
# An unbalanced tree may be desirable if you want to detect outliers because pruning an unbalanced tree can result in most observations
# assigned to one cluster and only a few observations assigned to other clusters

# scale data before clustering..
pokemon_scaled <- scale(pokemon)
hclust_pokemon <- hclust(dist(pokemon_scaled), method = "complete")
plot(hclust_pokemon)

# compare k-means vs hierarchical clustering..
cut_pokemon <- cutree(hclust_pokemon, k=3)
table(km_out$cluster, cut_pokemon)




# 3. Dimensionality Reduction
# ---------------------------
# a popular method is PCA
# 3 goals when finding lower dimensional representation of features:
# a. find linear combination of variables to create principal components
# b. maintain most variance in the data
# c. principal components are uncorrelated (i.e. orthogonal to each other)

pr_out <- prcomp(pokemon, scale = TRUE) # recommended to always scales & center!!
summary(pr_out)
# pr.out$rotation: the directions of the principal component vectors in terms of the original features/variables. This information 
# allows you to define new data in terms of the original principal components
# pr.out$x: the value of each observation in the original dataset projected to the principal components

# visualizing and interpreting PCA results: bi-plot & scree-plot
# which two original variables have approximately the same loadings in the first two principal components?
biplot(pr_out) # hitpoints & attack
# scree-plot..
# Variability of each principal component
pr_var <- pr_out$sdev^2
# variance explained by each principal component
pve <- pr_var / sum(pr_var)
plot(cumsum(pve), 
     xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), 
     type = "b")

# practical issues:
# a. scaling data
# b. missing values (drop observations or impute/estimate missing values)
# c. categorical data (don't use or encode categorical features as numbers)





# 4. Case Study
# -------------
# complete analysis using unsupervised learning to:
# - add steps not covered before (eg. preparing data, selecting good features for supervised learning)
# - emphasize creativity

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"
wisc_df <- read.csv(url)
wisc_data <- as.matrix(wisc_df[, 3:32])
row.names(wisc_data) <- wisc_df$id
# diagnosis vector..
diagnosis <- as.numeric(wisc_df$diagnosis == "M")

# should the data be scaled?
colMeans(wisc_data)
apply(wisc_data, 2, sd)
# pca..
wisc_pr <- prcomp(wisc_data, scale = TRUE)
summary(wisc_pr)
# scatter plot observations by components 1 and 2/3..
plot(wisc_pr$x[, c(1, 3)], 
     col = (diagnosis + 1), 
     xlab = "PC1", 
     ylab = "PC2")
# ..because PC 2 explains more variance in the original data than PC3, you see that the 1st plot has a clearer cut seperating the 2 subgroups

# communicating PCA results..
# wisc.pr$rotation: loadings, respresented as vectors, explain the mapping from the original features to the princpial components

# model comparison..
data_scaled <- scale(wisc_data)
# hierarchical..
data_dist <- dist(data_scaled)
wisc_hclust <- hclust(data_dist, method = "complete")
wisc_hclust_clusters <- cutree(wisc_hclust, k = 4) # a tree with 4 clusters
# compare cluster membership to actual diagnoses..
table(diagnosis, wisc_hclust_clusters) # ..how different number of clusters affect ability of HC to seperate the different diagnoses
# kmeans..
wisc_km <- kmeans(data.dist, centers = 2, nstart = 20)
table(wisc_km$cluster, diagnosis)
# kmeans vs hierarchical..
table(wisc_km$cluster, wisc_hclust_clusters)

# create a hierarchical clustering model using minimum # PC's that describe at least 90% of variability of data..
# note: in addition to normalizing data and potentially avoiding overfitting, PCA also uncorrelates the variables, 
# sometimes improving the performance of other modeling techniques.
wisc_pr_hclust <- hclust(dist(wisc_pr$x[, 1:7]), method = "complete")
wisc_pr_hclust_clusters <- cutree(wisc_pr_hclust, k = 4)
# comparison..
table(wisc_pr_hclust_clusters, diagnosis)
table(wisc_km$cluster, diagnosis)
table(wisc_hclust_clusters, diagnosis)









