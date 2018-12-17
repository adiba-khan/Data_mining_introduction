#K-means
#generate random data

set.seed(5)

x <- matrix(rnorm(300), 150, 2)

xmean <- matrix(rnorm(8), 4, 2)

which <- sample(1:4, 150, replace = T)

x <- x + xmean[which, ]

plot(x, col = which, pch = 19)

#k-means algorithm complete

x.cluster <- cbind(x, which)

while(TRUE){
  centroid <- c()
for(g in 1:4) {
  centroid <- c(centroid, mean(x.cluster[x.cluster[, 3] == g, 1]), mean(x.cluster[x.cluster[, 3] == g, 2]))
  }

centroid <- matrix(centroid, 4, 2, byrow = T)
distance <- c()
for(i in 1:nrow(x)){
  for(j in 1:4){
    dis <- sqrt(sum((x[i,] - centroid[j,])^2))
    distance <- c(distance, dis)          
  }
}

distance <- matrix(distance, 150, 4, byrow = T)
centroid.label <- apply(distance, 1, which.min)
if(all(centroid.label == x.cluster[, 3])){
  km.clusters <- centroid.label
  centroid.matrix <- centroid
  break
}else{
  x.cluster[, 3] <- centroid.label
}
}

#k-means R function
#plot k-means visualization

plot(x, col = centroid.label, pch = 19)
points(centroid, pch = 19, col = 6, cex = 2)

set.seed(5)
km.out <- kmeans(x, 2) #fill in parameters using pamk
km.out
plot(x, col = km.out$cluster, pch = 19)
points(km.out$centers, pch = 19, col = 6, cex = 2)

#how do we know the optimal clustering number?
pamk.best <- pamk(x)
cat("number of clusters according to optimum average silhouette width (best separated clusters):", pamk.best$nc, "\n")
plot(pam(x, pamk.best$nc))

#Patient data
data <- read.csv(file="Cluster_data.csv")

datasub <-data[,1:2]

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

datanorm <- as.data.frame(lapply(datasub, normalize))

set.seed(5)
km.out <- kmeans(datanorm, 3)
km.out
plot(datanorm, col = km.out$cluster, pch = 19)
points(km.out$centers, pch = 19, col = 6, cex = 2)

pamk.best <- pamk(datanorm)
cat("number of clusters according to optimum average silhouette width (best separated clusters):", pamk.best$nc, "\n")
plot(pam(datanorm, pamk.best$nc))

rules_data <- data.frame(as.factor(data$Disease), as.factor(km.out$cluster))
rules <- apriori(rules_data)
inspect(rules)