
################################Analysis of Iris Data Set: EDA, Supervised and Unsupervised Learning###########################################
#load data
data("iris")

#What's in the dataset?
dim(iris) 
names(iris) 
head(iris)
str(iris)

#representation of the flowers
sum(iris$Species=="setosa")
sum(iris$Species=="virginica")
sum(iris$Species=="versicolor")


#Summary of the variables
summary(iris)
apply(iris[,1:4], 2, sd)

#Summary of data by groups
aggregate(.~Species, iris, mean) 
aggregate(.~Species, iris, median)
aggregate(.~Species, iris, sd)



################################################Data Visualization
#1. Histogram
hist(iris$Petal.Length, prob=T, col="grey", breaks=20, main="Histogram and Density of petal Length", xlim=c(1,7), xlab="Petal Length")
lines(density(iris$Petal.Length), col="red", lwd=2)
abline(v=mean(iris$Petal.Length), col="blue", lty=2, lwd=3)# Add a vertical line that indicates the average of Petal Length

#2. Histogram with density plot
ggplot(iris, aes(x=Petal.Length)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2) 
# Color by groups
ggplot(iris, aes(x = Petal.Length, color=Species, fill=Species)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+ geom_density(alpha=.2)

#3. Scatterplot
pairs(iris[,1:4])




######################################Supervised and Unsupervised Learning: Clustering Analysis#############################
###########Split the data into 80% Training and 20% Testing data set
index <- sample(x=nrow(iris), size=nrow(iris)*0.8) 
iris_train <- iris[index,]
iris_test <- iris[-index,] # the remaining 20 percent of the data




################Supervised Learning: KNN with different k values
# k =5
library(class)
knn_iris_5 <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=5)
knn_iris_5

#Prediction Accuracy- True=observed value
table(iris_test[,5], knn_iris_5, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris_5)

#misclassification rate
miserror_5 <- sum(iris_test[,5]!=knn_iris_5)/nrow(iris_test)
miserror_5 


# k =10
library(class)
knn_iris_10 <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=10)
knn_iris_10

#Prediction Accuracy- True=observed value
table(iris_test[,5], knn_iris_10, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris_10)

#misclassification rate
miserror_10 <- sum(iris_test[,5]!=knn_iris_10)/nrow(iris_test)
miserror_10 


#k =100
library(class)
knn_iris_100 <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=100)
knn_iris_100

#Prediction Accuracy- True=observed value
table(iris_test[,5], knn_iris_100, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris_100)

#misclassification rate
miserror_100 <- sum(iris_test[,5]!=knn_iris_100)/nrow(iris_test)
miserror_100


##############################Unsupervised Learning: K-means clustering with different k values
library(fpc)
par(mfrow=c(1,2))

#k=3
fit <- kmeans(iris[,1:4], 3)
plotcluster(iris[,1:4], fit$cluster, main="K-means clustering with k=3")

#k=5
fit <- kmeans(iris[,1:4], 5)
plotcluster(iris[,1:4], fit$cluster, main="K-means clustering with k=5")


###############################Unsupervised Learning: Hierachical clustering
par(mfrow=c(1,2))
hc_result <- hclust(dist(iris[,1:4]))
plot(hc_result, main="Hierarchical clustering with k=3")
rect.hclust(hc_result, k=3) #Cut Dendrogram into 3 Clusters

hc_result <- hclust(dist(iris[,1:4]))
plot(hc_result, main="Hierarchical clustering with k=5")
rect.hclust(hc_result, k=5) #Cut Dendrogram into 5 Clusters

