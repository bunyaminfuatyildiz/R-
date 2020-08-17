# Download wholesale data from uci 
datacustomer  <- read.csv("C:/Users/bunya/Desktop/datas/Wholesale customers data.csv", header = TRUE, sep = ",")
datacustomer
# since the clustering algo will not use in categoric files, we must cancel out channel and  region variables
# that uses first and second column.
datacustomer <- datacustomer[3:8]
summary(datacustomer) # checking for looking information regarding data
#In the data set, the data may show different distributions within themselves, normalization should be done in order for the application to produce more efficient results.
library(clusterSim)
datacustomer$Fresh <- data.Normalization(datacustomer$Fresh, type = "n4", normalization = "column")

#The normalization process can be applied one by one to each attribute field with the above code line, 
# or it can be applied at once with the code block below.
library(clusterSim)
normFunc <-data.Normalization(datacustomer, type="n4", normalization = "column")
datacustomerN<-as.data.frame(lapply(1, function(x) normFunc))

#In the above code block, firstly, normalization function is assigned to normFunc variable,
#then it is applied to the whole data set with the help of lapply () function.
#The parameters of the data.Normalization () function indicate that the data set will be normalized, 
#the normalization type (where "n4" is min-max normalization) and the normalization will be applied to the columns.
#lapply () allows the same line of code to be executed multiple times. After normalization data set was kept in datacustomerN variable.





#Creating the k-means model
library(clusterCrit)
library(cluster)
# example code kmeans(x, centers, iter.max = 10, nstart = 1)
# x: is data set , centers is the number of sets to be obtained, iter.max Algorithm Terminates if there is no change in observed clusters in the new iteration; nstart = beginning solutions.
set.seed(1)
k_model <- kmeans(datacustomerN, 3, iter.max = 1000, nstart = 1)
#It was stated that in clustering methods, the inner cluster distance should be at least and the distance between clusters should be maximum.
#Different distance measures can be measured with the amap package.
library(amap)
k_model<-Kmeans(datacustomerN, 3, iter.max = 1000, nstart = 1, method = "manhattan")


#fuzzy c-means application 
# in example cmeans(x, centers, iter.max = 10, dist = "euclidean", verbose=FALSE, method = "cmeans")
# verbose It must be specified as True to obtain outputs while learning.

library(e1071)
set.seed(1)
c_model<- cmeans(datacustomerN, 3 , iter.max = 1000, dist = "euclidean", verbose = FALSE, method = "cmeans")
c_model


## *** EVALUATING THE MODEL PERFORMANCE ***
# set up the packages cluster, fpc, dunn, clusterSim , clValid
library(cluster)
library(fpc)
library(clValid)
library(clusterSim)
#calculate silhoutte index


distance <- dist(datacustomerN, method = "euclidean")
sil_index_k_model<- silhouette(k_model$cluster, distance)
sil_index_k_model
plot(sil_index_k_model)



distance2 <- dist(datacustomerN, method = "euclidean")
sil_index_c_model <- silhouette(k_model$cluster, distance2)
sil_index_c_model
plot(sil_index_c_model)


# Dunn index

library(clValid)
distance<-dist(datacustomerN, method = "euclidean")
dunn_index_k_model <- dunn(distance, k_model$cluster, datacustomerN, method ="euclidean")

library(clValid)
distance2<-dist(datacustomerN, method = "euclidean")
dunn_index_c_model <- dunn(distance2, k_model$cluster, datacustomerN, method ="euclidean")

#calisnki-harabasz index
library(fpc)
CHI_k_model<-calinhara(datacustomerN, k_model$cluster)
CHI_c_model<-calinhara(datacustomerN, c_model$cluster)

# Davies bouldin index
library(clusterSim)
DBI_k_model <- index.DB(datacustomerN, k_model$cluster, p = 2) # p=2 euclidean distance if 1 it is manhattan
DBI_c_model <- index.DB(datacustomerN, c_model$cluster, p = 2) # p=2 euclidean distance if 1 it is manhattan

#xie-beni index 

# intCriteria(traj,part,crit) #traj needs matrix form of data,  part clustering vector, crit evaluation criteria
library(clusterCrit)
datacustomerN_matrix <- as.matrix(datacustomerN)
XBI_k_model<- intCriteria(datacustomerN_matrix, k_model$cluster, "Xie_Beni") 

XBI_k_model<- intCriteria(datacustomerN_matrix, k_model$cluster, c("Xie_Beni", "Tau")) 
XBI_k_model

#intCriteria have many options better using at once like below and not waste time

XBI_k_model<- intCriteria(datacustomerN_matrix, k_model$cluster, c("Xie_Beni", "Davies_Bouldin","Calinski_Harabasz","Dunn","Silhouette")) 

XBI_c_model<- intCriteria(datacustomerN_matrix, c_model$cluster, c("Xie_Beni", "Davies_Bouldin","Calinski_Harabasz","Dunn","Silhouette")) 


# INDEX  AND RULES 
# Calinski     max
# daviesboul    min
# dunn          max
# silhoutte     max
#xie beni       min 


