# Clear your workspace before starting to work

rm(list = ls())

# Set your Working directory where your data and everything you produce will be stored
# Display currently set working directory

getwd() 

# Set your working directory with setwd("").
# copy-paste the whole path from the console below into the R script (editor)
setwd("C:location")

# Display the content of the current working directory:
dir() 
# Read CV file
read.csv("ECOWAS_12.csv")
read.csv("ECOWAS_13.csv")
read.csv("ECOWAS_14.csv")
read.csv("ECOWAS_15.csv")
read.csv("ECOWAS_16.csv")

#######HCA Analysis of Cases######
#Load Cluster Analysis Packages
library(cluster); 
library(ade4); 
library(vegan); 
library(labdsv); 
library(RColorBrewer)
library(gclus)
library(factoextra)
library(magrittr)
library(dendextend)

####Load Data######
ECOWAS12 <- read.csv("ECOWAS_12.csv", row.names=1, header = TRUE)
ECOWAS13 <- read.csv("ECOWAS_13.csv", row.names=1, header = TRUE)
ECOWAS14 <- read.csv("ECOWAS_14.csv", row.names=1, header = TRUE)
ECOWAS15 <- read.csv("ECOWAS_15.csv", row.names=1, header = TRUE)
ECOWAS16 <- read.csv("ECOWAS_16.csv", row.names=1, header = TRUE)


#####view data####
ECOWAS12
ECOWAS13
ECOWAS14
ECOWAS15
ECOWAS16

####Plot Dataset to Observe Variable Clustering#####
plot(ECOWAS12)
plot(ECOWAS13)
plot(ECOWAS14)
plot(ECOWAS15)
plot(ECOWAS16)

##### Prepare Data #####
# listwise deletion of missing (missing data must be in NA)
ECOWAS12 <- na.omit(ECOWAS12)
ECOWAS13 <- na.omit(ECOWAS13)
ECOWAS14 <- na.omit(ECOWAS14)
ECOWAS15 <- na.omit(ECOWAS15)
ECOWAS16 <- na.omit(ECOWAS16)


# standardize variables
ECOWAS12Scaled <- scale(ECOWAS12) 
ECOWAS13Scaled <- scale(ECOWAS13) 
ECOWAS14Scaled <- scale(ECOWAS14) 
ECOWAS15Scaled <- scale(ECOWAS15) 
ECOWAS16Scaled <- scale(ECOWAS16) 


### HIERACHICAL CLUSTERING 2012###

## Choosing Number of Cluster Groups Using (Kmeans) ####
##Year 2012####
k <- list()
for(i in 1:14){k[[i]] <- kmeans(ECOWAS12Scaled, i)} #find the value of K (clusters) from 1 to 14. Thus, 14 clusteers and everything inbetween

k #focus on ratio of between sum of squares with total sum of squares (value increase when an additional cluster is added)


#Calculate between sum of squares by total sum of square and plot it to identify a dip 

betweenss_totss <- list()
for(i in 1:14){betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss}

plot(1:14, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)") #where you see a shoulder on the plot is where the optimum number is clusters is to select



#Draw line for selected area
abline(h= 0.78,v= 7, col= "blue", lty=3)

#Plot dataset with coloured selected number of clusters 
for(i in 1:7){
  plot(ECOWAS12, col = k[[i]]$cluster)
}
##Number of clusters decided on: 7


#ECOWAS Economic Variables for 2012
ECOWAS12dist <- dist(ECOWAS12Scaled) #Euclidean distance of scaled dataframe
ECOWAS12HCA <- hclust(ECOWAS12dist, "ward.D2") #Hierachical Clustering using Wards method
plot(ECOWAS12HCA, hang=-1)  #Plot dendrogram (hang=1 makes sure clusters are in alinment)
ROECOWAS12HCA <- reorder.hclust(ECOWAS12HCA, ECOWAS12dist)#Reorder dendrogram
plot(ROECOWAS12HCA, hang=-1)
member.ECOWAS12HCA <- cutree(ROECOWAS12HCA,7)
rect.hclust(ROECOWAS12HCA, k = 7, border = "red") #Patition clusters
member.ECOWAS12HCA <- cutree(ECOWAS12HCA,7)#Identify memberships of each cluster
member.ECOWAS12HCA#view membership
table(member.ECOWAS12HCA) #use a table to show the number of memebrs in each cluster
aggregate(ECOWAS12, list(member.ECOWAS12HCA), median)#finding median to indicates which variables are playing important roles in each cluster
attributes(ECOWAS12HCA) #Check attributes 

ECOWAS12HCA$height #checking height of cluster

####Cluster Visialization####
#Heat Map of distance matrix order with the dendrogram
dend.ECOWAS12HCA <- as.dendrogram(ROECOWAS12HCA)
heatmap(as.matrix(ECOWAS12dist),Rowv = dend.ECOWAS12HCA, symm = TRUE, margins = c(3,3)) #the more yellow the more similar, the more red


### HIERACHICAL CLUSTERING 2013###

## Choosing Number of Cluster Groups Using (Kmeans) ####
##Year 2013####
k <- list()
for(i in 1:14){k[[i]] <- kmeans(ECOWAS13Scaled, i)} #find the value of K (clusters) from 1 to 14. Thus, 14 clusteers and everything inbetween

k #focus on ratio of between sum of squares with total sum of squares (value increase when an additional cluster is added)


#Calculate between sum of squares by total sum of square and plot it to identify a dip 

betweenss_totss <- list()
for(i in 1:14){betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss}

plot(1:14, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)") #where you see a shoulder on the plot is where the optimum number is clusters is to select



#Draw line for selected area
abline(h= 0.95,v= 7, col= "blue", lty=3)

#Plot dataset with coloured selected number of clusters 
for(i in 1:7){
  plot(ECOWAS13, col = k[[i]]$cluster)
}
##Number of clusters decided on:7


#ECOWAS Economic Variables for 2013
ECOWAS13dist <- dist(ECOWAS13Scaled) #Euclidean distance of scaled dataframe
ECOWAS13HCA <- hclust(ECOWAS13dist, "ward.D2") #Hierachical Clustering using Wards method
plot(ECOWAS13HCA, hang=-1)  #Plot dendrogram (hang=1 makes sure clusters are in alinment)
ROECOWAS13HCA <- reorder.hclust(ECOWAS13HCA, ECOWAS13dist)#Reorder dendrogram
plot(ROECOWAS13HCA, hang=-1)
member.ECOWAS13HCA <- cutree(ROECOWAS13HCA,7)
rect.hclust(ROECOWAS13HCA, k = 7, border = "red") #Patition clusters
member.ECOWAS13HCA <- cutree(ECOWAS13HCA,7)#Identify memberships of each cluster
member.ECOWAS13HCA#view membership
table(member.ECOWAS13HCA) #use a table to show the number of memebrs in each cluster
aggregate(ECOWAS13, list(member.ECOWAS13HCA), median)#finding median to indicates which variables are playing important roles in each cluster
attributes(ECOWAS13HCA) #Check attributes 

ECOWAS13HCA$height #checking height of cluster

####Cluster Visialization####
#Heat Map of distance matrix order with the dendrogram
dend.ECOWAS13HCA <- as.dendrogram(ROECOWAS13HCA)
heatmap(as.matrix(ECOWAS13dist),Rowv = dend.ECOWAS13HCA, symm = TRUE, margins = c(3,3)) #the more yellow the more similar, the more red


### HIERACHICAL CLUSTERING 2014###

## Choosing Number of Cluster Groups Using (Kmeans) ####
##Year 2014####
k <- list()
for(i in 1:14){k[[i]] <- kmeans(ECOWAS14Scaled, i)} #find the value of K (clusters) from 1 to 14. Thus, 14 clusteers and everything inbetween

k #focus on ratio of between sum of squares with total sum of squares (value increase when an additional cluster is added)


#Calculate between sum of squares by total sum of square and plot it to identify a dip 

betweenss_totss <- list()
for(i in 1:14){betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss}

plot(1:14, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)") #where you see a shoulder on the plot is where the optimum number is clusters is to select



#Draw line for selected area
abline(h= 0.82,v= 9, col= "blue", lty=3)

#Plot dataset with coloured selected number of clusters 
for(i in 1:9){
  plot(ECOWAS14, col = k[[i]]$cluster)
}
##Number of clusters decided on:9


#ECOWAS Economic Variables for 2014
ECOWAS14dist <- dist(ECOWAS14Scaled) #Euclidean distance of scaled dataframe
ECOWAS14HCA <- hclust(ECOWAS14dist, "ward.D2") #Hierachical Clustering using Wards method
plot(ECOWAS14HCA, hang=-1)  #Plot dendrogram (hang=1 makes sure clusters are in alinment)
ROECOWAS14HCA <- reorder.hclust(ECOWAS14HCA, ECOWAS14dist)#Reorder dendrogram
plot(ROECOWAS14HCA, hang=-1)
member.ECOWAS14HCA <- cutree(ROECOWAS14HCA,9)
rect.hclust(ROECOWAS14HCA, k = 9, border = "red") #Patition clusters
member.ECOWAS14HCA <- cutree(ECOWAS14HCA,9)#Identify memberships of each cluster
member.ECOWAS14HCA #view membership
table(member.ECOWAS14HCA) #use a table to show the number of memebrs in each cluster
aggregate(ECOWAS14, list(member.ECOWAS14HCA), median)#finding median to indicates which variables are playing important roles in each cluster
attributes(ECOWAS14HCA) #Check attributes 

ECOWAS14HCA$height #checking height of cluster

####Cluster Visialization####
#Heat Map of distance matrix order with the dendrogram
dend.ECOWAS14HCA <- as.dendrogram(ROECOWAS14HCA)
heatmap(as.matrix(ECOWAS14dist),Rowv = dend.ECOWAS14HCA, symm = TRUE, margins = c(3,3)) #the more yellow the more similar, the more red




### HIERACHICAL CLUSTERING 2015###

## Choosing Number of Cluster Groups Using (Kmeans) ####
##Year 2015####
k <- list()
for(i in 1:14){k[[i]] <- kmeans(ECOWAS15Scaled, i)} #find the value of K (clusters) from 1 to 14. Thus, 14 clusteers and everything inbetween

k #focus on ratio of between sum of squares with total sum of squares (value increase when an additional cluster is added)


#Calculate between sum of squares by total sum of square and plot it to identify a dip 

betweenss_totss <- list()
for(i in 1:14){betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss}

plot(1:14, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)") #where you see a shoulder on the plot is where the optimum number is clusters is to select



#Draw line for selected area
abline(h= 0.9,v= 9, col= "blue", lty=3)

#Plot dataset with coloured selected number of clusters 
for(i in 1:9){
  plot(ECOWAS15, col = k[[i]]$cluster)
}
##Number of clusters decided on:9 


#ECOWAS Economic Variables for 2015
ECOWAS15dist <- dist(ECOWAS15Scaled) #Euclidean distance of scaled dataframe
ECOWAS15HCA <- hclust(ECOWAS15dist, "ward.D2") #Hierachical Clustering using Wards method
plot(ECOWAS15HCA, hang=-1)  #Plot dendrogram (hang=1 makes sure clusters are in alinment)
ROECOWAS15HCA <- reorder.hclust(ECOWAS15HCA, ECOWAS15dist)#Reorder dendrogram
plot(ROECOWAS15HCA, hang=-1)
member.ECOWAS15HCA <- cutree(ROECOWAS15HCA,9)
rect.hclust(ROECOWAS15HCA, k = 9, border = "red") #Patition clusters
member.ECOWAS15HCA <- cutree(ECOWAS15HCA,9)#Identify memberships of each cluster
member.ECOWAS15HCA#view membership
table(member.ECOWAS15HCA) #use a table to show the number of memebrs in each cluster
aggregate(ECOWAS15, list(member.ECOWAS15HCA), median)#finding median to indicates which variables are playing important roles in each cluster
attributes(ECOWAS15HCA) #Check attributes 

ECOWAS15HCA$height #checking height of cluster

####Cluster Visialization####
#Heat Map of distance matrix order with the dendrogram
dend.ECOWAS15HCA <- as.dendrogram(ROECOWAS15HCA)
heatmap(as.matrix(ECOWAS15dist),Rowv = dend.ECOWAS15HCA, symm = TRUE, margins = c(3,3)) #the more yellow the more similar, the more red




### HIERACHICAL CLUSTERING 2016###

## Choosing Number of Cluster Groups Using (Kmeans) ####
##Year 2016####
k <- list()
for(i in 1:14){k[[i]] <- kmeans(ECOWAS14Scaled, i)} #find the value of K (clusters) from 1 to 14. Thus, 14 clusteers and everything inbetween

k #focus on ratio of between sum of squares with total sum of squares (value increase when an additional cluster is added)


#Calculate between sum of squares by total sum of square and plot it to identify a dip 

betweenss_totss <- list()
for(i in 1:14){betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss}

plot(1:14, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)") #where you see a shoulder on the plot is where the optimum number is clusters is to select



#Draw line for selected area
abline(h= 0.9,v= 12, col= "blue", lty=3)

#Plot dataset with coloured selected number of clusters 
for(i in 1:12){
  plot(ECOWAS16, col = k[[i]]$cluster)
}
##Number of clusters decided on: 12


#ECOWAS Economic Variables for 2016
ECOWAS16dist <- dist(ECOWAS16Scaled) #Euclidean distance of scaled dataframe
ECOWAS16HCA <- hclust(ECOWAS16dist, "ward.D2") #Hierachical Clustering using Wards method
plot(ECOWAS16HCA, hang=-1)  #Plot dendrogram (hang=1 makes sure clusters are in alinment)
ROECOWAS16HCA <- reorder.hclust(ECOWAS16HCA, ECOWAS16dist)#Reorder dendrogram
plot(ROECOWAS16HCA, hang=-1)
member.ECOWAS16HCA <- cutree(ROECOWAS16HCA,12)
rect.hclust(ROECOWAS16HCA, k = 12, border = "red") #Patition clusters
member.ECOWAS16HCA <- cutree(ECOWAS16HCA,12)#Identify memberships of each cluster
member.ECOWAS16HCA#view membership
table(member.ECOWAS16HCA) #use a table to show the number of memebrs in each cluster
aggregate(ECOWAS16, list(member.ECOWAS16HCA), median)#finding median to indicates which variables are playing important roles in each cluster
attributes(ECOWAS16HCA) #Check attributes 

ECOWAS16HCA$height #checking height of cluster

####Cluster Visialization####
#Heat Map of distance matrix order with the dendrogram
dend.ECOWAS16HCA <- as.dendrogram(ROECOWAS16HCA)
heatmap(as.matrix(ECOWAS16dist),Rowv = dend.ECOWAS16HCA, symm = TRUE, margins = c(3,3)) #the more yellow the more similar, the more red


######Visual comparism of two dendrograms########
# Create a list to hold dendrograms for each time interval (2012/13; 2013/14; 2013/14; 2015/16)

 
dend_list.1 <- dendlist(dend.ECOWAS12HCA, dend.ECOWAS13HCA) #for first time interval 2012/13

dend_list.2 <- dendlist(dend.ECOWAS13HCA, dend.ECOWAS14HCA) #for second time interval 2013/14

dend_list.3 <- dendlist(dend.ECOWAS14HCA, dend.ECOWAS15HCA) #for first time interval 2014/15

dend_list.4 <- dendlist(dend.ECOWAS15HCA, dend.ECOWAS16HCA) #for first time interval 2015/16

# Align and plot dendrograms side by side

plot(dend_list.1) #plot comparism 2012/13

plot(dend_list.2) #plot comparism 2013/14

plot(dend_list.3) #plot comparism 2014/15

plot(dend_list.4) #plot comparism 2015/16








