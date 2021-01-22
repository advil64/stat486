library(datasets)
library(ggplot2)
library(cluster)
library(NbClust)
library(psych)

head(USArrests)

#median UrbanPop is 66 if the urban population is greater than 66, the urban cat is 1
USArrests$Urbancat <- as.factor(ifelse(USArrests$UrbanPop > 66,1,0))
USArrests$Urbancat

summary(USArrests$Urbancat)

Arrests_z <- data.frame(scale(USArrests[,1:2]))

set.seed(1423)

nc <- NbClust(Arrests_z, min.nc=2, max.nc=15, method="kmeans")
ArrestCluster <- kmeans(Arrests_z[,1:2], 2, nstart = 50)  
ggplot(Arrests_z,aes(x = Murder,y = Assault, color= USArrests$Urbancat)) + geom_point()
table(ArrestCluster$cluster,USArrests$Urbancat)