library(faraway)
#install.packages("caret") #install this package if needed
library(caret)
set.seed(13245) #use this seed

head(cars,1L)

attach(cars) #n=50
# sorting dataset by distance for graphing purposes
cars <- cars[order(dist),]
#print out our dataset to see the sorted values
cars

windows(7,7)
plot(x=cars$dist,y=cars$speed)

#Fit a polynomial of degree 4
poly4<- lm(speed~dist+I(dist^2)+I(dist^3)+I(dist^4), data=cars)
summary(poly4) #summary of results from fitting a polynomial of degree 4
plot(x=cars$dist,y=cars$speed)
lines(x=cars$dist,y=poly4$fitted, type="l", col="red")

#Compute the cross-validation metrics for degree 4
# Define training control
train.control <- trainControl(method = "cv", number = 5)
# Train the model
CVpoly4 <- train(speed~dist,data = cars, method = "lm",
trControl = train.control)
# Summarize the results
print(CVpoly4)
##

if(FALSE){
    "The polynomial with a degree of 2 seems to be the best suited model for our data, this is because
    the R-Squared value is the highest while keeping the Mean Square Error to a minimum. The results
    from the other models seemed to suffer from overfitting the data which causes an increased Error
    value."
}