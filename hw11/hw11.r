library(faraway) #this command brings in a library of regression functions
require(olsrr)
library(caret)

data(fat,package="faraway")

# Define training control
set.seed(13245)
train.control <- trainControl(method = "cv", number = 10)

#We need to chose between two different models, one that includes weight and abdom
lmwa <- train(brozek ~ abdom + weight, data = fat, method = "lm", trControl = train.control)
#print results
print(lmwa)

#now for the other model
lmwat <- train(brozek ~ abdom + weight + thigh,data = fat, method = "lm", trControl = train.control)
#print the results
print(lmwat)

if(FALSE){
    "It seems that the model including abdom, weight, AND thigh was the superior one. The R-Squared value
    was greater and the Root Mean Square Error was lower which means lower variance, the MAE was also lower
    . The choices are consistant across the three tests."
}