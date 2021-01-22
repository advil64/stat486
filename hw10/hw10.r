library(faraway) #this command brings in a library of regression functions

#install.packages("olsrr") #install olsrr if package has not been installed on your computer
library(olsrr)

data(fat,package="faraway")
#Can we predict body fat using only easy-to-record measurements?

#use the variables specified in this model
lmod <- lm(brozek ~ weight + neck + chest + abdom + hip + thigh, data=fat)

ols_step_all_possible(lmod)
ols_step_forward_p(lmod,details=TRUE)
ols_step_backward_p(lmod,details=TRUE)
ols_step_both_p(lmod,details=TRUE)
if(FALSE){
    "I would chose the 5 variable model of ambdom, weight, thigh, nexk, and hip. This is because all of
    our model selection procedures agreed on the same model and because the R^2 adjusted is relatively high
    whereas the RMSE is the lowest. The other model statistics also agree with this statement."
}