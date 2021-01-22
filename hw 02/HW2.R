
require("vcd")
#set a seed to randomize the process
set.seed(13425)
#generate 1000 observations from a binomial distribution with n=30 and p=0.2
ybinom <- rbinom(1000, 30, 0.2)
describe(ybinom)
hist(ybinom)
#distribution plots used to measure how well the data fits a distribution type = "poisson", "binomial", "nbinomial"
distplot(ybinom, type="binomial",size=30, main="Binomialness plot")
distplot(ybinom, type="poisson", main="Poissonness plot")
distplot(ybinom, type="nbinomial", main="Negative Binomialness plot")
#Use a kernel density plot to better visualize the distribution
d <- density(ybinom)
plot(d,main="kernel density of ybinom") # plots the results 