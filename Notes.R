#trimming
install.packages("psych")
library(psych)
library(faraway)
library(vcd)

options(digits = 5)

diameter <- c(0.017, .018, .023, .031, .031, .033, .036, .070, .079, .2)

#change the trim value to control the outliers
trim1 <- describe(diameter, trim = 0.1)
print(trim1, digits = 5)

#winsorizing
winsor.means(diameter, trim=0.2,na.rm=TRUE)
y <- winsor(diameter, trim=0.1)

diameter
y

#quantiles where do we get these values from?
quantile(diameter, prob = c(0.1, 0.9))

#Monte Carlo Methods used in Gambling
isEvent = function(numDice, numSides, targetValue, numTrials){
  apply(matrix(sample(1:numSides, numDice*numTrials, replace=TRUE),nrow=numDice), 2, sum) == targetValue
}

set.seed(12345)
outcomes = isEvent(2,6,7,100000)
table(outcomes)
mean(outcomes)

#Monte Carlo stock trading incomplete

set.seed(54321)

days <- 200
changes <- rnorm(200, mean = 1.001, sd = 0.005)
plot(cumprod(c(10, changes)), type = '1', ylab="price", xlab="day", main="CompanyX closing price (Sample path)")
runs <- 100000

#simple linear models in matrix representation with mice

doses <- c(0,2,4,6)
time <- c(7,5,4,4)

#create the x matrix
X <- as.matrix(cbind(1, doses))
X
myframe <- data.frame(doses, time)
# X'Y = (X'X)β
XtX <- t(X) %*% X
# β = (X'Y)(X'X)^-1 invert the above matrix how does the solve method work?
XtXi <- solve(XtX)
#solve the other side X'Y
XtY <- t(X) %*% time
coefficientMatrix <- XtXi %*% XtY
coefficientMatrix

plot(myframe)
abline(lm(time ~ doses, myframe))

#do we need to know what a variance-covariance matrix is?

#Multiple Linear Regression galapogos island 

plot(Species~Adjacent, data=gala)
checkmod <- lm(Species ~ Adjacent, data=gala)
abline(checkmod)
summary(checkmod)

#multicolinearity taking place in this multiple regression
summary(lm(formula = Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data=gala))

#correlation matrix to prevent multicolinearity
cov(odor[,-1])
summary(lm(formula = odor ~ temp + gas + pack, data = odor))
summary(lm(formula =odor ~ gas + pack, data = odor))
#delete two observations to prevent multicollinearity
odor_trim <- odor[-c(1,8), ]
odor_trim

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
x <- model.matrix(~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
x
y <- gala$Species
y
xtx <- t(x) %*% x
xtxi <- solve(xtx)
xty <- t(x) %*% gala$Species
coeffmatr <- xtxi %*% xty
coeffmatr

sqrt(diag(xtxi)) * 60.975

#quadratic and polynomial regression

x <- 1:20
y <- x+rnorm(20)

print(x)
print(y)

plot(x,y,type="p",ylim=c(0,25), xlim=c(0,20))

lwy <- loess(y ~ x)

plot(x,y,ylim=c(0,25), xlim=c(0,20))
lines(predict(lwy),col='red',lwd=2)
quad_model <- lm(y~x + I(x^2))
summary(quad_model)

#Bootstrapping - ECDF takes a sample and creates a distribution function
#based on the sample P(X<=t) Cumulative Distribution Function F-hat
#is a good estimator of F. using monte carlo to predict a distribution of 
#a certain population standard normal

set.seed(12345)

normal.numbers <- rnorm(100)

normal.ecdf <- ecdf(normal.numbers)
normal.ecdf
plot(normal.ecdf, col="blue")

#bootstrapping chi square distribution
chisq.ecdf <- ecdf(rchisq(n=100,df=1))
plot(chisq.ecdf,col="blue")

#random variables are charecterized by their cdfs

#bootstrapping confidence intervals in linear regression
library(faraway)
data(gala,package = "faraway")
summary(gala)
nrow(gala)

lmod <- lm(lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala))
confint(lmod)

#Bootstrapping in regression
# 1. Obtain the residuals from the model n = 30

sample(30, rep=TRUE)

#take a sample of size 30 large as what you start with with replacement
#from the residuals

set.seed(12345)
nb <- 4000
coefmat <- matrix(NA,nb,6)
head(coefmat)

resids <- residuals(lmod)
resids

preds <- fitted(lmod)
preds

#yi-hat *

for(i in 1:nb){
  
  booty <- preds + sample(resids, rep=TRUE)
  bmod <- update(lmod, booty ~ Area + Elevation + Nearest + Scruz + Adjacent)
  coefmat[i,] <- coef(bmod)
}

#Robust Regression when you have an outlier (what is quantile regression?)
#LAD vs OLS find out the difference
#F-hat of the 7-esubis is a good estimator for F
#residuals i the LAD model tend to be larger in absolute value than the OLS
#residuals. LAD model uses bootstrapping to be a robust regression

install.packages("quantreg")
set.seed(15234)
require(quantreg)
#non robust estimation
summary(lmod)
confint(lmod)
#now for the robust regression, better against outliers
LADmodel <- rq(Species ~ Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
summary(LADmodel)
bcoef <- matrix(0,1000,6)
head(bcoef)
for(i in 1:1000){
  newy <- predict(LADmodel) + residuals(LADmodel)[sample(30, rep=TRUE)]
  brg <- rq(newy ~ Area + Elevation + Nearest + Scruz+Adjacent, data=gala)
  bcoef[i,] <- brg$coef
}
colnames(bcoef) <- names(coef(LADmodel))
#basically this regression is non-parametric
apply(bcoef,2,function(x) quantile(x, c(0.025,0.975)))
bcoef <- data.frame(bcoef)
head(bcoef)
p1 <- ggplot(bcoef, aes(x=Area)) + goem_density() + xlim(-0.08,0.08)

#permutation tests

type1 = c(65,81,57,66,82,82,67,59,75,70)
type2 = c(64,71,83,59,65,56,69,74,82,79)

summary(type1)
summary(type2)
#do the F test to compare variances to see if they come from same population
var.test(type1,type2)
# now do a two sample t-test (review this)
t.test(type1, type2, var.equal=TRUE, paired=FALSE)
#you can use a normal qq plot to see if the data are normal
#to choose the sample for type 1 and type 2 vectors how unusual is it
#with permutations of these 20 is it to get a rejection of the null
choose(20,10)
set.seed(13254)
#sample of 100000
#generate a single column vector of two variables, type and burntime
example <- data.frame(type=c(rep("1", times=length(type1)), 
                             rep("2", times=length(type2))), 
                      burntime=c(type1,type2))
attach(example)


set.seed(13254)

R <- 100000
scores <- burntime
t.values <- numeric(R)

for (i in 1:R) {
  index <- sample(1:20, size=10, replace=FALSE)
  group1 <- scores[index]
  group2 <- scores[-index]
  t.values[i] = t.test(group1,group2)$statistic
}

#more permutation testing
SAT <- c(576,635,578,558,666,580,555,651,661,605,575,653,545,572,594)
GPA <- c(3.9,3.8,3.3,3.5,4.0,3.6,4.0,3.5,3.9,3.6,3.6,3.2,3.2,3.3,3.4)

plot(SAT,GPA)
#calculate empirical correlation of the data
r.hat <- cor(SAT,GPA)  #Pearson product-moment correlation
r.hat
#This is the standard way to calculate the r value

#Spearemon test of independence
install.packages("survival")

spearman_test(SAT~GPA, distribution="asymptotic", data=mydata) #nonparametric test
#Asymptotic theory basically means that the distribution is a large samle?

library(faraway)


lmod <- lm(Species ~ Nearest + Scruz, data=gala)

show1 <- summary(lmod)
show1
names(show1)
show1$fstatistic
 #we want to know if all of the coefficients are zero vs one of them is not
#equal to 0
#Under H0 species is equal to B0 the intercept

set.seed(123)
#perform permuation test by permuting the Species (or y) observations
#note there are 30! permutations of the y vector
factorial30 <- factorial(30)  #factorial(x) computes the factorial of x
factorial30

#so, try sampling among the many permutations
nreps <- 4000
vector_fstats <- numeric(nreps)
head(vector_fstats)

for (i in 1:nreps) {
  lmod_permute <- lm(sample(Species,rep=FALSE) ~ Nearest + Scruz, data=gala)
  #the sampling is in without replacement
  
  vector_fstats[i] <- summary(lmod_permute)$fstatistic[1]
}

mean(vector_fstats > show1$fstatistic[1])

#Bonferroni intervals
#alpha*=alpha/(2m) alpha*=alpha/m

#Now the good stuff, logistic regression different from a continuous regression
#looking at responses which are binary model: xi'B + ei
#the response to a logistic regression is a bernoulli random variable
#Expected value of Y equals probability of Y equal 1
#O-ring failures for space shuttle challenger
#we model the expected value of the logistic response function by using the
#sigmoidal function which is basically an 'S' shape as x increases exp value
#decreases if B1 < 0 and the other way around for B1 > 0
# ln(pi/1-pi) is called the logit transformation and it linearizes the response
#Model parameters are estimated by the maximum likelihood estimator
#L(y1,y2,y3,y4...|theta)

logistic <- glm(TD ~ Temp, dat=oring, family=binomial(link='logit'), data=oring)
wald.test(b=coef(logistic), sigma=vcov(logistic), Terms=2)
#prediction and classification models
#the response for a ligistic regression is pi given different levels of x
#Accuracy of the model tells how often was the model correct

#bonferroni again?
#you are sure that at least 95%for both Beta1 and Beta2
#why are joint confidence intervals useful??

#Maximum likelihood estimation
#P(yi) = pi^yi * 1-pi^1-yi
#Maximum likelihood estimator maximizes the log of the likelihood function which is above

set.seed(45321)
p.parameter <- 0.8

#copy from the MLE text file

#MLE function tries to find the maximum value of the probability
likelihood <- function(sequence, p.parameter){
  likelihood <- 1
  for(i in 1:length(sequence)){
    if(sequence[i] == 1){
      likelihood <- likelihood * p.parameter
    } else{
      likelihood <- likelihood * (1-p.parameter)
    }
  }
}

#Newton Raphson Technique - function that is useful for the MLE calculations with respect
#to the derivative calculation
lmod <- lm(happy ~ money + sex + love + work, data=happy)
summary(lmod)

summary(lmod)$coefficients[5,3]
qt(0.975,34)

#we can do the same thing through permutationx
2 tests, to see wether work is really significant by permuting the 
#predictor rather than the response, before we tested wether all betas were 0 with the F-test
#changing work due to the permutation changes the other variables as well due to the multicollinearity

#looks to see how rare our given p-hat is we do this test basically
mean(abs(vector_tB4)>abs(summary(lmod)$coefficients([5,3])))

#Now we do the newton raphson diagram
#T0 is the initial guess, study newton raphson

#Likelihood ratio
neuralgia <- read.csv(file.choose())
neuralgia

table1 <- with(neuralgia, table(pain, Treatment))
with(neuralgia, table(neuralgia, prop.table(table1))
#basically more tables
logistic <- glm(Pain ~ Treatment+Sex+Age+Duration, data=neuralgia())
summary(logistic)

