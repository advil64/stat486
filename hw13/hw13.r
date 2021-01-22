#read in the data which is in a csv file
presidents <- read.csv(file="presidents.csv",header = TRUE)
head(presidents)
str(presidents)

library(faraway)
#code to use for different spans. The one below uses a span of 0.20
with(presidents,{
plot(presidents ~ quarter, col=gray(0.1))
f <- loess(presidents ~ quarter,span=0.05)
i <- order(quarter)
lines(f$x[i],f$fitted[i])
})
#optimal span reaction
if(FALSE){
    "This dataset seems pretty noisy so we don't want to overfit the data by choosing a small span value.
    However we also want to choose a value that is descriptive enough to understand the overall pattern.
    I think a span of 0.2 does well to meet both expectations."
}
#code to use for different choices of h. The one below uses an h
library(sm) #install if not already installed
with(presidents,sm.regression(x=quarter,y=presidents,h=.2))
#optimal standard deviation
if(FALSE){
    "Again, a kernel density function with h=0.2 seems to overfit the data I would most likely chose 
    h=0.8"
}