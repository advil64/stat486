library(smooth)
#load data from the csv file
presidents <- read.csv(file="presidents.csv")
#perform the smoothing
MovingAverage <- sma(presidents$presidents,order=5,silent=FALSE)
summary(MovingAverage)