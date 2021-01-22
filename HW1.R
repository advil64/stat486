#Question 3 Pg. 12
#The dataset prostate is from a study on 97 men with prostate cancer who were
#due to receive a radical prostatectomy. Make a numerical and graphical summary
#of the data as in the first question.

install.packages("faraway")
library(faraway)
help(prostate)

data(prostate, package="faraway")

#SVI stands for seminal vesicle invasion and it is a categorical variable
prostate$svi <- factor(prostate$svi)
#name the different categories
levels(prostate$svi) <- c("negative", "positive")

#Gleason score is also a categorical variable
prostate$gleason <- factor(prostate$gleason)
levels(prostate$gleason) <- c(6:10)

#look at the summary of our data
summary(prostate)

#patients who had an SVI seem to have had a larger cancerous volume
plot(lcavol ~ svi,prostate)
#histogram is skewed left which migh indicate prostate cancer chance increases with age
hist(prostate$age, xlab = "Age", main = "")