library(fpp2)
library(dplyr)
library(tidyverse)
library(readxl)

data_train = readxl::read_excel(file.choose())
data_test = readxl::read_excel(file.choose())


#####################
#####  Part 1 #######  
#####################

# modifying xtrain
xtrain = rep(0.0, 12)
for(i in 1:12){
  xtrain[i] = 2018 + (i - 1)/12
}

xtrain <- as.vector(xtrain)
xtrain = t(xtrain)
xtrain = as.data.frame(xtrain)
names(xtrain) = 1:12

# modifying ytrain
ytrain = data_train[1, 2:13]
ytrain <- as.data.frame(lapply(ytrain, as.numeric))
names(ytrain) = 1:12

df = rbind(xtrain, ytrain)
df = t(df)

plot(df[,1], df[,2], col="blue")

# We can observe that the data looks linear to some point, though a curve
# would probably fit better. To analize further whe should consider that
# the scale of the axis might bias usto see linearity. 


#####################
#####  Part 2 #######  
#####################

X = cbind(1, df[,1])
y = df[,2]
beta_hat = solve(t(X)%*%X)%*%t(X)%*%y
beta0 = theta_hat[1]
beta1 = theta_hat[2]  
abline(a = theta_0, b=theta1, col='red')
  
# To estimate the parameters beta, I used the matrix form of the maximum
# likelihood estimator for the parameters. For this, I considered that 
# each observation comes from a normal distribution, and the 
# errors, with mean 0 and constant variance sigma are uncorrelated.



