library(fpp2)
library(dplyr)
library(tidyverse)
library(readxl)

data_train = readxl::read_excel(file.choose())
data_test = readxl::read_excel(file.choose())


#####################
#####  Part 1 #######  
#####################

#########
## 1.1 ##
#########

# Creating the xtrain variable
xtrain = rep(0.0, length(data_train) - 1)
for(i in 1:length(data_train) - 1){
  xtrain[i] = 2018 + (i - 1)/12
}

xtrain <- as.vector(xtrain)
xtrain = t(xtrain)
xtrain = as.data.frame(xtrain)

# modifying ytrain
ytrain = data_train[1, 2:length(data_train)]
ytrain <- as.data.frame(lapply(ytrain, as.numeric))

df = rbind(xtrain, ytrain)
df = t(df)

plot(df[,1], df[,2], col="blue", 
     xlab="Time", ylab="Number of Vehicles")

#########
## 1.2 ##
#########

# We can observe that the data looks linear to some point, though a curve
# would probably fit better. To analize further whe should consider that
# the scale of the axis might bias usto see linearity. 


#####################
#####  Part 2 #######  
#####################

#########
## 2.1 ##
#########

X = cbind(1, df[,1])
x = df[,1]
y = df[,2]
N = length(y)
inv_xtx = solve(t(X)%*%X)
beta_hat = inv_xtx%*%t(X)%*%y
beta0 = round(as.double(beta_hat[1]), 3)
beta1 = round(as.double(beta_hat[2]), 3)
abline(a = beta0, b=beta1, col='red')

# To estimate the parameters beta, I used the matrix form of the maximum
# likelihood estimator for the parameters. For this, I considered that 
# each observation comes from a normal distribution, and the 
# errors have mean 0 and constant variance sigma. For every pair of
# error_i and error_j, i != j, error_i and error_j are uncorrelated.


#########
## 2.2 ##
#########

cat(sprintf("\nthe values are: \n b0.hat: %f \n b1.hat: %f", beta0, beta1))


# No we will estimate the standard errors for b0 and b1. for this, we must 
# remember that the variance for the beta_hat vector is sigma * (x^T x)^{-1}.
# Then, we just take the diagonal elements of the resulting matrix.

# We estimate first the std deviation of the errors using its MLE:

errors = y - (beta0 + beta1*x)
SSE = as.double(t(errors) %*% errors)

sigma_MLE = sqrt(SSE/(N - 1))
beta_hat.var = sigma_MLE * inv_xtx
beta0.var= beta_hat.var[1,1]
beta1.var= beta_hat.var[2,2]

cat(sprintf("\nthe values for the std errors of the estimators are: \nstd(b0.hat): %f \nstd(b1.hat): %f",
            beta0.var, beta1.var))



#####################
#####  Part 4 #######  
#####################

#########
## 4.1 ##
#########

# As the data shows a linear behavior, we will implement a linear trend model.

L = cbind(c(1,0), c(1,1))
f.0 = c(1,0)


#########
## 4.2 ##
#########

# Our x vector starts from 2018. We need to translate it into the negative
# part to the axis.

x.trend = 
