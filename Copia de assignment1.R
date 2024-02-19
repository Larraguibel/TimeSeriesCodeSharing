# Assignment 1
library(ggplot2)
library(fpp2)
library(dplyr)
library(tidyverse)
library(readxl)

#####################
#####  Part 1 #######  
#####################

#########
## 1.1 ##
#########

# Create the vector of time, x
n<-59
x1=2018.
x59=x1+n/12-1/12
tstep=1/12
x=seq(x1,x59,by=tstep)

# Load the training data & the dataframe of the training data and time
path_t<-"C:/Users/spisa/OneDrive/Υπολογιστής/Assignment1_TSA/DST_BIL54_train.xlsx"           # Change the path !
training_data <- t(readxl::read_excel(path_t, sheet = 1, col_names =TRUE)[1,2:(n+1)])
tr_da <- data.frame(time = x,Drivmidler_i_alt=training_data)

# Plot the training data versus x
y_name<- 'Number of vehicles'
p<-ggplot(tr_da, aes(x = time, y = Drivmidler_i_alt)) +
  geom_point(color = "blue", shape = "o") +
  labs(x = 'Year', y = y_name)+ ggtitle('Drivmidler i alt vs time')+
  theme(plot.title = element_text(hjust = 0.5))
p

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

X = cbind(1, tr_da$'time')
y <- tr_da$'Drivmidler_i_alt'
inv_xtx = solve(t(X)%*%X)
theta_hat = inv_xtx%*%t(X)%*%y
beta0 = round(as.double(theta_hat[1]), 3)
beta1 = round(as.double(theta_hat[2]), 3)
p+geom_abline(intercept = theta_hat[1], slope=theta_hat[2], col='red')

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
