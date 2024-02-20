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
train_data <- data.frame(time = x,Drivmidler_i_alt=training_data)

# Plot the training data versus x
y_name<- 'Number of vehicles'
plotting<-ggplot(train_data, aes(x = time, y = Drivmidler_i_alt)) +
  geom_point(color = "blue", shape = "o") +
  labs(x = 'Year', y = y_name)+ ggtitle('Drivmidler i alt vs time')+
  theme(plot.title = element_text(hjust = 0.5))
plotting

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

X = cbind(1, train_data$'time')
y <- train_data$'Drivmidler_i_alt'
inv_xtx = solve(t(X)%*%X)
theta_hat = inv_xtx%*%t(X)%*%y
beta0 = round(as.double(theta_hat[1]), 3)
beta1 = round(as.double(theta_hat[2]), 3)
plotting+geom_abline(intercept = theta_hat[1], slope=theta_hat[2], col='red')

# To estimate the parameters theta, I used the matrix form of the maximum
# likelihood estimator for the parameters. For this, I considered that 
# each observation comes from a normal distribution, and the errors also
# follow the normal distribution with mean equal to 0 and constant variance 
# sigma. For every pair of error_i and error_j, i != j, error_i and error_j 
# are uncorrelated.


#########
## 2.2 ##
#########

cat(sprintf("\nthe values are: \n b0.hat: %f \n b1.hat: %f", beta0, beta1))

# At this point, we will estimate the predicted values for b0 and b1, and the 
# residuals (the difference of real values and the predicted).Afterwards, we 
# can calculate the variance of the residuals (sigma^2) as a sum of squared 
# errors of residuals divided by (n (observations)-p (parameters)) and then 
# the variance-covariance matrix of the residuals that is given by sigma**2 * 
# (x^T x)^{-1}.The estimated standart error of each parameter is given by the
# square root of the corresponding diagonal element of the variance-covariace
# matrix.

# Predicted values
y_pred <- X %*% theta_hat
# residuals
residuals <- y - y_pred
S_theta<-as.double(t(residuals) %*% residuals)
# sigma^2
residuals_std <- sqrt(S_theta/(n-2))
# variance of theta
Var_theta<-residuals_std**2*inv_xtx
#  est. standard errors
sigma_beta0_hat <- sqrt(Var_theta[1, 1])
sigma_beta1_hat <- sqrt(Var_theta[2, 2]) 
# present the  estimated parameters beta and their standard errors
results2_2 <- data.frame(
  Coeff = c('beta_0', 'beta_1'),
  Estimate = theta_hat,
  `Standard Error` = c(sigma_beta0_hat, sigma_beta1_hat)
)
print(results2_2)

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
