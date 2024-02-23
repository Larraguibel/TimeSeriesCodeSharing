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
# path_t<-"C:/Users/spisa/OneDrive/Υπολογιστής/Assignment1_TSA/DST_BIL54_train.xlsx"           # Change the path !
path_t <- "C:/Users/margr/OneDrive - Danmarks Tekniske Universitet/Skrivebord/DTU/time-series-analysis/assignments/assignment1/data/DST_BIL54_train.xlsx"
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
y_lm <- X %*% theta_hat
# residuals
residuals <- y - y_lm
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

#########
## 2.3 ##
#########

# Create the vector of forecasting time, xtest
x60=x59+1/12
x71=x59+1
xtest=seq(x60,x71,by=tstep)
Xtest <- cbind(1, xtest)
print(Xtest)

# compute predictions 
y_pred <- Xtest%*%theta_hat
print(y_pred)
# compute prediction variance-covariance matrix:
sigma2<-residuals_std**2
Vtheta_hat_test <- sigma2*(1+Xtest%*%inv_xtx%*%t(Xtest))
# the variances of individual predictions are in the diagonal of the matrix above# compute "prediction intervals"
#he critical value for a 5% significance level is(as the t-distribution can be approximated by the standard normal
#distribution when the sample size is large (typically N > 30)): 
z1_a2=1.96
y_pred_lwr <- y_pred - z1_a2*sqrt(diag(Vtheta_hat_test))
y_pred_upr <- y_pred + z1_a2*sqrt(diag(Vtheta_hat_test))
# present a table of predicted values and their intervals
forecast_values <- data.frame(Time=xtest,Predicted_values= y_pred,
                              Lower_intervals = y_pred_lwr,
                              Upper_intervals = y_pred_upr)
print(forecast_values)

#########
## 2.4 ##
#########

plotting2_4<-ggplot(train_data, aes(x = time, y = Drivmidler_i_alt,color = "Train Data")) +
  geom_point(shape = 16,size=1) +
  labs(x = 'Year', y = y_name)+ ggtitle('Drivmidler i alt vs time')+
  theme(plot.title = element_text(hjust = 0.6))+
  geom_line(aes(y=y_lm,color = "Fitted model"), size=.5,show.legend = TRUE)+
  geom_point(data=forecast_values, aes(x = Time, y = Predicted_values,color = "Forecast"),shape = 16, size=1.)+
  geom_ribbon(data=forecast_values, aes(x=Time,ymin=Lower_intervals, ymax=Upper_intervals), inherit.aes=FALSE, alpha=0.2, fill="red")
plotting2_4+labs(color = "") +
  scale_color_manual(values = c("red","red","blue"), name = "", labels = c("Fitted model","Forecasted values","Training data"))+
  guides(color = guide_legend(override.aes = list(linetype = c("solid","blank","blank"),shape=c(NA,16,16))))

#########
## 2.5 ##
#########

# Load the test data:
# path_ch<-"C:/Users/spisa/OneDrive/Υπολογιστής/Assignment1_TSA/DST_BIL54_test.xlsx"
path_ch <- "C:/Users/margr/OneDrive - Danmarks Tekniske Universitet/Skrivebord/DTU/time-series-analysis/assignments/assignment1/data/DST_BIL54_train.xlsx"
check_data <- t(readxl::read_excel(path_ch, sheet = 1, col_names =TRUE)[1,2:13])
test_data <- data.frame(Time = xtest,Drivmidler_i_alt=check_data)
# plot WITH test data:
plotting2_5 <- plotting2_4 +
  geom_point(data=test_data, aes(x=Time,y=Drivmidler_i_alt,color = "Test Data"), size=1.25)+
  labs(color = "") +
  scale_color_manual(values = c("red","red","green","blue"), name = "", labels = c("Fitted model","Forecasted values","Test data","Training data"))+
  guides(color = guide_legend(override.aes = list(linetype = c("solid","blank","blank","blank"),shape=c(NA,16,16,16))))
plotting2_5

# Our forecast is not good as the model consistently overestimates the values of the test data. 
# This is evident in the graph, where all test data points fall below the lower prediction interval.
# The model initially performs well, exhibiting smaller residuals during the first 2 years of the 
# 5-year training data period. However, a notable increase in residuals is observed during the last 
# 3 years of the training data.As a result, the linear model is inadequate to describe the test data,
# as it fails to capture changing dynamics and trends in the later years of the training data. The
# The accuracy of the  model could  be enchanced if we consider more weighting the later data  of the 
# training data (the last 3 years) than the training data of the earlier years.

#########
## 2.6 ##
#########

# The residuals have an increasing trend over time during the training data period , especially the last
# training data (2020-2023), whereas from from 2018-2020 ( 2 years) the residuals are smaller and they do
# not follow a normal distribution. So, the model assumptions are not fulfilled, as the variance of the 
# training data is not the same during these periods. 
# (Propably I should calculate the variance of data the first and later years of the training period) 
# and make some scatter plots and qqplots of residuals.

#####################
#####  Part 4 #######  
#####################

#########
## 4.1 ##
#########

# As the data shows a linear behavior, we will implement a linear trend model.

L <- cbind(c(1,0), c(1,1))
Linv <- solve(L)
f.0 <- c(1,0)

#########
## 4.2 ##
#########

# Our x vector starts from 2018. We need to translate it into the negative
# part to the axis.

x.trend = 
