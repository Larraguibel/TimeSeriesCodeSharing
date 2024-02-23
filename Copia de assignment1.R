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

#########
## 2.2 ##
#########

cat(sprintf("\nthe values are: \n b0.hat: %f \n b1.hat: %f", beta0, beta1))

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
# the variances of individual predictions are in the diagonal of the matrix above
#compute "prediction intervals"
#the critical value for a 5% significance level is(as the t-distribution can be approximated by the standard normal
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
# path_ch<-"C:/Users/spisa/OneDrive/Υπολογιστής/Assignment1_TSA/DST_BIL54_test.xlsx"        # Change the path !
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

#########
## 2.6 ##
#########

# Residual plot vs fitted values
residual_plot <- ggplot(data = data.frame(Fitted = y_lm, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(color = "black", shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme(plot.title = element_text(hjust = 0.5))

# Normal Q-Q plot of residuals
qq_plot <- ggplot(data = data.frame(Residuals = residuals), aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot of Residuals",x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plots
residual_plot
qq_plot


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
