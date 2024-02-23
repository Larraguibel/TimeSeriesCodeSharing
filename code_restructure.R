# Assignment 1
library(ggplot2)
library(fpp2)
library(dplyr)
library(tidyverse)
library(readxl)

######################################
##### Load data - generalization #####
######################################
# Create the vector of time, x
n<-59
x1=2018.
x59=x1+n/12-1/12
tstep=1/12
x=seq(x1,x59,by=tstep)

# Load training data & the dataframe of the training data and time:
parent_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path)) # Put data in parent folder of repository
data_path <- paste0(parent_dir, "/DST_BIL54_train.xlsx")
test_path <- paste0(parent_dir, "/DST_BIL54_test.xlsx")
# Conventional:
data_path <- "C:/Users/margr/OneDrive - Danmarks Tekniske Universitet/Skrivebord/DTU/time-series-analysis/assignments/assignment1/DST_BIL54_train.xlsx"
data <- t(readxl::read_excel(data_path, sheet = 1, col_names =TRUE)[1,2:(n+1)])
train_data <- data.frame(time = x, Drivmidler_i_alt=data)

# Create the vector of forecasting time, xtest:
x60=x59+1/12
x71=x59+1
xtest=seq(x60,x71,by=tstep)
Xtest <- cbind(1, xtest)

# Load test data:
check_data <- t(readxl::read_excel(test_path, sheet = 1, col_names =TRUE)[1,2:13])
test_data <- data.frame(Time = xtest,Drivmidler_i_alt=check_data)

#####################
#####  Part 1 #######  
#####################

#########
## 1.1 ##
#########

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

# L <- cbind(c(1,0), c(1,1))
L <- matrix(c(1., 0., 1., 1.), byrow = TRUE, nrow = 2)
Linv <- solve(L)
f.0 <- c(1,0)

# Print values:
L
Linv
f.0

#########
## 4.2 ##
#########

# Our x vector starts from 2018. We need to translate it into the negative
# part to the axis.

j <- seq(- (length(train_data) - 1), 0, 1)

f <- function(j) rbind(1, j)

# F function for arbitrary N and lambda values:
F_N <- function(n, lambda) { # If Lambda initialized at 1 -> has no influence, just notation
    if(n < 0 || lambda > 1 || lambda <= 0) {
      return(0)
    }
  
    F_val <- (lambda^0) * f(0)%*%t(f(0))
    
    for (i in 1:n) {
      F_val <- F_val + (lambda^(i)) * f(-i)%*%t(f(-i))
    }
    return(F_val)
}
res <- F_N(1, lambda = 1)
res

#########
## 4.3 ##
#########

# Compute recursive h_N:
h_N <- function(n, lambda, Linv, Y) {
  if(N <= 0 || lambda <= 0 || lambda > 1){
    return(0)
  }

  result <- f(0) * Y[n]

  for(i in 1:n) {
    result <- result + lambda * Linv %*% h_N + f(-i) * Y[n - i]
  }
  
  return(result)
}

h_N(10, lambda=0.9, y)

#############################
## Part 4.3  (Alternative) ##
#############################
ytrain <- train_data$'Drivmidler_i_alt'
ytrain
j <- matrix(seq(-58, 0), ncol = 1)
y <- matrix(ytrain)
f <- function(j) rbind(1, j)
L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)
Linv <- solve(L)

F_H_theta_calc <- function(N, lambda, Y) {
  
  # Define F.1 and h.1
  F.N <- f(0) %*% t(f(0))
  h.N <- f(0) * Y[1]
  
  for(i in 2:N) {
    F.N <- F.N + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
    h.N <- lambda * Linv %*% h.N + f(0) * Y[i]
  }
  
  theta.N <- solve(F.N) %*% h.N

  return(list(F.N = F.N, h.N = h.N, theta.N = theta.N))
}

# Initial F_N:
i <- 10
outs <- F_H_theta_calc(N=i, lambda=0.9, Y=y)
outs$F.N # Answer to 4.3
outs$h.N # Answer to 4.3

# Plot predictions up to i:
yhat_N <- t(f(-(i-1):(n-i))) %*% outs$theta.N
yhat_N
df <- data.frame(j = j, y = y)
ggplot(df, aes(x=j, y=y)) +
  geom_point() + 
  geom_point(data=df[1:i,], col="blue") +
  geom_line(data=df[1:i,], aes(y=yhat_N[1:i]), col="blue") +
  xlim(-60, 0) + ylim(2300000, 2700000)

#################
## 4.4 and 4.5 ##
#################

F_H_pred <- function(N, lambda, Y, l) {
  
  # Define F.1 and h.1
  F_new <- f(0) %*% t(f(0))
  h_new <- f(0) * Y[1]
  # F_new <- F_new + lambda^(1) * f(-(1)) %*% t(f(-(1)))
  # h_new <- lambda * Linv %*% h_new + f(0) * Y[2]
  
  
  # Empty list:
  l_steps <- c()

  for(i in 2:N) {
    F_new <- F_new + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
    h_new <- lambda * Linv %*% h_new + f(0) * Y[i]
    theta_new <- solve(F_new) %*% h_new

    yhat_new <- t(f(-(i-1):(N-i)))%*%theta_new

    # Predict l steps ahead if i + l <= N
    if(i+l <= N) {
      l_steps <- c(l_steps, yhat_new[i+l])
    }
  }
  
  theta_x <- solve(F_new) %*% h_new

  return(list(F_new = F_new, h_new = h_new, theta_new = theta_x, l_steps = matrix(l_steps)))
}

i <- 59

# l = 1:
l <- 1
pred_res_1 <- F_H_pred(N=i, lambda=0.05, Y=y, l=l)

# Plot pred_res$l_steps together with the data:
start_1 <- length(df[,1]) - length(pred_res_1$l_steps) + 1 # 1 indexed
plot_N <- ggplot(df, aes(x=j, y=y)) +
    geom_point() +
    geom_point(data=df[start_1:length(df[,1]),], aes(y = pred_res_1$l_steps), col="blue", size=3) +
    xlim(-60, 12) + ylim(2300000, 2700000)
plot_N

# l = 6:
l <- 6
pred_res_6 <- F_H_pred(N=i, lambda=0.9, Y=y, l=l)

# Plot pred_res$l_steps together with the data:
start_6 <- length(df[,1]) - length(pred_res_6$l_steps) + 1
plot_N <- ggplot(df, aes(x=j, y=y)) +
    geom_point() +
    geom_point(data=df[start_6:length(df[,1]),], aes(y = pred_res_6$l_steps), col="blue", size=4) +
    xlim(-60, 12) + ylim(2300000, 2700000)
plot_N

# l = 12:
l <- 12
pred_res_12 <- F_H_pred(N=i, lambda=0.9, Y=y, l=l)

# Plot pred_res$l_steps together with the data:
start_12 <- length(df[,1]) - length(pred_res_12$l_steps) + 1
plot_N <- ggplot(df, aes(x=j, y=y)) +
    geom_point() +
    geom_point(data=df[start_12:length(df[,1]),], aes(y = pred_res_12$l_steps), col="blue", size=4) +
    xlim(-60, 12) + ylim(2300000, 2700000)
plot_N

# Plot all together in different colours:
plot_N <- ggplot(df, aes(x=j, y=y)) +
    geom_point() +
    geom_point(data=df[start_1:length(df[,1]),], aes(y = pred_res_1$l_steps), col="blue", size=3) +
    geom_point(data=df[start_6:length(df[,1]),], aes(y = pred_res_6$l_steps), col="red", size=3) +
    geom_point(data=df[start_12:length(df[,1]),], aes(y = pred_res_12$l_steps), col="green", size=3) +
    xlim(-60, 12) + ylim(2300000, 2700000)
plot_N

###########################
## 4.6, 4.7, 4.8 and 4.9 ##
###########################

# Repeat iterative predictions with lambda = 0.55, 0.56, ... 0.95 and calculate RMSE for each forecast-horizon (l = 1, l = 6, l = 12) and for each lambda value:
iter_pred <- function(N, lambda_lst, Y, l_lst) {
  
  rmse_lambda <- list(one_step = numeric(length(lambda_lst)),
                      six_step = numeric(length(lambda_lst)),
                      twelve_step = numeric(length(lambda_lst)))
  
  for(l in l_lst) {

    for(lambda_idx in seq_along(lambda_lst)) {
      lambda <- lambda_lst[lambda_idx]
      rse_lambda <- 0
      # Define F.1 and h.1
      F_new <- f(0) %*% t(f(0))
      h_new <- f(0) * Y[1]
      
      for(i in 2:10) {
        F_new <- F_new + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
        h_new <- lambda * Linv %*% h_new + f(0) * Y[i]
      }

      for(j in 11:N) {
        F_new <- F_new + lambda^(j - 1) * f(-(j - 1)) %*% t(f(-(j - 1)))
        h_new <- lambda * Linv %*% h_new + f(0) * Y[j]
        theta_new <- solve(F_new) %*% h_new

        yhat_new <- t(f(-(j-1):(N-j)))%*%theta_new

        # Predict l steps ahead if j + l <= N
        if(j+l <= N) {
          rse_lambda <- rse_lambda + sqrt((Y[(j+l)] - yhat_new[j+l])^2)
        }
      }
      
      rmse_lambda[[paste0(l, "_step")]][lambda_idx] <- rse_lambda 
    }
  }

  return(rmse_lambda)
}

l_lst <- c(1, 6, 12)
lambda_lst <- seq(0.55, 0.95, 0.01)
rmse_lambda <- iter_pred(N=i, lambda_lst=lambda_lst, Y=y, l_lst=l_lst)
rmse_lambda
rmse_lambda[[paste0(6, "_step")]]

# Plot RMSE for each lambda value and forecast-horizon:
plot_RMSE <- function(rmse_lambda) {
  l_lst <- c(1, 6, 12)
  lambda_lst <- seq(0.55, 0.95, 0.01)
  
  par(mfrow = c(1, 3))  # Set the layout to have 1 row and 3 columns
  
  for (l in l_lst) {
    rmse <- rmse_lambda[[paste0(l, "_step")]]
    plot(lambda_lst, rmse, type = "l", xlab = "Lambda", ylab = "RMSE", main = paste("RMSE for l =", l))
  }
}

plot_RMSE(rmse_lambda)
rmse_lambda[[paste0(1, "_step")]]

# Minimum index:
min_idx <- function(rmse_lambda, l) {
  return(which.min(rmse_lambda[[paste0(l, "_step")]]))
}

# Minimum values:
best_lambda <- function(rmse_lambda, l) {
  return(0.55 + (min_idx(rmse_lambda, l) - 1) * 0.01)
}

best_lambda(rmse_lambda, 1)
best_lambda(rmse_lambda, 6)
best_lambda(rmse_lambda, 12)


# What this tells us:
  # The optimal lambda value depends on the forecast-horizon. For l = 1, it is revealed, that a low lambda value is optimal (lambda = 0.55),
  # whilst for both l = 6 and l = 12, these reach a minima when lambda = 0.95. This is interesting, as it suggests that the optimal lambda
  # value is not the same for all forecast-horizons. This is likely due to the fact that the optimal lambda value depends on the underlying
  # trend in the data, and as such, the optimal lambda value is not the same for all forecast-horizons.

# Checking the plot of lambda = 0.55, l = 1:
pred_res_1 <- F_H_pred(N=i, lambda=0.55, Y=y, l=1)
plot_N <- ggplot(df, aes(x=j, y=y)) +
    geom_point() +
    geom_point(data=df[1:(length(pred_res_1$l_steps)),], aes(y = pred_res_1$l_steps), col="blue", size=2) +
    xlim(-60, 12) + ylim(2300000, 2700000)
plot_N

##########
## 4.10 ##
##########

# Sum of weights when lambda = 0.5:
lambda <- 0.50
lambda <- 0.7
sum(lambda^(1:length(y))) # = 1 -> problematic!

# The sum of weights *must* be larger than the number of parameters estimated. As we're estimating a single parameter, the sum of weights must be larger than 1, which
# is not the case when lambda <= 0.5.

##########
## 4.11 ##
##########

naive_persistence_model <- function(N, Y) {
  
  rse <- 0
  for(j in 11:N) {
    # Predict l steps ahead if j + l <= N
    if(j+1 <= N) {
      rse <- rse + sqrt((Y[(j+1)] - Y[j])^2)
    }
  }

  return(rse)
}

# Plot data and y displaced by one to the right (naive persistence model):
y_plot <- y
plot_N <- ggplot(df, aes(x=j, y=y)) +
    geom_point() +
    geom_point(data=df[2:length(df[,1]),], aes(y = matrix(y_plot[1:(length(y_plot)-1)])), col="blue", size=2) +
    xlim(-60, 12) + ylim(2300000, 2700000)
plot_N

# Retrieve RMSE for the model:
npm_rmse <- naive_persistence_model(N=59, Y=y)
npm_rmse
min_idx(rmse_lambda, 1)
rmse_lambda$'1_step'[1]
# Difference:
one_step_pred_vs_npm <- npm_rmse - rmse_lambda$'1_step'[min_idx(rmse_lambda, 1)]
one_step_pred_vs_npm # Given, that the error is positive, this showcases that our non-naive WLS predictions create superior predictions than those of a NPM

##########
## 4.12 ##
##########

# Merge training and test dataset:
x_train_test <- seq(x1, x71, by=tstep)
ytest <- test_data$'Drivmidler_i_alt'
ytest
y_train_test <- matrix(c(y, ytest))

y_train_test
y_df <- data.frame(x=x_train_test, y=y_train_test)
y_df

plot_y_df <- ggplot(y_df, aes(x=x, y=y)) +
  geom_point() +
  xlim(2018, 2024) + ylim(2300000, 2700000)
plot_y_df

test_forecast <- function(N, lambda, Y, l) {
  
  # Define F.1 and h.1
  F_new <- f(0) %*% t(f(0))
  h_new <- f(0) * Y[1]
  
  # Empty list:
  l_steps <- c()

  for(i in 2:N) {
    F_new <- F_new + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
    h_new <- lambda * Linv %*% h_new + f(0) * Y[i]
  }
  
  theta_new <- solve(F_new) %*% h_new

  # Create prediction:
  yhat <- t(f(-(length(Y)-1):(N-i))) %*% theta_new

  return(yhat)
}

preds_one <- test_forecast(N=59, lambda=best_lambda(rmse_lambda, 1), Y=y_train_test, l=1)

# Plot preds_one and y_df together:
plot_y_df <- ggplot(y_df, aes(x=x, y=y)) +
  geom_point() +
  geom_point(data=y_df[1:length(y_df[,1]),], aes(y = preds_one), col="blue", size=2) +
  xlim(2018, 2024) + ylim(2300000, 2700000)
plot_y_df
