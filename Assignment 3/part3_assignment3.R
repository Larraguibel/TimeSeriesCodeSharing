# Add dependencies:
rm(list=ls())
setwd("C:/Users/ludvi/Desktop/Dutten/Semester_10/Time-Series-Analysis/assignment3")
library(marima2)
library(ggplot2)
library(gridExtra)

# Functions for marima2:
files <- dir("functions",full.names=TRUE)
for(i in 1:length(files)) source(files[i])

# Get data:
X <- read.table("data/experiment1.csv", sep=",", header=TRUE)
X$t <- X$t - X$t[1] # Centre data



# Cross validation:
train_val <- round(0.5*nrow(X))
train_val
X_train <- X[1:train_val, ]
X_train$t <- X_train$t - X_train$t[1] # Centre data
X_test <- X[(train_val+1):nrow(X), ]
X_test$t <- X_test$t - X_test$t[1] # Centre data

#######################
# AUXILIARY FUNCTIONS #
#######################

# Add lags:
# Add the lags to X
add_lags_to_X <- function(X, maxlag) {
  for(i in 1:maxlag){
    tmp <- lagdf(X[ ,2:6], i) # Segment only to add lags onto data, not time
    names(tmp) <- paste0(names(tmp),".l",i)
    X <- cbind(X, tmp)
  }
  return(X)
}

# So we should have a burn-in, e.g. of 5 steps (in priciple the max order of the models)
score <- function(res, n, p, nburnin, penalty=2){
  # How many to use
  nuse <- n - nburnin
  # Take the nuse last values
  res <- res[(length(res)-nuse+1):length(res)]
  # Calc
  sd <- sqrt(sum((res - mean(res))^2) / length(res) )
  loglik <- sum(log(dnorm(res, sd=sd)))
  # Return
  return(penalty*p - 2*loglik)
}

get_aic <- function(fit, D, n) {
  p <- sum(fit$ar.estimates[ , ,-1] != 0) + sum(fit$ma.estimates[ , ,-1] != 0) # Find number of params
  (p <- p + 1)
  score(fit$residuals, n=nrow(D), p=p, nburnin=n)
}

# Remove NA observations:
remove_NA <- function(X, num_lag) {
  
  columns <- c("Ta", "Tinner", "Touter", "Pinner", "Pouter")
  
  # Iterate through lagged columns:
  for(i in 1:num_lag) {
    
    lag_suffix <- paste0(".l", i) # Generate a lag suffix
    
    for(col in columns) {
      lagged_col <- paste0(col, lag_suffix)
      NA_indices <- which(is.na(X[[lagged_col]])) # Get NA indices
      
      # Replace if available:
      if(length(NA_indices) > 0) {
        X[[lagged_col]][NA_indices] <- 0
      }
    }
  }
  
  return(X)
}

# Make predictions:
multi_step_pred <- function(X_pred, fit, coupled=FALSE) {
  val <- predict(fit, X_pred, nstep=nrow(X_pred)-1) # Do multi-step prediction
  val$forecasts
  yhat <- val$forecasts[1, ] #
  se <- sqrt(val$pred.var[1,1, ])
  pred_plot <- (ggplot() + geom_line(aes(x=1:length(yhat), y=yhat), col="blue") + geom_line(aes(x=1:length(X_pred$Tinner), y=X_pred$Tinner), col="red"))
  
  if(coupled == TRUE) { # Add to pred_plot if coupled system
    yhat2 <- val$forecasts[2, ]
    pred_plot <- (pred_plot + geom_line(aes(x=1:length(yhat2), y=yhat2), col="blue") + geom_line(aes(x=1:length(X_pred$Touter), y=X_pred$Touter), col="red"))
  }
  return(pred_plot)
}

step_pred <- function(fit, X_hat, with_Tinner=FALSE) {
  val <- predict(fit, X_hat, nstep=nrow(X_hat)-1) # Do multi-step prediction
  print(X_hat[-1:-10, ])
  yhat <- val$forecasts[1, ] #
  se <- sqrt(val$pred.var[1,1, ])
  step_plot <- (ggplot() +
                   geom_line(aes(x=1:length(yhat), y=yhat), col="blue") +
                   geom_line(aes(x=1:length(yhat), y=yhat - qnorm(0.975)*se), col="black", linetype="dashed") +
                   geom_line(aes(x=1:length(yhat), y=yhat + qnorm(0.975)*se), col="black", linetype="dashed") +
                 ggtitle("0-300W step (length = 10 datapoints)"))
  if(with_Tinner == TRUE) {
    step_plot <- (step_plot + geom_line(aes(x=1:length(X$Tinner), y=X$Tinner), col="red"))
  }
  return(step_plot)
}

######################
###### PART 3.1 ######
######################

# Problem -> utilize marima package to estimate the ARX model below:
  #  Tinner = -0.99375 Tinner.l1
# Are the residuals equal?

# Compute 1 lag for the random variables:
num_lags <- 1
X_lm <- add_lags_to_X(X, num_lags)
# X_lm <-remove_NA(X_lm, num_lags)
X_lm
# Make an ARX model:
fit <- lm(ARX(c("Tinner"), c("Tinner", "Ta"), 1), data=X_lm)

# Get a summary of the model:
summary(fit)
validate(fit)

# Marima version --->:
fit2 <- marima("Tinner ~ AR(1) + Ta(1)", data=X)
summary(fit2)

fit2$residuals[1, ]
fit2$residuals[1] <- 0
diff <- abs(fit$residuals - fit2$residuals[1,2:length(fit2$residuals[1,])])
plot(diff)

# 1 to 1 to the results we got in 2.1. Only difference is the signs, whereby the AR part is negative rather
# than positive for the other result.

# Residuals are basically identical, given the scale of them.

######################
###### PART 3.2 ######
######################

# ARMAX(1) fit (based on part 2.3):
# armax_fit <- marima("Tinner ~ AR(1) + Ta(1) + MA(1)", data=X)
armax_fit <- marima("Tinner ~ AR(1) + Ta(1) + MA(1)", data=X)

summary(armax_fit)

# In general - on P-values:
# Low P-values for the estimated values for the lags suggest, that these variables
# have a strong relationship with the variable Tinner. Furthermore, 3 stars emphasize
# this point, as '***' indicates high statistical significance.

# From this, it is perfectly clear, Ta(1) and Pouter(1) have the least significant impact on the ARMAX
# model.

# Model validation:
validate(armax_fit)

# Compare with arx_fit:
arx_fit <- marima("Tinner ~ AR(1) + Ta(1) + Touter(1)", data=X_orig)
summary(arx_fit)
validate(arx_fit)

# Here we notice, that the residuals aren't dependent on the air temperature, as the values don't fade.
# Similarly, we notice, that the ACF and PACF indicate that the residuals are white noise

# For the simple ARMAX model:
# The ACF of the residuals clearly gets a variational sinusoid shape rather than the decaying exponential
# sinusoid as for the model ARX model. Furthermore, the PACF is shifted right.

######################
###### PART 3.3 ######
######################

# Find optimal fit (results reported below):
# baseline_fit <- marima("Tinner ~ AR(1) + Touter(1) + Ta(1) + Pinner(1) + Pouter(1) + MA(1)", data=X_orig)
fit <- marima("Tinner ~ AR(1:5) + Touter(1:5) + Ta(1:5) + Pinner(1:5) + Pouter(1:5) + MA(1:5)", data=X_train)
  # This seems to be the best model. Massive AIC score (4880.7) ??? Multi-pred showcases its accuracy.
  # The model does however HIGHLY overfit. 
# Models to keep:
# fit <- marima("Tinner ~ AR(1:3) +Ta(1:3) + Touter(1:3) + MA(1:3)", data=X_train, penalty=2)
# fit <- marima("Tinner ~ AR(1:3) + Pinner(1:3) + MA(1:3)", data=X_train, penalty=2)
# fit <- marima("Tinner ~ AR(1) + Pinner(1) + MA(1)", data=X)

step_pred(fit, Xs3)

# Model of choice:
fit2 <- marima("Tinner ~ AR(1) +Ta(1:3) + Touter(1:3) + Pinner(1:2) + MA(1)", data=X_train, penalty=2)
fit <- marima("Tinner ~ AR(1:3) +Ta(1:3) + Touter(1:3) + Pinner(1:3) + MA(1:3)", data=X_train, penalty=2)
# fit <- marima("Tinner ~ AR(1) + MA(1)", data=X_train, penalty=2)

fit <- marima("Tinner ~ AR(1:2) + Ta(1:2) + Touter(1:2) + Pinner(1:2) + Pouter(1:2) + MA(1:2)", data=X, penalty=2)
fit2 <- marima("Tinner ~ AR(1:2) + Ta(1:2) + Touter(1:2) + Pinner(1:2) + MA(1:2)", data=X, penalty=2)
summary(fit)
summary(fit2)
validate(fit)
validate(fit2)
# Get AIC score:
get_aic(fit, X_train, 5)
get_aic(fit2, X_train, 5)

# Results:
  
  # Order 1:
    # Baseline model: 4873.948
    # Baseline - Pouter: 5050.313
    # Baseline - Pinner: 5050.173
    # Baseline - Ta: 4933.348
    # Baseline - Touter: 3893.226
    # ARMA model: 257.65 ??????

# Make prediction:
par(mfrow=c(2, 2))
multi_step_pred(X, fit)
multi_step_pred(X, fit2)
plot1 <- multi_step_pred(X_test, fit)
plot1
plot2 <- multi_step_pred(X_test, fit2)
grid.arrange(plot1, plot2, ncol = 2)

fit <- marima("Tinner ~ AR(1:2) + Ta(1:2) + Touter(1:2) + Pinner(1:2) + Pouter(1:2) + MA(1:2)", data=X_train, penalty=2)
summary(fit)
validate(fit)
multi_step_pred(X, fit, FALSE)

# First of all, an important note:
  # We chose utilize cross validation measures, by dividing our dataset into a training and test dataset, where
  # 65% was used for training and the other 35% was used for testing. 
  # This allowed us to validate, that our model did not overfit to the data, thus confirming that our model performed
  # correctly within the domain.

# To methods that we utilized:
  
  # Used a baseline ARMAX order 1 model, whereby all exogeneous inputs were included. We set penalty = 2 such, that
  # we may lessen the model complexity as much as possible, given the backward propagation of the step(fit) function.
  # As well as this, we also utilized summary(fit) to investigate what variables were significant to the time series.
  # Here, we noticed that Touter was deemed not significant, which we found intuitively dissatisfying.
  # Increasing the order of the models (up to order-six), we got an okay fit, however, it was not capable of capturing ambient temperature
  # features very well.

  # The other methodology was utilizing a form of 'domain expertize'. Intuitively we know, that the input that
  # affect the inner temperature the least should be Pouter, as the effect of this would better be modelled as
  # an effect of the surrounding temperatures. Using this knowledge, we utilized a baseline model where we only
  # excluded Pouter. Increasing the order of this model, we found a better model than that we had found at the first
  # round of model iteration.

######################
###### PART 3.6 ######
######################

# Simulate step responses!! :))

input <- "Pinner" # This is what we want to simulate
Xs <- X[ ,2:6]
#Xs[ ,2:6] <- 0
Xs$Pinner <- 0 # Keep power at inner at 0
Xs$Pouter <- 0
#
Xs$Tinner <- 20
Xs$Touter <- 20
Xs$Ta <- 20 # Steady all temperatures at 20

# Make step:
Xs1 <- Xs
Xs1[100:121,input] <- Xs1[100:121,input] + 100 # from 10th -> end of Pinner add 100
Xs1

# Do multi-step prediction on Xs and plot:
val <- predict(fit, Xs1, nstep=nrow(Xs1)-1) # Do multi-step prediction
yhat <- val$forecasts[1, ] #
se <- sqrt(val$pred.var[1,1, ])
step_plot <- (ggplot() +
              geom_line(aes(x=1:length(yhat), y=yhat), col="blue") +
              geom_line(aes(x=1:length(X$Tinner), y=X$Tinner), col="red") +
              geom_line(aes(x=1:length(yhat), y=yhat - qnorm(0.975)*se), col="black", linetype="dashed") +
              geom_line(aes(x=1:length(yhat), y=yhat + qnorm(0.975)*se), col="black", linetype="dashed") +
              ggtitle("0-100W step (length = 20 datapoints)"))
step_plot

# Add extra step type, where output = 300:
Xs2 <- Xs
Xs2[100:110,input] <- Xs2[100:110,input] + 300

# Make prediction and plot:
val2 <- predict(fit, Xs2, nstep=nrow(Xs2)-1) # Do multi-step prediction
yhat2 <- val2$forecasts[1, ] #
se2 <- sqrt(val2$pred.var[1,1, ])

step_plot2 <- step_pred(fit, Xs2)
step_plot2

# General step from 0-100:
Xs3 <- Xs
Xs3[400:nrow(Xs3), ]
Xs3[200:(nrow(Xs)-100),input] <- Xs3[200:(nrow(Xs)-100),input] + 100
Xs3[200:nrow(Xs),input] <- Xs3[200:nrow(Xs),input] + 100
step_plot3 <- step_pred(fit, Xs3)
step_plot3


# Check steady state gain:
gain.marima <- function(fit, output, input){
  tbl <- summary(fit)[[output]]
  -sum(tbl[grep(input,tbl$Name), "Estimate"]) / sum(1,tbl[grep("AR",tbl$Name), "Estimate"])
}
gain.marima(fit, "Tinner", "Pinner")

######################
###### PART 4.1 ######
######################

outer_fit <- marima("Touter ~ AR(1:5) + Tinner(1:5) + Pouter(1:5) + MA(1:5)", data=X_train, penalty=2)

summary(outer_fit)
validate(outer_fit)


# Check predictions:
val <- predict(outer_fit, X_test, nstep=nrow(X_test)-1) # Do multi-step prediction
yhat <- val$forecasts[1, ] #
se <- sqrt(val$pred.var[1,1, ])
pred_plot <- (ggplot() + geom_line(aes(x=1:length(yhat), y=yhat), col="blue") + geom_line(aes(x=1:length(X_test$Touter), y=X_test$Touter), col="red"))
pred_plot

######################
###### PART 5.1 ######
######################

# Get model strings:
(inner_str <- "Tinner ~ AR(1:3) +Ta(1:3) + Touter(1:3) + Pinner(1:3) + MA(1:3)")
(outer_str <- "Touter ~ AR(1:5) + Tinner(1:5) + Pouter(1:5) + MA(1:5)")

# Fit model:
multi_fit <- marima(inner_str,
                    outer_str, data=X_train, penalty=0)
summary(multi_fit)
validate(multi_fit)
multi_step_pred(X_test, multi_fit, TRUE)

######################
###### PART 5.2 ######
######################

# Get model strings:
(inner_str <- "Tinner ~ AR(1) + Ta(1) + Touter(1) + Pouter(1) + Pinner(1) + MA(1)")
(outer_str <- "Touter ~ AR(1) + Ta(1) + Tinner(1) + Pouter(1) + Pinner(1) + MA(1)")

# Fit model:
multi_fit <- marima(inner_str,
                    outer_str, data=X_train, penalty=2)
summary(multi_fit)
validate(multi_fit)
multi_step_pred(X_test, multi_fit, TRUE)
