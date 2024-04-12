## Source the files in the "functions" folder
# "/Users/terriblepollo/Desktop/Exchange Courses/Time Series Analysis/Assignments/Assignment 3"
files <- dir("functions",full.names=TRUE)
for(i in 1:length(files)) source(files[i])
files
# Read data and create lag vectors

X <- read.table("data/experiment1.csv", sep=",", header=TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]
lagdf(X[ ,2:6], 1)

# Add the lags to X
maxlag <- 10
for(i in 1:maxlag){
  tmp <- lagdf(X[ ,2:6], i)
  names(tmp) <- paste0(names(tmp),".l",i)
  X <- cbind(X, tmp)
}

##########################
##### Exercise 2.1 #######
##########################

ARX_1_string = ARX("Tinner", c("Ta"), 1)
ARX_1 = lm(ARX_1_string, data=X)
summary(ARX_1)

plot(X$t, X$Tinner, type = "l", xlab = "Time", ylab = "Tinner", main = "Tinner vs. Time")
pred_valuesARX1 = c(NA_character_, fitted.values(ARX_1))
lines(X$t,pred_valuesARX1, col = "red")
legend("topleft", legend = c("Tinner", "ARX(1)"), col = c("black", "red"), lty = 1)

##########################
##### Exercise 2.2 #######
##########################

validate(ARX_1)
ccf(ARX_1$residuals, X$Ta)

##########################
##### Exercise 2.3 #######
##########################

# Finding relevant variables
first_fit = lm("Tinner ~ 0 + Ta + Pouter + Touter + Pinner", data=X)
summary(first_fit)
validate(first_fit)
step_fit = step(first_fit, direciton="backward", scope="Tinner ~ 0")

second_fit = lm("Tinner ~ 0 + Ta + Touter + Pinner", data=X)
step_fit = step(second_fit, direciton="backward", scope="Tinner ~ 0")
summary(step_fit)
validate(step_fit)

third_fit = lm("Tinner ~ 0 + Ta + Touter + Pouter", data=X)
step_fit = step(third_fit, direciton="backward", scope="Tinner ~ 0")
summary(step_fit)
validate(step_fit)

# Fit ARX model
fit = step(lm(ARX("Tinner", c("Pinner", "Ta", "Touter"), 1:5), data=X), direction="backward", scope="Tinner ~ 0", k=0.05)
summary(fit)
validate(fit)
fit
a = AIC(fit)


ARX("Tinner", c("Pinner", "Ta", "Touter"), 1:5)

anova(fit)
