##################
###Assignment 2###
##################

library(ggplot2)

#########
###1.1###
#########

# Define AR(2) solver function
ar2solver <- function(p1, p2) {
  # Define the coefficients of the characteristic polynomial
  poly_coeff <- c(1, p1, p2)
  # Calculate the roots of the characteristic polynomial
  roots <- polyroot(poly_coeff)
  # Calculate the magnitude of the roots
  mag <- Mod(roots)
  # Calculate the reciprocal of the magnitude
  absrecip <- 1/mag
  # Create a data frame with the results
  result_df <- data.frame(roots = roots, magnitude = mag, abs.recip = absrecip)
  return(result_df)
}
# Set values for p1 and p2
p1 <- -0.7 ########################### change this value for the questions 1.7-1.10
p2 <- -0.2
# Call the AR(2) solver function
result <- ar2solver(p1, p2)
# Define points on the unit circle
theta <- seq(0, 2 * pi, length.out = 100)
x <- cos(theta)
y <- sin(theta)
# Plot
plotting<-ggplot(result, aes(x = Re(roots), y =  Im(roots)))+
  geom_point(color = "red", shape =16, size=2)+
  geom_path(data = data.frame(x = x, y = y), aes(x = x, y = y), col = "blue", size = 0.5) +
  labs(x = 'Real', y = 'Imaginary')+
  coord_fixed(ratio = 1,xlim = c(-5, 5), ylim = c(-2, 2))  
plotting

#########
###1.4###
#########

# Yule-Walker equations for AR(2) autocorrelation
acf_ar2 <- function(phi1, phi2, k) {
  if (k == 1) {
    return(-phi1 / (1 + phi2))
  } else if (k == 2) {
    rho1 <- acf_ar2(phi1, phi2, 1)
    return(-phi1 * rho1 - phi2)
  } else if (k > 2) {
    rho1 <- acf_ar2(phi1, phi2, k-1)
    rho2 <- acf_ar2(phi1, phi2, k-2)
    return(-phi1 * rho1 - phi2 * rho2)
  } else {
    return(NA)
  }
}
# number of lags
nlag<-30
# Calculate autocorrelations up to nlag
autocorrelations <- sapply(1:nlag, function(k) acf_ar2(p1, p2, k))

# Plot autocorrelations
plot(1:nlag, autocorrelations, type = 'h', lwd = 2,
     xlab = 'k', ylab = 'Autocorrelation ρ(κ)', main = paste('Autocorrelation ρ(κ) for the AR(2)', 
                                                             '\n(φ1=', as.character(p1), ', φ2=', as.character(p2),', nlag=', as.character(nlag), ')'))
#########
###1.5###
#########

set.seed(12)
# define the number of observations
n_obs <- 200
coef<-c(-p1,-p2)
# Simulate 5 realizations of the AR(2) process
simulations <- replicate(5, arima.sim(model = list(ar = coef), n = n_obs))
# Create a data frame for plotting
plot_data <- data.frame(value = c(simulations),
                        time = 1:n_obs,
                        realization = rep(1:5, each = n_obs))
# Plot the realizations
  ggplot(plot_data, aes(x = time, y = value, group = realization, color = factor(realization))) +
    geom_line() +
    labs(x = 'Obs', y = 'X', 
         title = paste('5 Realizations of AR(2) Process', 
                       '\n(φ1=', as.character(p1), ', φ2=', as.character(p2), ')'),
         color = "Realization" ) +
    theme_minimal()
  
#########  
###1.6###
#########
  
# Calculate empirical ACF for each simulation
empirical_acf <- lapply(data.frame(simulations), function(column) acf(column, lag.max = 30, plot = FALSE)$acf)
# comment the result
# Plot empirical ACFs and your process ACF
plot(1:30, autocorrelations, type = "h", lwd = 3.5, col = "black", 
     xlab = "Lag", ylab = "Autocorrelation", main = paste("Empirical ACF of simulations vs. Autocorrelation ρ(k)",'\n(φ1=', as.character(p1), ', φ2=', as.character(p2),', nlag=', as.character(nlag), ')'),ylim = c(-1, 1))
for (i in 1:5) {
  lines(0:30, empirical_acf[[paste0("X",as.character(i))]], col = rainbow(5)[i], lwd = 0.8)
}
legend("bottomright", legend = c("autocorrelation ρ(κ)", paste("emp. ACF of sim.", 1:5)), 
       lty = c(1, rep(1, 5)), lwd = c(2, rep(2, 5)), 
       col = c("black", rainbow(5)),cex = 0.5)


# would you recommend always plotting the time series databor just it provide sufficient information just to examine the ACF?
# Plot autocorrelation line


