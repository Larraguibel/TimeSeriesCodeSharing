# Function: prune_variables
# Description: This function removes non-significant variables (with p-value > 0.05) 
#              and variables that do not change the AIC value from a linear regression model.
# Parameters:
#   - model: A linear regression model object (e.g., created using lm() function).
# Returns:
#   - Updated linear regression model object after removing non-significant variables and those that do not change AIC.

