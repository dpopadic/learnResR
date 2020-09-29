# Volatility Modelling using GARCH Models
# ---------------------------------------


# simple garch(1, 1) simulation example:
ret <- rnorm(200) / 100
alpha <- 0.1
beta <- 0.8
omega <- var(ret) * (1 - alpha - beta)
e <- (ret - mean(ret))^2
# predictions
n <- length(ret)
sigma_hat <- rep(NA, n)
sigma_hat[1] <- var(ret) # initialise with sample var
# constant prediction error
for (t in 2:n)
  sigma_hat[t] <- omega + alpha * e[t-1] + beta * sigma_hat[t-1]

# recursive prediction error
for (t in 2:n)
  sigma_hat[t] <- omega + alpha * (ret[t-1] - mean(ret[1:(t-1)]))^2 + beta * sigma_hat[t-1]


# intuition: garch estimates the volatility using prediction errors (shocks) & previous 
# volatility predictions
# - alpha determines the weight given to shocks
# - beta determines the importance of previous volatilities
# - these parameters are usually estimated by maximum-likelihood (finding the parameters
#   for which the model is most likely to have generated the observed data)

# determine whether there are volatility clusters (via autocorrelation):
par(mfrow = c(2,1))
plot(abs(sqrt(e)), type = "l")
acf(abs(sqrt(e)))


# rugarch-pkg volatility modelling steps:
# 1. specify the garch model that one wants to use: ugarchspec()
# 2. estimate the model on the data with ugarchfit()
# 3. make predictions with estimated model with ugarchforecast()




