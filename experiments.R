# RANDOM EXPERIMENTS
# ------------------

# --- spearman vs pearson vs kendall correlation
# assumptions
# pearson: continuous variables, no outliers, linearity & homoscedastictiy
# spearman: independent observations, ordinal variables, monotonic relationship
# kendall: same as spearman
# features:
# - kendall preferred to spearman (more robust) & usually has lower values (+/- 0.3 is significant)
# - kendall O(n^2) vs spearman O(n*logn(n))
x <- 1:100  
y <- exp(x)
stats::cor(x, y, method = "spearman") # 1
stats::cor(x, y, method = "pearson") # 0.25
stats::cor(x, y, method = "kendall") # 1


# --- logloss (cross-entropy) vs hit-ratio
y <- c(1, 0, 1, 1, 0)
p <- c(0.9, 0.9, 0.9, 0.9, 0.9)
p <- c(0.51, 0.51, 0.9, 0.51, 0.51)
p <- runif(length(y))

# - probability 50% -> -log(0.5)=0.693
# - perfect model: log-loss=0
logloss <- function(y, p)
  1 / length(y) * sum(-1 * (y * log(p) + (1 - y) * log(1 - p)), na.rm = TRUE)

hr <- function(y)
  sum(y == 1) / length(y)

logloss(y=y, p=p)
hr(y=y)


p <- seq(from=0, to=1, by=0.03)
y <- rep(1, length(p))
ll <- NULL
for(i in 1:length(p))
  ll[i] <- logloss(y, p[i])
plot(x = p, y = ll, type = "l", main = "log loss vs hit ratio",
     ylab = "log loss",
     xlab = "probability estmate for correct class", col = "blue", lwd = 3)
abline(h = 0.5, col = "lightblue")
text(0.2, 0.7, "hit ratio", col = "lightblue")
text(0.15, 3, "log loss", col = "blue")



