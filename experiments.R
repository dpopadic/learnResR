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


