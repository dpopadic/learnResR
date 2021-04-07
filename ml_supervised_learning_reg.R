# Supervised Learning in R: Regression
# ------------------------------------


# 1. Regression Basics
# --------------------
unemployment <- readRDS("data/unemployment.rds")
library(broom)
library(sigr)
library(ggplot2)
library(tidyr)
library(dplyr)

# linear regressions..
fmla <- as.formula("female_unemployment ~ male_unemployment")
unemployment_model <- lm(fmla, data=unemployment)
# examining a model..
summary(unemployment_model)
broom::glance(unemployment_model) # model stats
wrapFTest(unemployment_model) # F-test

# prediciting from training data..
unemployment$prediction <-  predict(unemployment_model, unemployment)
ggplot(unemployment, aes(x = prediction, y = female_unemployment)) + geom_point() + geom_abline(color = "blue")
# predict female unemployment rate when male unemployment is 5%..
newrates <- data.frame(male_unemployment = 5)
pred <- predict(unemployment_model, newrates)

# multiple regression..
bloodpressure <- readRDS("data/bloodpressure.rds")
fmla <- as.formula(blood_pressure ~ age + weight)
bloodpressure_model <- lm(fmla, data = bloodpressure)
# prediction..
bloodpressure$prediction<- predict(bloodpressure_model, bloodpressure)
ggplot(bloodpressure, aes(x = prediction, y = blood_pressure)) + geom_point() + geom_abline(color = "blue")




# 2. Training & Evaluating Regression Models
# ------------------------------------------

# residuals..
unemployment$predictions <- predict(unemployment_model, unemployment)
ggplot(unemployment, aes(x = predictions, y = female_unemployment)) + geom_point() + geom_abline()
unemployment$residuals <- unemployment$female_unemployment - unemployment$predictions
ggplot(unemployment, aes(x = predictions, y = residuals)) + geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + ggtitle("residuals vs. linear model prediction")

# gain curve..
library(WVPlots)
GainCurvePlot(unemployment, "predictions", "female_unemployment", "Unemployment model")
# .. a relative gini coefficient close to one shows that the model correctly sorts high unemplyoment situations from lower ones

# root mean squared error (rmse)..
# -> interpretation: typical model error
res <- unemployment$predictions - unemployment$female_unemployment
(rmse <- sqrt(mean(res^2)))
(sd_unemployment <- sd(unemployment$female_unemployment))
# compare rmse to the stdev of the outcome: with a good model, the rmse should be smaller..
rmse / sd_unemployment

# r-squared..
# rsquared=1-RSS/SST=1-(residual sum of squares:=variance from model)/(total sum of squared:=variance from data)
tss <- sum((unemployment$female_unemployment - mean(unemployment$predictions))^2)
rss <- sum((unemployment$female_unemployment - unemployment$predictions)^2)
rsq <- 1 - rss / tss

# properly training a model..
# overfit-test: if train vs test model performance similar
# cross-validation: if not enough data to split into train/test sets
library(vtreat)
mpg <- load("data/Mpg.RData")
mpg <- bind_rows(mpgtest, mpgtrain)

# generate a random test/train split..
N <- nrow(mpg)
# a vector of N uniform random variables (0-1 range)..
gp <- runif(N)
# create the training set: mpg_train (75% of data) and mpg_test (25% of data)..
mpg_train <- mpg[gp < 0.75, ]
mpg_test <- mpg[gp >= 0.75, ]

# train model..
mpg_model <- lm(cty ~ hwy, data = mpg_train)
# Examine the objects in the workspace
mpg_train$pred <- predict(mpg_model, mpg_train)
mpg_test$pred <- predict(mpg_model, mpg_test)
ggplot(mpg_test, aes(x = pred, y = cty)) + geom_point() + geom_abline()

# creating a cross-validation plan..
# ---
# 3-fold cross-fold plan..
# nRows: # rows in training data
# nSplits: # folds (partitions) in the cross-validation
splitPlan <- kWayCrossValidation(nrow(mpg), 3, dframe = NULL, y = NULL)
str(splitPlan)
# train: training set & app: test (or application) set

# run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(cty ~ hwy, data = mpg[split$train, ])
  mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app, ])
}
# predict from a full model..
mpg$pred <- predict(lm(cty ~ hwy, data = mpg),mpg)
# rmse comparison..
res.full <- sqrt(mean((mpg$pred - mpg$cty)^2))
res.cv <- sqrt(mean((mpg$pred.cv - mpg$cty)^2))




# 3. Issues to Consider
# ---------------------

# examine structure of categorical inputs..
flowers <- read.csv('data/Flowers.csv')

# 3.1 modelling with categorical data..
# ---
fmla <- as.formula("Flowers ~ Intensity + Time")
# see how the data is represented for modeling..
mmat <- model.matrix(fmla, data = flowers)
head(mmat, 20)
summary(mmat)
flower_model <- lm(fmla, data = flowers)
flowers$predictions <- predict(flower_model, flowers)
ggplot(flowers, aes(x = predictions, y = Flowers)) + geom_point() + geom_abline(color = "blue") 

# 3.2 variable interactions..
# ---
alcohol <- read.csv('data/alcohol.csv')
# a variable interaction occurs when a simultanous effect of 2 variables on the outcome is not additive
# expressing interactions:
# main effects & interactions: y ~ a*b or y ~ a + b +a:b 
# expressing the product of 2 variables: y ~ I(a*b)

# main effects..
fmla_add <- as.formula(Metabol ~ Gastric + Sex)
# interactions..
fmla_interaction <- as.formula(Metabol ~ Gastric + Gastric:Sex)
model_add <- lm(fmla_add, data = alcohol)
model_interaction <- lm(fmla_interaction, data = alcohol)
summary(model_add)
summary(model_interaction)

# modelling interactions with cross-validation..
# ---
set.seed(34245)
splitPlan <- kWayCrossValidation(nrow(alcohol), 3, dframe = NULL, y = NULL)
# main-effects only model..
alcohol$pred_add <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_add <- lm(fmla_add, data = alcohol[split$train, ])
  alcohol$pred_add[split$app] <- predict(model_add, newdata = alcohol[split$app, ])
}

# model with interactions..
alcohol$pred_interaction <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_interaction <- lm(fmla_interaction, data = alcohol[split$train, ])
  alcohol$pred_interaction[split$app] <- predict(model_interaction, newdata = alcohol[split$app, ])
}

# Get RMSE
alcohol %>% 
  gather(key = modeltype, value = pred, pred_add, pred_interaction) %>%
  mutate(residuals = Metabol - pred) %>%      
  group_by(modeltype) %>%
  summarize(rmse = sqrt(mean(residuals^2)))


# 3.3 transforming the response before modelling
# ---

# lognormal distributions: 
# mean > median -> predicting mean will overpredict typical values -> take log of lognormal distributed data to get normally distributed  data
fdata<- read.csv('data/fdata.csv')

# realtive error..
# ---
fdata %>% group_by(label) %>% summarize(min  = min(y), 
                                        mean = mean(y),
                                        max = max(y))
fdata2 <- fdata %>%  group_by(label) %>% mutate(residual = pred - y, 
                                                relerr = residual / y)
fdata2 %>% group_by(label) %>% summarize(rmse = sqrt(mean(residual^2)), 
                                         rmse.rel = sqrt(mean(relerr^2))) 
par(mfrow = c(2,1))
ggplot(fdata2, aes(x = pred, y = y, color = label)) + geom_point() + geom_abline() + 
       facet_wrap(~ label, ncol = 1, scales = "free") + ggtitle("Outcome vs prediction")

# modelling log-transformed output..
# ---
income_train <- read.csv('data/income_train.csv')
income_test <- read.csv('data/income_test.csv')
fmla_log <- as.formula(log(Income2005) ~ Arith + Word + Parag + Math + AFQT)
model_log <-  lm(fmla_log, data = income_train)
income_test$logpred <- predict(model_log, income_test)
income_test$pred.income <- exp(income_test$logpred)
summary(income_test$pred.income)
ggplot(income_test, aes(x = pred.income, y = Income2005)) + geom_point() + geom_abline(color = "blue")

# -> transformations can reduce relative RMSE!

# why to transform input variables..
# - domain knowledge / synthetic variables
# - pragmatic reasons (reduce dynamic range, meaningful changes in variables are multiplicative)
# - y approximately linear in f(x) rather than in x

# hockey stick transformations..
# ---
houseprice <- read.csv('data/houseprice.csv')
fmla_sqr <- as.formula(price ~ I(size^2))
model_sqr <- lm(fmla_sqr, houseprice)
model_lin <- lm(price ~ size, houseprice)

# make predictions and compare..
houseprice %>% 
  mutate(pred_lin = predict(model_lin), pred_sqr = predict(model_sqr)) %>%
  gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>%
  ggplot(aes(x = size)) + geom_point(aes(y = price)) + geom_line(aes(y = pred, color = modeltype)) + scale_color_brewer(palette = "Dark2")

# quadratic model seems to fit houseprice better than a linear model! out-of-sample???
set.seed(34245)  # set the seed for reproducibility
splitPlan <- kWayCrossValidation(nrow(houseprice), 3, dframe = NULL, y = NULL)
houseprice$pred_lin <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_lin <- lm(price ~ size, data = houseprice[split$train, ])
  houseprice$pred_lin[split$app] <- predict(model_lin, newdata = houseprice[split$app, ])
}

houseprice$pred_sqr <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_sqr <- lm(fmla_sqr, data = houseprice[split$train, ])
  houseprice$pred_sqr[split$app] <- predict(model_sqr, newdata = houseprice[split$app, ])
}
# out-of-sample residuals..
houseprice_long <- houseprice %>% gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>% mutate(residuals = pred - price)
# compare the cross-validated RMSE for the two models..
houseprice_long %>% group_by(modeltype) %>% summarize(rmse = sqrt(mean(residuals^2)))





# 4. Dealing with Non-Linear Responses
# ------------------------------------

# Logistic regression
# ---

# predicting whether an event occurs: classification
# predicting the probability that an event occurs: regression

# squared-error & RMSE not good for logistic regression, use deviance & pseudo-r-squared (analagous to RSS)

sparrow <- read.csv('data/sparrow.csv')
# creating the synthethic target variable..
sparrow$survived <- ifelse(sparrow$status == "Survived", TRUE, FALSE)
# fitting logistic regression model..
fmla <- as.formula(survived ~ total_length + weight + humerus)
sparrow_model <- glm(fmla, sparrow, family = "binomial")
summary(sparrow_model)
# call glance to see different functions for examining a logistic regression model..
perf <- broom::glance(sparrow_model)
# pseudo-R-squared
pseudoR2 <- 1 - (perf$deviance / perf$null.deviance)

# predictions..
sparrow$pred <- predict(sparrow_model, sparrow, type = "response")
# gain curve..
WVPlots::GainCurvePlot(sparrow, "pred", "survived", "sparrow survival model")



# Poisson & quasi-poisson regression to predict counts
# ---
# input additive & linear in log(count) (same as logistic regression)
# outcome: integer
# assumption: mean(y)=var(y) -> if var(y) much different from mean(y): quasi-poisson
# requires large sample size

# modelling: 
# bike_model<- glm(fmla, data=bikesJuly, family="quasipoisson")
# predict(bike_model, bikesAugust, type="response")


# GAM to learn non-linear transforms
# ---
# GAM: generalized additive model, more likely to overfit & more suitable for large datasets
library(mgcv)
soybean_train <- read.csv('data/soybean_train.csv')
# weight vs Time..
ggplot(soybean_train, aes(x = Time, y = weight)) + geom_point() 
# fit the GAM Model..
model.gam <- gam(weight ~ s(Time), data = soybean_train, family = gaussian)
summary(model.gam)
plot(model.gam)




# 5. Tree-Based Methods
# ---------------------

# Pro: trees have an expressive concept space
# Con: coarse-grained predictions, it's hard for trees to express linear relationships

# other issues:
# - tree with too many splits (deep tree) -> danger of overfit
# - tree with too few splits (shallow tree) -> predictions too coarse-grained
# -> ensembles of trees: 1) Random Forest & 2) Gradient Boosted Trees

# Random Forest
# ---
# multiple diverse decision trees averaged together..
# reduces overfit
# increases model expressiveness
# finer grain predictions
bikesJuly <- read.csv('data/bikesJuly.csv')
library(ranger)


# build a rf-model for bike rentals..
seed <- set.seed(423563)
outcome <- "cnt"
vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed")
fmla <- paste(outcome, "~", paste(vars, collapse = " + "))
# fit the model..
bike_model_rf <- ranger(fmla, bikesJuly, num.trees = 500, respect.unordered.factors = "order", seed = seed)



# one-hot encoding
# ---
# one-hot encoding categorical variables (xgboost does not directly accept categorical variables, this is called one-hot-encoding)
# designTreatmentsZ() to design a treatment plan from the training data
# prepare() to created "clean" data
# issue resolved: sometimes there are different levels in training/test data leading to errors -> .. vtreat() function helps overcome this
library(vtreat)
library(magrittr)
dframe <- read.csv('data/dframe.csv')

vars <- c("color","size")
# create a treatment plan..
# a treatment plan to transform categorical variables into indicator variables (coded "lev"), and to clean bad values out of 
# numerical variables (coded "clean")
treatplan <- vtreat::designTreatmentsZ(dframe, vars)
# treatplan returns a list with element scoreFrame that includes the names & types of the new variables..
scoreFrame <- treatplan %>% use_series(scoreFrame) %>% select(varName, origName, code)
# only want the rows with codes "clean" or "lev"..
newvars <- scoreFrame %>% filter(code %in% c("clean", "lev")) %>% use_series(varName)
# the new (cleaned) training data..
dframe.treat <- vtreat::prepare(treatplan, dframe, varRestriction = newvars)


# another example (bikes dataset)..
outcome <- "cnt"
vars <- c("hr", "holiday", "workingday", "weathersit", "temp", "atemp", "hum", "windspeed")
treatplan <- designTreatmentsZ(bikesJuly, vars, verbose = FALSE)
newvars <- treatplan %>% use_series(scoreFrame) %>% filter(code %in% c("clean", "lev")) %>% use_series(varName) 
# prepare the training data..
bikesJuly_treat <- prepare(treatplan, bikesJuly,  varRestriction = newvars)
bikesAugust_treat <- prepare(treatplan, bikesAugust,  varRestriction = newvars)



# Gradient boosting machines
# ---
# What is it? It's an ensemble method that incrementally improves the existing model to build a better one
library(xgboost)
bikesAugust <- read.csv('data/bikesAugust.csv')
# procedure:
# 1. run xgb.cv() with a large number of rounds (trees)
# 2. xgb.cv()$evaluation_log: records estimated RMSE for each round (find # trees that minimizes estimated RMSE: nu_best)
# 3. run xgboost() by setting nrounds=nu_best

# example..
cv <- xgb.cv(data = as.matrix(bikesJuly_treat), 
             label = bikesJuly$cnt,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0)

# get the evaluation log..
elog <- cv$evaluation_log

# determine and print how many trees minimize training and test error..
elog %>% summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
                   ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)
# .. in most cases, test is less than train. the training error keeps decreasing even after the test error starts to increase. it's
# important to use cross-validation to find the right number of trees..
ntrees <- 34

bike_model_xgb <- xgboost(data = as.matrix(bikesJuly_treat), # training data as matrix
                          label = bikesJuly$cnt,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0)
# predictions (usually one with test-data, not training data)..
bikesAugust$pred <- predict(bike_model_xgb, as.matrix(bikesAugust_treat))
ggplot(bikesAugust, aes(x = pred, y = cnt)) + geom_point() + geom_abline()
# rmse..
bikesAugust %>% mutate(residuals = cnt - pred) %>% summarize(rmse = sqrt(mean(residuals^2)))






