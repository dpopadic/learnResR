# Machine Learning Toolbox
# ------------------------
library(caret)
library(caTools)
library(mlbench)

## 1. Regression models: fitting them and evaluating their performance
# --------------------------------------------------------------------

# In-sample RMSE for linear regression on diamonds
# ---
data("mtcars")
head(diamonds)
# Fit lm model: model
model <- lm(price ~ ., diamonds)
# Predict on full data: p
p <- predict(model, diamonds)
# Compute errors: error
error <- p - diamonds$price
# Calculate RMSE
RMSE <- sqrt(mean(error^2))


# One way you can take a train/test split of a dataset is to order the dataset randomly, then divide it into the two sets..
# ---
set.seed(42)
# Shuffle row indices: rows
rows <- sample(nrow(diamonds))
# Randomly order data
diamonds <- diamonds[rows,]
# 80/20 split..
split <- round(nrow(diamonds) * .80)
# Create train
train <- diamonds[1:split, ]
# Create test
test <- diamonds[(split + 1):nrow(diamonds), ]
# Fit lm model on train
model <- lm(price ~ ., train)
# Predict on test
p <- predict(model, test)

# Cross-validation
# ---
# cross-validation: a better approach to validating models is to use multiple systematic test sets, rather than a single random train/test split
model <- train(price ~ ., diamonds, 
               method = "lm", 
               trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
# do more than just one iteration of cross-validation. Repeated cross-validation gives you a better estimate of the test-set error..
model <- train(price ~ ., diamonds, 
               method = "lm", 
               trControl = trainControl(method = "repeatedcv", 
                                        number = 5, 
                                        repeats = 5, 
                                        verboseIter = TRUE))
# predictions on new data..
predict(model, test)



# 2. Classification models: fitting them and evaluating their performance
# -----------------------------------------------------------------------
data(Sonar)

# logistic regression models
# ---
# data prep and split in training/test dataset..
rows <- sample(nrow(Sonar))
Sonar <- Sonar[rows, ]
split <- round(nrow(Sonar) * .60)
train <- Sonar[1:split, ]
test <- Sonar[(split + 1):nrow(Sonar), ]
model <- glm(Class ~ ., family = "binomial", train) # warning messages: common on smaller datasets and usually don't cause any issues
p <- predict(model, test, type = "response")

# confusion matrix..
# ---
# If p exceeds threshold of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > 0.5, "M", "R")
p_class <- factor(m_or_r, levels = levels(test[["Class"]])) # same levels as those of test
confusionMatrix(p_class, test$Class)

# ROC curves (summarise performance of classifier over all possible thresholds)
# ---
colAUC(p, test$Class, plotROC = TRUE)
# area under the curve: ROC model's ability to discriminate the positive from the negative class 
# use the trainControl() function in caret to use AUC (instead of acccuracy), to tune the parameters of your models
myControl <- trainControl(method = "cv", 
                          number = 10, 
                          summaryFunction = twoClassSummary, 
                          classProbs = TRUE, 
                          verboseIter = TRUE)
model <- train(Class ~ ., Sonar, 
               method = "glm", 
               trControl = myControl)




# 3. Tuning model parameters to improve performance
# -------------------------------------------------
# library(ranger)
download.file(paste0('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/', 'winequality-red.csv'), 
              'winequality-red.csv')
wine <- read.csv('winequality-red.csv', sep = ';')

# fit random forest
# ---
model <- train(Class ~ ., 
               tuneLength = 1, 
               data = Sonar, 
               method = "ranger", 
               trControl = trainControl(method = "cv", 
                                        number = 5, 
                                        verboseIter = TRUE))
# explore wider model space: mtry most important hyperparameter (# of randomly selected variables used at each split)
# gridsearch: selecting hyperparameters based on out-of-sample performance
model <- train(Class ~ ., 
               tuneLength = 3, 
               data = Sonar, 
               method = "ranger", 
               trControl = trainControl(method = "cv", 
                                        number = 5, 
                                        verboseIter = TRUE))
plot(model)
# mtry: controls how many variables are exposed to the splitting search routine at each split
# custom tuning grids..
# Define the tuning grid: tuneGrid
tuneGrid <- data.frame(.mtry = c(2, 3, 7), 
                       .splitrule = "variance", 
                       .min.node.size = 5)
model <- train(quality ~ ., 
               tuneGrid = tuneGrid, 
               data = wine, 
               method = "ranger", 
               trControl = trainControl(method = "cv", 
                                        number = 5, 
                                        verboseIter = TRUE))
print(model)

# fit glmnet (fits many models at once)
# ---
myControl <- trainControl(method = "cv", 
                          number = 10, 
                          summaryFunction = twoClassSummary, 
                          classProbs = TRUE, 
                          verboseIter = TRUE)
model <- train(Class ~ ., 
               Sonar, 
               method = "glmnet", 
               trControl = myControl)
# custom tuning grid
# Train glmnet with custom trainControl and tuning
model <- train(Class ~ ., Sonar, 
               tuneGrid = expand.grid(alpha = 0:1, 
                                      lambda = seq(0.0001, 1, length = 20)), 
               method = "glmnet", 
               trControl = myControl)
print(model)
plot(model)
max(model[["results"]][["ROC"]])




# 4. Preprocessing your data
# --------------------------
data("mtcars")

# median imputation..
model <- train(Class ~ ., Sonar, 
               method = "glm", 
               trControl = myControl, 
               preProcess = "medianImpute")
# knn imputation..
model <- train(Class ~ ., Sonar, 
               method = "glm", 
               trControl = myControl, 
               preProcess = "knnImpute")
# combining preprocessing methods..
model <- train(Class ~ ., Sonar, 
               method = "glm", 
               trControl = myControl, 
               preProcess = c("medianImpute", "center", "scale")) # median imputation, centering & scaling in that order
# Remove near zero variance predictors
model <- train(Class ~ ., Sonar, 
               method = "glm", 
               trControl = myControl, 
               preProcess = c("nzv","medianImpute", "center", "scale")) # median imputation, centering & scaling in that order
# alternatively:
remove_cols <- nearZeroVar(Sonar, names = TRUE, freqCut = 2, uniqueCut = 20) # % of distinct values out of # samples

# principal component analysis
data(BloodBrain)
# near-zero vol variables..
names(bbbDescr)[nearZeroVar(bbbDescr)]
model <- train(logBBB ~ ., bbbDescr, 
               method = "glm", 
               trControl = trainControl(method = "cv", 
                                        number = 10, 
                                        verbose = TRUE), 
               preProcess = c("nzv", "center", "scale"))
min(model$results$RMSE)
# use pca..
model <- train(logBBB ~ ., bbbDescr, method = "glm", preProcess = c("pca"))





# 5. Selecting models: a case study in churn prediction
# -----------------------------------------------------

# make custom train/test indices
myFolds <- createFolds(Sonar$Class, k = 5)
# create reusable trainControl object..
myControl <- trainControl(summaryFunction = twoClassSummary, 
                          classProbs = TRUE, 
                          verboseIter = TRUE, 
                          savePredictions = TRUE, 
                          index = myFolds)
# fit glmnet model: model_glmnet
churn_x <- Sonar[, 1:60]
churn_y <- Sonar$Class
model_glmnet <- train(x = churn_x, 
                      y = churn_y, 
                      metric = "ROC", 
                      method = "glmnet", 
                      trControl = myControl)
# fit random forest model
model_rf <- train(x = churn_x, 
                  y = churn_y, 
                  metric = "ROC", 
                  method = "ranger", 
                  trControl = myControl)

# comparing models
# ---
model_list <- list(item1 = model_glmnet, item2 = model_rf)
resamples <- resamples(model_list)
summary(resamples)
# box & whisker plot
bwplot(resamples, metric = "ROC")















