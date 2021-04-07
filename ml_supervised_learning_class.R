# Supervised Learning in R: Classification
# ----------------------------------------
# Algos: k-Nearest Neighbors, Naive Bayes, Logistic Regression, Random Forest
library(dplyr)

# 1. k-Nearest Neighbors
# ----------------------
# many knn algos use euclidean distance (straight line distance) to measure similarity between objects
library(class) # contains knn-package
# load sign dataset..
signs0 <- read.csv("data/knn_traffic_signs.csv")
signs <- signs0[signs0$sample == "train",]

# create a vector of labels
sign_types <- signs$sign_type
# observation to classify:
next_sign <- signs0[206, -c(1:3)]
# classify the next sign observed:
knn(train = signs[-c(1:3)], test = next_sign, cl = sign_types)

# exploring the dataset..
table(signs$sign_type)
# check average red level by sign type..
aggregate(r10 ~ sign_type, data = signs, mean)
# example: road sign classification
sign_types <- signs$sign_type
signs_pred <- knn(train = signs[-c(1:3)], test = signs0[signs0$sample == "test", -c(1:3)], cl = sign_types)
# confusion matrix of the actual versus predicted values..
signs_actual <- signs0[signs0$sample == "test", 3]
table(signs_actual, signs_pred)
# accuracy..
mean(signs_actual == signs_pred)

# what's kNN?
# ---
# k: allows the algorithm to consider additional nearby neighbors. This enlarges the collection of neighbors which 
# will vote on the predicted class. (default: 1 -> only a single nearest neighbour used for classification)

# how to set k?
# ---
# optimal value depends on impact of noisy data and the underlying problem 
# rule of thumb = (# observation)^(1/2) or better: test out-of-sample
# smaller k increases impact of noisy data
# smaller k may use more subtle (scharf) patterns
k_7 <- knn(train = signs[-c(1:3)], test = signs0[signs0$sample == "test", -c(1:3)], cl = sign_types, k = 7)
mean(signs_actual == k_7)
# seeing how the neighbors voted.. how certain are the votes?
sign_pred <- knn(train = signs[-c(1:3)], 
                 test = signs0[signs0$sample == "test", -c(1:3)], 
                 cl = sign_types, 
                 k = 7, 
                 prob = TRUE)
# get the "prob" attribute from the predicted classes..
sign_prob <- attr(sign_pred, "prob")
# examine the proportion of votes for the winning class
head(sign_prob)

# data preparation for kNN
# ---
# Uses distance functions to measure similarity. Hence, numerical variables need to be provided (eg. rectangle/diamond = 1/0).
# make sure the range of data is the same for all data! Use min-max normalization to rescale data before classification..
# min-max normalization (ensure all data elements contribute equal shares to distance!):
normalize<- function(x){
  return( (x - min(x)) / (max(x) - min(x)) )
}




# 2. Naive Bayes
# --------------
library(naivebayes)
# load location dataset..
locations <- read.csv("data/locations.csv")
where9am <- subset(locations, hour == 9, select = c("daytype","location"))
# theory
# ---
# conditional probability: P(A|B) = P(A and B) / P(B)
# P(A):  P(office)
p_A <- nrow(subset(where9am, location == "office")) / 91
# P(B): P(weekday)
p_B <- nrow(subset(where9am, daytype == "weekday")) / 91
# P(A and B): P(office and weekday)
p_AB <- nrow(subset(where9am, where9am$location == "office" & where9am$daytype == "weekday")) / 91
# P(A | B): P(office | weekday)
p_A_given_B <- p_AB / p_B
p_A_given_B

# using naive bayes package..
locmodel <- naive_bayes(location ~ daytype, data = where9am)
# predict location..
thursday9am <- data.frame(daytype = subset(where9am, daytype == "weekday")[1, 1])
predict(locmodel, thursday9am, type = "prob")
# naive: rather than treating the problem as an intersection of all events, it approximates..
locations2 <- subset(locations, select = c("daytype","hourtype","location"))
locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations2)
# adjusting for unforeseen circumstances - laplace correaction..
weekend_afternoon <- data.frame(daytype = subset(locations2, daytype == "weekend" & hourtype == "afternoon")[1, 1])
locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations2, laplace = 1)
predict(locmodel2, weekend_afternoon, type = "prob")

# numeric data ois often binned before it's used in naive bayes
# "binning" technique: simple method for creating categories from numeric data (eg. time -> afternoon/evening or temperature -> hot/warm/cold)





# 3. Logistic Regression
# ----------------------
# logistics regression: glm(y ~ x1 + x2 +x3, data, family="binomial") -> binomial is logistic regression
donors <- read.csv("data/donors.csv")
table(donors$donated)
donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, data = donors, family = "binomial")
summary(donation_model)
# estimate the donation probability..
donors$donation_prob <- predict(donation_model, type = "response")
# find the donation probability of the average prospect..
mean(donors$donated)
# predict a donation if probability of donation is greater than average..
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)
# model accuracy
mean(donors$donated == donors$donation_pred)

# model performance
# ---
# accuracy is a very misleading measure of model performance on imbalanced datasets
# calculate ROC curves..
library(pROC)
ROC <- roc(donors$donated, donors$donation_prob)
plot(ROC, col = "blue")
auc(ROC) # higher auc does not necessarily translate into better predictions (just indicator) -> more info required

# dummy variables, missing data & interactions
# ---
# convert the wealth rating to a factor..
donors$wealth_rating <- factor(donors$wealth_rating, levels = c(0, 1, 2, 3), labels = c("Unknown", "Low", "Medium", "High"))
# use relevel() to change reference category
donors$wealth_rating <- relevel(donors$wealth_rating, ref = "Medium")
# see how our factor coding impacts the model
summary(glm(donated ~ wealth_rating, data = donors, family = "binomial"))

# missing value indicator - why add it? may represent unique category itself, there may be an important different with/without missing data or
# whatever caused the missing value may also be related to the outcome
# more sophisticated model with interaction term:
rfm_model <- glm(donated ~ money + recency * frequency, data = donors, family = "binomial")
summary(rfm_model)
# missing value indicator for age..
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

# automatic feature selection
# ---
# there maybe too many predictors etc. -> use stepwise regression (eg. backward stepwise or forward stepwise selection)
# stepwise regression model..
null_model <- glm(donated ~ 1, data = donors, family = "binomial")
full_model <- glm(donated ~ ., data = donors, family = "binomial")
# use a forward stepwise algorithm to build a parsimonious model..
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
# estimate the stepwise donation probability..
step_prob <- predict(step_model, type = "response")
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)



# 4. Classification Trees
# -----------------------
# decision trees are used to find a set of if/else conditions to make better decision
# package for classification trees: rpart
library(rpart)
loans <- read.csv("data/loans.csv")
loans$outcome <- ifelse(loans$default == 1, "default", "repaid")
loans <- loans[, c(4:13, 17)]

# loan model..
loan_model <- rpart(outcome ~ loan_amount + credit_score, data = loans, method = "class", control = rpart.control(cp = 0))
good_credit <- loans[loans$credit_score == "HIGH",][1, ]
predict(loan_model, good_credit, type = "class")
bad_credit <- loans[loans$credit_score == "LOW",][1,]
predict(loan_model, bad_credit, type = "class")

# visualizing decision trees
# ---
library(rpart.plot)
rpart.plot(loan_model)
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# growing larger classification trees
# ---
# A classification tree grows using a divide-and-conquer process. Each time the tree grows larger, it splits groups of 
# data into smaller subgroups, creating new branches in the tree.
# Given a dataset to divide-and-conquer, DT first split the group that leads to the greatest improvement in subgroup homogeneity

# create a random sample of row IDs
sample_rows <- sample(11312, 8484)
loans_train <- loans[sample_rows, ]
loans_test <- loans[-sample_rows, ]
# train dataset has no other -> exclude in test dataset..
loans_test <- loans_test[loans_test$home_ownership != "OTHER",]
loans_test <- loans_test %>% filter(loan_purpose %in% unique(loans_train$loan_purpose))
# model..
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))
loans_test$pred <- predict(loan_model, loans_test, type = "class")
# confusion matrix..
table(loans_test$outcome, loans_test$pred)
# compute the accuracy on the test dataset..
mean(loans_test$outcome == loans_test$pred)


# Pruning..
# ---

# 1. Pre-pruning:
# 1.1. maxdepth of 6
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0, maxdepth = 6))
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$outcome == loans_test$pred)
# 1.2. minimum observations to split: 500
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0, minsplit = 500))
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$outcome == loans_test$pred)

# remember: Stopping a tree from growing all the way can lead it to ignore some aspects of the data or miss important trends 
# it may have discovered later.

# 2. Post-pruning:
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))
# complexity plot..
plotcp(loan_model)
# prune the tree..
loan_model_pruned <- prune(loan_model, cp = 0.0014)
loans_test$pred <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$outcome == loans_test$pred)

# Summary: Simpler trees are easier to interpret, faster to train and may perform better on testing data.

# random-forest
# ---
# Decision tree forests power comes not from a single tree that has grown complex, but rather from a collection of smaller trees. Each
# of the forest trees is diverse. Growing diverse trees requires growing conditions to vary from tree to tree. This is done by allocating
# each tree a random subset of the data. Rf uses the "ensemble" principle that weaker learners become stronger with teamwork.
library(randomForest)

# Build a random forest model
loan_model <- randomForest(outcome ~ ., data = loans_train)
loans_test$pred <- predict(loan_model, loans_test, type = "class")




