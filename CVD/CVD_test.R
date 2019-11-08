rm(list=ls())
setwd("/home/jaspo/Documents/Cardiovascular-Disease-Analysis-master/CVD/")
cvd <- read.csv(file = "cardio_train.csv", header = T, sep = ";")
str(cvd)

# Checking for NA values
colSums(is.na(cvd)) # No NA values

# Cleaning Data
index <- which(cvd$ap_lo >= cvd$ap_hi) # Checking and indexing rows with diastolic blood 
# pressure being greater than systolic blood pressure
cvd <- cvd[-index, ] # Removing these rows as they are incorrect entries

# There is a systolic blood pressure value of 16,020. An obvious misentry. 
# After further inspection, we remove observations with greater than 300 or lower than
# 70 systolic blood pessure
index <- which(cvd$ap_hi > 300 | cvd$ap_hi < 70)
cvd <- cvd[-index, ]

index <- which(cvd$ap_lo <= 25)
cvd <- cvd[-index, ]

# Creating two new variables: age1 and bmi.
# age1 is age in years
cvd$age1 <- round(cvd$age/365, 0)
cvd$bmi <- round(cvd$weight / ((cvd$height/100)^2), 2)

# We see that there are incorrect entries from looking at weights.
# For example, some people weigh 20 lbs or less despite being middle-aged

# We are going to address these incorrect entries by calculating bmi and eliminating all
# data entries with less than 15 bmi value and greater than 50. These account for 
# only ~1% of the data. 

index <- which(cvd$bmi <= 15 | cvd$bmi >= 50)
cvd <- cvd[-index, ]

summary(cvd)

cvd_dup <- cvd

# Converting integer variables that are categorical into factor variables
cvd$gender <- as.factor(cvd$gender); levels(cvd$gender) <- c("female", "male")
cvd$cholesterol <- as.factor(cvd$cholesterol) 
levels(cvd$cholesterol) <- c("normal", "above normal", "well above normal")
cvd$gluc <- as.factor(cvd$gluc)
levels(cvd$gluc) <- c("normal", "above normal", "well above normal")
cvd$smoke <- as.factor(cvd$smoke); levels(cvd$smoke) <- c("Non-smoker", "Smoker")
cvd$alco <- as.factor(cvd$alco); levels(cvd$alco) <- c("Non-drinker", "Drinker")
cvd$active <- as.factor(cvd$active); levels(cvd$active) <- c("Not active", "Active")
cvd$cardio <- as.factor(cvd$cardio); levels(cvd$cardio) <- c("No CVD", "CVD")

str(cvd)

count <- table(cvd$cardio, cvd$age1)
barplot(count, main="Distribution by Age and CVD",
        xlab="Age(In Years)", ylab="Count", col=c("darkblue","red"),
        legend = rownames(count), beside=TRUE, args.legend = list(x = "topleft"))

library(reshape)
library(ggplot2)
data_noCVD <- cvd[cvd$cardio == 'No CVD', ]; data_CVD <- cvd[cvd$cardio == 'CVD', ]
melt_noCVD <- melt(data_noCVD, id.vars = 'cardio', measure.vars = c('cholesterol', 'gluc', 'smoke', 'alco', 'active'))
melt_CVD <- melt(data_CVD, id.vars = 'cardio', measure.vars = c('cholesterol', 'gluc', 'smoke', 'alco', 'active'))
combine_melt <- rbind(melt_noCVD, melt_CVD)

ggplot(combine_melt,aes(factor(variable)))+geom_bar(aes(fill = value), position = "dodge")+
  ggtitle("Comparison of Categorical Variables Among those w/o CVD and with CVD")+labs(x = "Categories")+
  facet_grid(. ~ cardio)

# Correlation matrix
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

corr_matrix <- rquery.cormat(cvd_dup[, c(-1, -2)])

ggplot(cvd, aes(x=cardio, y=ap_hi)) +
  geom_boxplot() + coord_flip()+ 
  ggtitle("Systolic Blood Pressure Among No CVD and CVD Groups")+
  labs(x="Group", y="Systolic Blood Presure")

ggplot(cvd, aes(x=gender, y=bmi, fill=cardio)) +
  geom_boxplot()

# First off, we look at tables displaying CVD with all of the categorical variables
# We do this to see if there are a sufficient amount of reported data across all variables
# of each data. If there are an insufficient amount of reported data, that could cause
# an issue with finding a  model/line that best fits the data 
xtabs(~ cardio + gender, data = cvd)
xtabs(~ cardio + cholesterol, data = cvd)
xtabs(~ cardio + gluc, data = cvd)
xtabs(~ cardio + smoke, data = cvd)
xtabs(~ cardio + alco, data = cvd)
xtabs(~ cardio + active, data = cvd)

# There are sufficient amount of reported data across all levels of each categorical variable

# We make a new data frame
cvd_new = cvd[, c(-1, -2, -4, -5)]

set.seed(1234)
ind <- sample(2, nrow(cvd_new), replace = T, prob = c(0.8, 0.2))
train <- cvd_new[ind==1, ]
test <- cvd_new[ind==2, ]

# Logistic Regression Classifier Model
logistic <- glm(cardio ~ ., data = train, family = "binomial")
# step(logistic, direction = "both") # Performing stepwise selection
summary(logistic)

table(Predicted = ifelse(logistic$fitted.values < 0.50, "No CVD", "CVD"), Actual = train$cardio)
(21805+18072)/nrow(train) # 0.7286265 - Training Classification Rate

p <- predict(logistic, newdata = test, type="response")
table_class <- table(Predicted = ifelse(p < 0.50, "No CVD", "CVD"), Actual = test$cardio)
table_class
correct <- (table_class[1,2] + table_class[2,1])/nrow(test)
cat("")
cat("Logistic Regression Model with one-hot encoding and stepwise feature selection yields 
a testing successful classification rate of",correct)

predicted.data <- data.frame(
  probability.of.cvd = logistic$fitted.values, cvd = train$cardio)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.cvd, decreasing = FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)


library(ggplot2)
#library(cowplot)
#theme_set(theme_cowplot())
ggplot(data = predicted.data, aes(x=rank, y=probability.of.cvd)) +
  geom_point(aes(color=cvd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting CVD")

# XG Boosting Classifier Algorithm with One-Hot Encoding
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)

train_xgb <- train; test_xgb <- test
train_xgb$cardio <- as.integer(train$cardio) - 1; test_xgb$cardio <- as.integer(test$cardio) - 1

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(cardio ~ .-1, data = train_xgb)
train_label <- train_xgb[, "cardio"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(cardio ~.-1, data = test_xgb)
test_label <- test_xgb[,"cardio"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

# XGB Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 500,
                       watchlist = watchlist, 
                       eta = 0.01,
                       max.depth = 6,
                       seed = 333)

# Training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
# Some overfitting is taking place

min(e$test_mlogloss)
# e[e$test_mlogloss == 0.543199, ]

# Feature Importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
xgb.plot.importance(imp)

# Prediction and confusion matrix
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
table_class <- table(Prediction = pred$max_prob, Actual = pred$label)
table_class
correct <- (table_class[1,1] + table_class[2,2])/nrow(test)
cat("XG Boosting Classifier model yields a testing successful classification rate of",correct)

# Random Foresting
library(randomForest)
set.seed(1234)
model <- randomForest(cardio ~ ., data = train, type="classification", ntree=300, proximity = FALSE, importance = TRUE)
model
model$importance

p = predict(model, newdata=test[,-9])
table_class <- table(Predicted = p, Actual = test$cardio)
table_class
correct <- (table_class[1,1] + table_class[2,2])/nrow(test) 
cat("Random Forest model with 300 trees yields a testing successful classification rate of",correct)