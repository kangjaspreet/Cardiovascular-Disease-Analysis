rm(list=ls())
cvd <- read.csv(file = "~/Documents/Jaspreet/CVD/cardio_train.csv", header = T, sep = ";")


# Cleaning Data
index <- which(cvd$ap_lo > cvd$ap_hi) # Checking and indexing rows with diastolic blood pressure being greater than systolic bloof pressure
cvd <- cvd[-index, ] # Eliminating these rows as they are incorrect entries

# Checking for NA values
colSums(is.na(cvd)) # No NA values

# Understanding the data
summary(cvd)
str(cvd)

# Creating two new variables: age1 and bmi.
# age1 is age in years
cvd$age1 <- round(cvd$age/365, 0)
round(cvd$age / 365, 0)
cvd$bmi <- round(cvd$weight / ((cvd$height/100)^2), 2)

# We see that there are incorrect entries from looking at weights.
# For example, some people weight 20 lbs or less despite being middle-aged

# We are going to address these incorrect entries by calculating bmi and eliminating all
# data entries with less than 15 bmi value and greater than 50. These account for only ~1% of the data. 

index <- which(cvd$bmi <= 15 | cvd$bmi >= 50)
cvd <- cvd[-index, ]

summary(cvd)
# There is a systolic blood pressure value of 16,020. An obvious misentry. 
# After further inspection, we eliminate observations with greater than 300 or lower tha 70 systolic blood pessure

cvd[cvd$ap_hi > 300, ]
cvd[cvd$ap_hi < 90, ]

index <- which(cvd$ap_hi > 300 | cvd$ap_hi < 70)
cvd <- cvd[-index, ]

summary(cvd)

index <- which(cvd$ap_lo <= 25)
cvd <- cvd[-index, ]

# Summary: Eliminated observations with systolic blood pressures higher than 300 or lower 
# than 70, and disystolic blood pressures lower than 26.
# Drawback: Due to my limited knowledge of abnormal ranges of blood pressure, I relied on
# online research to determine blood pressure values that are impossible or highly 
# improbable. However, getting an expert's consultation would have allowed me to address these outliers
# more accurately. 

summary(cvd)
cvd_dup <- cvd
# Changing classication numeric variables into factors

cvd$gender <- as.factor(cvd$gender); levels(cvd$gender) <- c("female", "male")
cvd$cholesterol <- as.factor(cvd$cholesterol); levels(cvd$cholesterol) <- c("normal", "above normal", "well above normal")
cvd$gluc <- as.factor(cvd$gluc); levels(cvd$gluc) <- c("normal", "above normal", "well above normal")
cvd$smoke <- as.factor(cvd$smoke); levels(cvd$smoke) <- c("Non-smoker", "Smoker")
cvd$alco <- as.factor(cvd$alco); levels(cvd$alco) <- c("Non-drinker", "Drinker")
cvd$active <- as.factor(cvd$active); levels(cvd$active) <- c("Not active", "Active")
cvd$cardio <- as.factor(cvd$cardio); levels(cvd$cardio) <- c("No CVD", "CVD")

str(cvd)

# Now the dataset is clean

# Research questions:
# 1) At what age does the event of obtaining CVD surpass not having CVD?
# 2) Comparing those with CVD and w/o CVD, which variables show greater risk/correlation?
# Gluc, Chol, smoke, active, bmi
# systolic bp, dysystolic bp
# 3) What variables are most correlated with CVD? (Create correlation heat matrix)
# 4) Compare BMI of genders with CVD
# 5) Use logistic regression to predict CVD

count <- table(cvd$cardio, cvd$age1)
barplot(count, main="Distribution by Age and CVD",
        xlab="Age(In Years)", ylab="Count", col=c("darkblue","red"),
        legend = rownames(count), beside=TRUE, args.legend = list(x = "topleft"))
legend("topleft")

# We see that the prevalence of CVD surpasses the not having CVD after the age of 54.
# Furthermore, the ratio of CVD:No CVD rapidly reaches 1:1 and higher in the 40s, which
# is quite worrying.
library(reshape)
library(ggplot2)
data_noCVD <- cvd[cvd$cardio == 'No CVD', ]; data_CVD <- cvd[cvd$cardio == 'CVD', ]
melt_noCVD <- melt(data_noCVD, id.vars = 'cardio', measure.vars = c('cholesterol', 'gluc', 'smoke', 'alco', 'active'))
melt_CVD <- melt(data_CVD, id.vars = 'cardio', measure.vars = c('cholesterol', 'gluc', 'smoke', 'alco', 'active'))

ggplot(melt_noCVD,aes(factor(variable)))+geom_bar(aes(fill = value), position = "dodge")
ggplot(melt_CVD,aes(factor(variable)))+geom_bar(aes(fill = value), position = "dodge")

cvd[, 1]
# Correlation matrix
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

rquery.cormat(cvd_dup[, c(-1, -2)])
# We see that the blood pressure variables are most highly correlated with CVD
# followed by age, weight & bmi, cholestorol, then glucose levels to a lesser degree.

# We will be comparing several classification models to see which model produces the highest
# successful classifcation rate/accuracy with the use of cross-validation.
# We will be using: Logistic Regression, XG Boosting Classifier, Decision Tree, Support
# Vector Machine Classifier, and Random Forest Classifier

# First off, we look at all the tables displaying CVD with all the categorical variables
# We do this to see if there are sufficient amount of reported data across all variables
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

# Logistic Regression Classifier Model
logistic <- glm(cardio ~ ., data = cvd_new, family = "binomial")
step(logistic, direction = "both") # Performing stepwise selection
summary(logistic)

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null - ll.proposed) / ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=length((logistic$coefficients)-1))

predicted.data <- data.frame(
  probability.of.cvd = logistic$fitted.values, cvd = cvd$cardio)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.cvd, decreasing = FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)


library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
ggplot(data = predicted.data, aes(x=rank, y=probability.of.cvd)) +
  geom_point(aes(color=cvd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting CVD")

# XG Boosting Classifier Algorithm with One-Hot Encoding
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)

cvd_xgb <- cvd_new
cvd_xgb$cardio <- as.integer(cvd_xgb$cardio) - 1
str(cvd_xgb)

# Partition Data
set.seed(1234)
ind <- sample(2, nrow(cvd_xgb), replace = T, prob = c(0.8, 0.2))
train <- cvd_xgb[ind==1, ]
test <- cvd_xgb[ind==2, ]

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(cardio ~ .-1, data = train)
train_label <- train[, "cardio"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(cardio ~.-1, data = test)
test_label <- test[,"cardio"]
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

bst_model
# Training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
# Some overfitting is taking place

min(e$test_mlogloss)
e[e$test_mlogloss == 0.5432, ]

# Feature Importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction and confusion matrix
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
head(pred)
table(Prediction = pred$max_prob, Actual = pred$label)
(5477+4579)/13688

# Random Foresting
library(randomForest)
set.seed(1234)
model <- randomForest(cardio ~ ., data = cvd_new, proximity = TRUE)
Sys.setenv('R_MAX_VSIZE'=32000000000)
Sys.getenv('R_MAX_VSIZE')
usethis::edit_r_environ()










