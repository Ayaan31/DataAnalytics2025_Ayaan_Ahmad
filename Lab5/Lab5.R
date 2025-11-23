library(tidyverse)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

wine$Type <- as.factor(wine$Type)


N <- nrow(wine)
train_indices <- sample(N, 0.8 * N)

train_data <- wine[train_indices,]
test_data <- wine[-train_indices,]

# Seperate the featues and labeles
label <- wine[,1]
features <- wine[,-1]

ggpairs(train_data, aes(colour = Type))

cost <- c(0.001, 0.01, 0.1, 1, 10, 100)

# MODEL 1: LINEAR
tune_linear <- tune.svm(
  Type ~., 
  data = train_data, 
  kernel = "linear",
  gamma=2^(-1:1),cost=2^(2:4))

# Show the tuning results
summary(tune_linear)

best_cost <- tune_linear$best.parameters$cost
best_cost

# Final linear model using the best cost
svm_linear_subset <- svm(
  Type ~.,
  data = train_data,
  kernel = "linear",
  gamma=2^(-1:1),cost=4
  )

print(svm_linear_subset)

# MODEL 2: Radial
tune_radial <- tune.svm(
  Type ~ Flavanoids + Proline,
  data = train_data,
  kernel = "radial",
  gamma=2^(-1:1),cost=2^(2:4)
)


summary(tune_radial)

# Get the best parameters
best_cost_radial <- tune_radial$best.parameters$cost
best_gamma_radial <- tune_radial$best.parameters$gamma

print(paste("Best Cost:", best_cost_radial, "| Best Gamma:", best_gamma_radial))

# Train the final radial model using the best parameters
svm_radial_subset <- svm(
  Type ~ Flavanoids + Proline,
  data = train_data,
  kernel = "radial",
  cost = best_cost_radial,
  gamma = best_gamma_radial
)

print(svm_radial_subset)

# kNN MODEL:
k_value <- 3
kNN_model <- knn(train = train_data[,-1],
                 test = test_data[,-1],
                 cl = train_data$Type,
                 k = k_value)

confusion_matrix <- as.matrix(table(Actual = test_data$Type, Predicted = kNN_model))

# EVALUATION OF kNN Model (F1, precision, recall)
n <- sum(confusion_matrix)
nc = nrow(confusion_matrix) # number of classes
diag = diag(confusion_matrix) # number of correctly classified instances per class 
rowsums = apply(confusion_matrix, 1, sum) # number of instances per class
colsums = apply(confusion_matrix, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 


precision_kNN = diag / colsums
recall_kNN = diag / rowsums 
f1_kNN = 2 * precision_kNN * recall_kNN / (precision_kNN + recall_kNN) 


data.frame(recall_kNN, precision_kNN, f1_kNN)

# EVALUATION OF LINEAR SVM
confusion_matrix <- as.matrix(table(Actual = test_data$Type, Predicted = predict(svm_linear_subset, test_data)))

n <- sum(confusion_matrix)
nc = nrow(confusion_matrix) # number of classes
diag = diag(confusion_matrix) # number of correctly classified instances per class 
rowsums = apply(confusion_matrix, 1, sum) # number of instances per class
colsums = apply(confusion_matrix, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision_linear = diag / colsums
recall_linear = diag / rowsums 
f1_linear = 2 * precision_linear * recall_linear / (precision_linear + recall_linear) 

data.frame(recall_linear, precision_linear, f1_linear)

# EVALUATION OF RADIAL SVM
confusion_matrix <- as.matrix(table(Actual = test_data$Type, Predicted = predict(svm_radial_subset, test_data)))

n <- sum(confusion_matrix)
nc = nrow(confusion_matrix) # number of classes
diag = diag(confusion_matrix) # number of correctly classified instances per class 
rowsums = apply(confusion_matrix, 1, sum) # number of instances per class
colsums = apply(confusion_matrix, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision_radial = diag / colsums
recall_radial = diag / rowsums 
f1_radial = 2 * precision_radial * recall_radial / (precision_radial + recall_radial) 

data.frame(recall_radial, precision_radial, f1_radial)

