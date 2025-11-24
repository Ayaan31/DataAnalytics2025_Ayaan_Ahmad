library(tidyverse)
library(Metrics)
library(rpart)
library(glmnet)
library(e1071)

NYC_data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

# I want to drop prices that have 
price_sqf_nyc <- NYC_data %>% 
  transmute(
    price = as.numeric(SALE.PRICE),
    square_footage = as.numeric(GROSS.SQUARE.FEET)
  ) %>% 
  filter(price > 1000 & square_footage > 100) %>% 
  drop_na()


train_indices <- sample(nrow(price_sqf_nyc),0.75*nrow(price_sqf_nyc))

train <- price_sqf_nyc[train_indices,]
test <- price_sqf_nyc[-train_indices,]
  
# Model 1: Linear Regression
lin_reg_log <- lm(log(price) ~ log(square_footage), data = train)
summary(lin_reg_log)

pred_lin_reg_log <- predict(lin_reg_log, test)
pred_lin_reg <- exp(pred_lin_reg_log)

lin_mae <- mae(test$price, pred_lin_reg)
lin_mse <- mse(test$price, pred_lin_reg)
lin_rmse <- rmse(test$price, pred_lin_reg)


# Model 2: Regression Tree
model_tree <- rpart(price ~ square_footage, data = train)

pred_tree <- predict(model_tree, test)

mae_tree  <- mae(test$price, pred_tree)
mse_tree  <- mse(test$price, pred_tree)
rmse_tree <- rmse(test$price, pred_tree)

# Model 3: SVM Regression
svm_regression <- svm(
                      price ~ square_footage,
                      data = train, kernel = "radial" 
                    )

svm_predict <- predict(svm_regression, test)

svm_mae <- mae(test$price, svm_predict)
svm_mse <- mse(test$price, svm_predict)
svm_rmse <- rmse(test$price, svm_predict)


# Evaluation of Linear Regression
print("Linear Regression Evaluations: ")
cat("MAE: ", lin_mae, "| MSE: ", lin_mse, "| RMSE", lin_rmse)

print("Regression Tree Evaluations: ")
cat("MAE: ", mae_tree, "| MSE: ", mse_tree, "| RMSE", rmse_tree)

print("Support Vector Regression Evaluations: ")
cat("MAE: ", svm_mae, "| MSE: ", svm_mse, "| RMSE", svm_rmse)

results <- data.frame(
  Algorithm = c("Linear Regression (Log)", "Regression Tree", "SVM"),
  MAE  = c(lin_mae, mae_tree, svm_mae),
  MSE  = c(lin_mse, mse_tree, svm_mse),
  RMSE = c(lin_rmse, rmse_tree, svm_rmse)
)

results_formatted <- results %>%
  mutate(across(where(is.numeric), ~ format(round(., 0), big.mark = ",")))

results_formatted

