library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
nyc_data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv")

bronx_data <- nyc_data %>% filter(BOROUGH == "BRONX")

# Part 1
head(bronx_data$SALE.PRICE)
glimpse(bronx_data)

summary(bronx_data$SALE.PRICE)

bronx_neigh_prices <- bronx_data %>%
                      select(NEIGHBORHOOD, SALE.PRICE) %>% 
                      group_by(NEIGHBORHOOD) %>% 
                      summarise(avg_price = mean(SALE.PRICE))

ggplot(bronx_data, aes(x = SALE.PRICE)) +
  geom_boxplot(fill = "orange") +
  scale_x_continuous(labels = scales::label_dollar()) +
  theme_bw() +
  labs(title = "Box Plot of Bronx Sale Prices to Show Outliers", x = "Sale Price")

# I am plotting names on the y axis because otherwise 
# the names get all smushed together
ggplot(bronx_neigh_prices, aes(x = avg_price,
                               y = fct_reorder(NEIGHBORHOOD, avg_price),
                               fill = avg_price)) +
  geom_col() +
  scale_x_continuous(labels = scales::label_dollar()) +
  theme_bw() +
  labs(y = "Neighborhood", 
       x = "Average Sale Price", title = "Average Sale Price by Bronx Neighborhood")


# Data Cleaning Part 1c
bronx_clean <- bronx_data %>%
  mutate(
    GROSS.SQUARE.FEET = as.numeric(GROSS.SQUARE.FEET),
    LAND.SQUARE.FEET = as.numeric(LAND.SQUARE.FEET)
  ) %>%
  
  # Filter out non-sales and create log_price
  filter(SALE.PRICE > 1000) %>%
  mutate(log_price = log(SALE.PRICE)) %>%
  dplyr::select(
    log_price,
    GROSS.SQUARE.FEET,
    LAND.SQUARE.FEET,
    YEAR.BUILT,
    RESIDENTIAL.UNITS,
    COMMERCIAL.UNITS,
    NEIGHBORHOOD,
    BUILDING.CLASS.CATEGORY
  ) %>% na.omit()

glimpse(bronx_clean)
summary(bronx_clean)

reg_model <- lm(log_price ~., data = bronx_clean)
summary(reg_model)

# Getting rid of least helpful labels
reg_model <- lm(log_price ~. - YEAR.BUILT - COMMERCIAL.UNITS - GROSS.SQUARE.FEET - LAND.SQUARE.FEET, data = bronx_clean)
summary(reg_model)


# PART 1d
predictors <- c(
  "log_price",
  "LAND.SQUARE.FEET"
)
target <- "NEIGHBORHOOD"

# Create the final data for modeling
model_data <- bronx_clean %>%
  mutate(NEIGHBORHOOD = as.factor(NEIGHBORHOOD)) %>%
  select(all_of(target), all_of(predictors))

set.seed(2025)
trainIndex <- createDataPartition(model_data$NEIGHBORHOOD, p = 0.70, list = FALSE)
trainData <- model_data[trainIndex, ]
testData <- model_data[-trainIndex, ]

preProcValues <- preProcess(trainData[predictors], method = c("center", "scale"))

# Transform the predictor columns in both sets
trainData[predictors] <- predict(preProcValues, trainData[predictors])
testData[predictors] <- predict(preProcValues, testData[predictors])

fitControl <- trainControl(method = "cv", number = 5)

# Model 1: k-Nearest Neighbors (k-NN)
knnFit <- train(NEIGHBORHOOD ~ ., 
                data = trainData, 
                method = "knn",
                trControl = fitControl)

# Model 2: Random Forest
rfFit <- train(NEIGHBORHOOD ~ ., 
               data = trainData, 
               method = "rf",
               trControl = fitControl,
               ntree = 100)

# Model 3: Decision Tree (rpart)
rpartFit <- train(NEIGHBORHOOD ~ ., 
                  data = trainData, 
                  method = "rpart",
                  trControl = fitControl)

knnPred <- predict(knnFit, testData)
rfPred <- predict(rfFit, testData)
rpartPred <- predict(rpartFit, testData)

print("kNN Results")
knn_cm <- confusionMatrix(data = knnPred, reference = testData$NEIGHBORHOOD)
print(knn_cm)

print("Decision Tree Results")
rpart_cm <- confusionMatrix(data = rpartPred, reference = testData$NEIGHBORHOOD)
print(rpart_cm)

print("Random Forest Results")
rf_cm <- confusionMatrix(data = rfPred, reference = testData$NEIGHBORHOOD)
print(rf_cm)

# PART 2, Using BROOKLYN
brooklyn_data <- nyc_data %>% filter(BOROUGH == "BROOKLYN")
brooklyn_clean <- brooklyn_data %>%
  mutate(
    GROSS.SQUARE.FEET = as.numeric(GROSS.SQUARE.FEET),
    LAND.SQUARE.FEET = as.numeric(LAND.SQUARE.FEET)
  ) %>%
  
  # Filter out non-sales and create log_price
  filter(SALE.PRICE > 1000) %>%
  mutate(log_price = log(SALE.PRICE)) %>%
  dplyr::select(
    log_price,
    GROSS.SQUARE.FEET,
    LAND.SQUARE.FEET,
    YEAR.BUILT,
    RESIDENTIAL.UNITS,
    COMMERCIAL.UNITS,
    NEIGHBORHOOD,
    BUILDING.CLASS.CATEGORY
  ) %>% na.omit()

glimpse(brooklyn_clean)

# PART 2a
reg_model <- lm(log_price ~., data = brooklyn_clean)
summary(reg_model)

# Getting rid of least helpful labels
# Interestingly the r^2 goes up for the brooklyn data
reg_model <- lm(log_price ~. - YEAR.BUILT - COMMERCIAL.UNITS - GROSS.SQUARE.FEET - LAND.SQUARE.FEET, data = bronx_clean)
summary(reg_model)


# PART 1d
model_data <- brooklyn_clean %>%
  mutate(NEIGHBORHOOD = as.factor(NEIGHBORHOOD)) %>%
  select(all_of(target), all_of(predictors))

trainIndex <- createDataPartition(model_data$NEIGHBORHOOD, p = 0.70, list = FALSE)
trainData <- model_data[trainIndex, ]
testData <- model_data[-trainIndex, ]

preProcValues <- preProcess(trainData[predictors], method = c("center", "scale"))

# Transform the predictor columns in both sets
trainData[predictors] <- predict(preProcValues, trainData[predictors])
testData[predictors] <- predict(preProcValues, testData[predictors])

fitControl <- trainControl(method = "cv", number = 5)

# Model 1: k-Nearest Neighbors (k-NN)
knnFit <- train(NEIGHBORHOOD ~ ., 
                data = trainData, 
                method = "knn",
                trControl = fitControl)

# Model 2: Random Forest
rfFit <- train(NEIGHBORHOOD ~ ., 
               data = trainData, 
               method = "rf",
               trControl = fitControl,
               ntree = 100)

# Model 3: Decision Tree (rpart)
rpartFit <- train(NEIGHBORHOOD ~ ., 
                  data = trainData, 
                  method = "rpart",
                  trControl = fitControl)

knnPred <- predict(knnFit, testData)
rfPred <- predict(rfFit, testData)
rpartPred <- predict(rpartFit, testData)

print("kNN Results")
knn_cm <- confusionMatrix(data = knnPred, reference = testData$NEIGHBORHOOD)
print(knn_cm)

print("Decision Tree Results")
rpart_cm <- confusionMatrix(data = rpartPred, reference = testData$NEIGHBORHOOD)
print(rpart_cm)

print("Random Forest Results")
rf_cm <- confusionMatrix(data = rfPred, reference = testData$NEIGHBORHOOD)
print(rf_cm)
