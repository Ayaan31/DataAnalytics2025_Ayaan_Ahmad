library(tidyverse)
library(caret)


epi_data <- read.csv("epi_results_2024_pop_gdp_v2.csv")

epi_east_europe <- subset(epi_data, region == "Eastern Europe")
epi_south_asia <- subset(epi_data, region == "Southern Asia")

# PART 1: VARIABLE DISTRIBUTIONS
# Box plot for Eastern Europe (EPI.new)
ggplot(epi_east_europe, aes(x = region, y = EPI.new)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "EPI Scores in Eastern Europe",
       x = "Region",
       y = "EPI Score") +
  theme_bw()

# Histogram for eastern Europe (EPI.new)
ggplot(epi_east_europe, aes(x = EPI.new)) +
  geom_histogram(fill = "lightgreen", color = "darkgreen", bins = 10) +
  labs(title = "EPI Scores Distribution in Eastern Europe",
       x = "EPI",
       y = "Frequency") +
  theme_bw()


# Box plot for Southern Asia (EPI.new)
ggplot(epi_south_asia, aes(x = region, y = EPI.new)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "EPI Scores in Southern Asia",
       x = "Region",
       y = "EPI Score") +
  theme_bw()

# Histogram for Southern Asia (EPI.new)
ggplot(epi_south_asia, aes(x = EPI.new)) +
  geom_histogram(fill = "lightgreen", color = "darkgreen", bins = 10) +
  labs(title = "EPI Scores Distribution in Southern Asia",
       x = "EPI",
       y = "Frequency") +
  theme_bw()


# QQplot of EPI between the two regions
qqplot(epi_east_europe$EPI.new, epi_south_asia$EPI.new,
       main = "QQ Plot of EPI Scores: Eastern Europe vs Southern Asia",
       xlab = "Eastern Europe EPI Scores",
       ylab = "Southern Asia EPI Scores")

# PART 2: LINEAR MODELS
# Choosing EPI.new as the variable with gdp and population as predictors

# MODEL 1: Without Log
model1 <- lm(EPI.new ~ gdp + population, data = epi_data)
summary(model1) # According to the model, gdp is a stronger predictor of EPI comapred to population

# Graph gdp because it is a stronger predictor
ggplot(epi_data, aes(x = gdp, y = EPI.new)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "GDP vs. EPI (Model 1)",
       x = "GDP",
       y = "EPI") +
  theme_bw()

# Residuals plot for Model 1
plot(model1, which = 1) # The curve signifies that the model might no be a good fit

# MODEL 2: With LOG
model2 <- lm(log(EPI.new) ~ log(gdp) + log(population), data = epi_data)
summary(model2)

ggplot(epi_data, aes(x = log(gdp), y = log(EPI.new))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Log of EPI vs. Log of GDP (Model 2)",
       x = "log(GDP)",
       y = "log(EPI)") +
  theme_bw()

# Residuals plot for Model 2
plot(model2, which = 1) 
# ^^^ There is a slight curve on this plot as well indicating the relationship 
# might not be perfectly linear

# Section 2.2

# MODEL 1: Without Log
model1 <- lm(EPI.new ~ gdp + population, data = epi_south_asia)
summary(model1) # According to the model, gdp is a stronger predictor of EPI comapred to population

# Graph gdp because it is a stronger predictor
ggplot(epi_south_asia, aes(x = gdp, y = EPI.new)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "GDP vs. EPI (Model 1)",
       x = "GDP",
       y = "EPI") +
  theme_bw()

# Residuals plot for Model 1
plot(model1, which = 1) # The curve signifies that the model might no be a good fit

# MODEL 2: With LOG and just GDP
model2 <- lm(log(EPI.new) ~ log(gdp), data = epi_south_asia)
summary(model2)

ggplot(epi_south_asia, aes(x = log(gdp), y = log(EPI.new))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Log of EPI vs. Log of GDP (Model 2)",
       x = "log(GDP)",
       y = "log(EPI)") +
  theme_bw()

# Residuals plot for Model 2
plot(model2, which = 1)



# Part 3: Classification with kNN
knn_data <- epi_data %>% select(EPI.new, ECO.new, BDH.new, region)
knn_data$region <- as.factor(knn_data$region)

train_indices <- createDataPartition(knn_data$region, p = 0.7, list = FALSE)
train_data <- knn_data[train_indices, ] # 70% training, 30% test data
test_data <- knn_data[-train_indices, ]

k_value <- data.frame(k = 5)
# Train the kNN model
knn_model <- train(
  region~.,
  data = train_data,
  method = "knn",
  tuneGrid = k_value
)

knn_test_predictions <- predict(knn_model, newdata = test_data)

# Get the labels from the testdata to compare against.
knn_test_true <- test_data$region

# Built in confusion matrix instead of doing as.Matrix() shown in example code
confusion_matrix <- confusionMatrix(data = knn_test_predictions, reference = knn_test_true)
confusion_matrix

cat("Accuracy of correct classifications:", confusion_matrix$overall['Accuracy'], "\n")


# SECTION 3.2 kNN model with 3 other variables
knn_data <- epi_data %>% select(PAR.new, PHL.new, APO.new, region) %>% drop_na()
knn_data$region <- as.factor(knn_data$region)

train_indices <- createDataPartition(knn_data$region, p = 0.7, list = FALSE)
train_data <- knn_data[train_indices, ] # 70% training, 30% test data
test_data <- knn_data[-train_indices, ]

k_value <- data.frame(k = 5)
# Train the kNN model
knn_model <- train(
  region~.,
  data = train_data,
  method = "knn",
  tuneGrid = k_value
)

knn_test_predictions <- predict(knn_model, newdata = test_data)

# Get the labels from the test data to compare against.
knn_test_true <- test_data$region

# Built in confusion matrix instead of doing as.Matrix() shown in example code
confusion_matrix <- confusionMatrix(data = knn_test_predictions, reference = knn_test_true)
confusion_matrix

cat("Accuracy of correct classifications:", confusion_matrix$overall['Accuracy'], "\n")
