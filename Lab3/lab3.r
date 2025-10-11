library(tidyverse)
library(class)

abalone_data <- read.csv("abalone_dataset.csv")


# EXERCISE 1
abalone_data$age_group <- cut(abalone_data$rings, br =c (0, 8, 11, 35),
                            labels = c("young", "adult", "old"))

## alternative way of setting age.group
abalone_data$age_group[abalone_data$rings <= 8] <- "young"
abalone_data$age_group[abalone_data$rings > 8 & abalone_data$rings<=11] <- "adult"
abalone_data$age_group[abalone_data$rings > 11 & abalone_data$rings<=35] <- "old"


abalone_data$sex <- as.numeric(as.factor(abalone_data$sex))

# Split data into a 70% training set and a 30% testing set.
train_indices <- sample(1:nrow(abalone_data), 0.7 * nrow(abalone_data))
train_data <- abalone_data[train_indices, ]
test_data <- abalone_data[-train_indices, ]

# Separate the labels from the feature sets.
train_labels <- train_data$age_group
test_labels <- test_data$age_group

# 2. Train and Evaluate Two kNN Models

# --- Model 1: Using a broad set of physical measurement features ---
features1 <- c("sex", "length", "diameter", "height", "whole_weight",
               "shucked_wieght", "viscera_wieght", "shell_weight")

# Normalize the features for both training and testing sets.
# kNN is sensitive to the scale of data, so normalization is crucial.
train_features1 <- scale(train_data[, features1])
test_features1 <- scale(test_data[, features1])

# Train the kNN model with k=5
knn_pred1 <- knn(train = train_features1,
                 test = test_features1,
                 cl = train_labels,
                 k = 5)

# Create a contingency table to evaluate the model.
print("--- Model 1: Broad Feature Set ---\n")
cont_table1 <- table(Predicted = knn_pred1, Actual = test_labels)
print("Contingency Table for Model 1:")
print(cont_table1)

# (accuracy = correct classifications / total observations)
accuracy1 <- sum(diag(cont_table1)) / sum(cont_table1)
print("Accuracy for Model 1:", round(accuracy1, 4), "\n\n")


# --- Model 2: Using a smaller, more focused subset of features ---
features2 <- c("length", "diameter", "height", "shell_weight")

# Normalize the features for the second model.
train_features2 <- scale(train_data[, features2])
test_features2 <- scale(test_data[, features2])

# Train the kNN model with k=5
knn_pred2 <- knn(train = train_features2,
                 test = test_features2,
                 cl = train_labels,
                 k = 5)

# Evaluate Model 2 using a contingency table.
print("--- Model 2: Focused Feature Set ---\n")

cont_table2 <- table(Predicted = knn_pred2, Actual = test_labels)
print("Contingency Table for Model 2:")
print(cont_table2)

# Calculate and print the accuracy of Model 2.
accuracy2 <- sum(diag(cont_table2)) / sum(cont_table2)
print("Accuracy for Model 2:", round(accuracy2, 4), "\n\n")


# 3. Compare Models and Find the Optimal k

# Determine which model performed better based on accuracy.
if (accuracy1 > accuracy2) 
{
  print("Model 1 performed better. Finding optimal k for this model.\n")
  better_train_features <- train_features1
  better_test_features <- test_features1
} else 
{
  print("Model 2 performed better. Finding optimal k for this model.\n")
  better_train_features <- train_features2
  better_test_features <- test_features2
}

# Test k values from 1 - 30
k_values <- 1:30
accuracies <- numeric(length(k_values)) # Vector to store accuracies

# Loop through each k value, train a model, and record its accuracy.
for (i in seq_along(k_values)) 
{
  k <- k_values[i]
  
  knn_pred_tune <- knn(train = better_train_features,
                       test = better_test_features,
                       cl = train_labels,
                       k = k)
  
  cont_table_tune <- table(Predicted = knn_pred_tune, Actual = test_labels)
  
  accuracies[i] <- sum(diag(cont_table_tune)) / sum(cont_table_tune)
}

# Find the optimal k that yields the highest accuracy.
optimal_k <- k_values[which.max(accuracies)]
max_accuracy <- max(accuracies)

print("\n--- Optimal k Value ---\n")
print("Optimal k:", optimal_k, "\n")
print("Highest Accuracy Achieved:", round(max_accuracy, 4), "\n\n")

# Create a data frame for plotting the results.
accuracy_df <- data.frame(k = k_values, Accuracy = accuracies)

# Plot Accuracy vs. k-Value to visualize the optimal k.
ggplot(accuracy_df, aes(x = k, y = Accuracy)) +
  geom_line(color = "dodgerblue", size = 1) +
  geom_point(color = "dodgerblue") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
  labs(title = "k-NN Performance: Accuracy vs. Number of Neighbors (k)",
       subtitle = paste("Optimal k =", optimal_k, "with Accuracy =", round(max_accuracy, 4)),
       x = "k (Number of Neighbors)",
       y = "Model Accuracy") +
  theme_bw()

# --- EXERCISE 2: K-Means and PAM Clustering ---

# Load libraries for clustering and visualization
library(cluster)
library(factoextra)

# We will use the scaled feature set from the better performing kNN model
# The 'better_train_features' variable holds this data.
best_features_scaled <- scale(abalone_data[, head(features1, -1)]) # Using Model 1 features as it was better

# --- K-Means Clustering ---

# 1. Find the optimal number of clusters (K) for K-Means
# We use the silhouette method to find the optimal K.
print("\n--- Finding Optimal K for K-Means ---\n")

kmeans_opt_k_plot <- fviz_nbclust(best_features_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Optimal K for K-Means (Silhouette Method)")

print(kmeans_opt_k_plot)

# From the plot, we determine the optimal k (the one with the highest avg. silhouette width)
# Let's assume the plot suggests K=2 is optimal. We will proceed with that.
optimal_k_kmeans <- 2
print("Optimal K for K-Means is:", optimal_k_kmeans, "\n\n")

# 2. Train the K-Means model with the optimal K
kmeans_model <- kmeans(best_features_scaled, centers = optimal_k_kmeans, nstart = 25)

# 3. Create a silhouette plot for the K-Means model
print("--- K-Means Silhouette Plot ---\n")

kmeans_silhouette <- silhouette(kmeans_model$cluster, dist(best_features_scaled))
kmeans_sil_plot <- fviz_silhouette(kmeans_silhouette) +
  labs(title = "K-Means Silhouette Plot",
       subtitle = paste("k =", optimal_k_kmeans))

print(kmeans_sil_plot)


# --- PAM Clustering ---


