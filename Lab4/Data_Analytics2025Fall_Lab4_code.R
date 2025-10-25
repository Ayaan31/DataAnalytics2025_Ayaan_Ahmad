##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
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

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###
# The first column is the "Type" which is the label
# We want to grab every column that is not this first one
wine_features = wine[, -1]

# Put "Type" column in this data frame
wine_labels = wine[, 1]

wine_pca <- princomp(wine_features, cor = TRUE)
pca_summary <- summary(wine_pca)
pca_summary

# Plotted dataset 1st and 2nd Principal Components
ggplot(wine_pca$scores, aes(x = Comp.1, y = Comp.2, color = wine$Type)) +
  geom_point(size = 3) +
  labs(
    title = "First Two Pricipal Components of Wine Dataset",
    x = "PC 1",
    y = "PC 2",
    color = "Wine Type"
  ) +
  theme_bw()

loadings <- wine_pca$loadings

# Top contributors for PC1 (Comp.1)
pc1_loadings_sorted <- loadings[order(abs(loadings[, 1]), decreasing = TRUE), 1]
print("Top Contributors to PC1 (by absolute value)")
print(pc1_loadings_sorted)

# Top contributors for PC2 (Comp.2)
pc2_loadings_sorted <- loadings[order(abs(loadings[, 2]), decreasing = TRUE), 2]
print("Top Contributors to PC2 (by absolute value)")
print(pc2_loadings_sorted)

train_indices <- createDataPartition(wine$Type, p = 0.7, list = FALSE)

# Original dataset split
train_set_orig <- wine[train_indices, ]
test_set_orig <- wine[-train_indices, ]

# Store the "ground truth" labels from the test set for comparison
test_labels_actual <- test_set_orig$Type


# Create a new dataframe with ONLY PC scores and the Type
pca_data <- data.frame(
  Type = wine$Type,
  wine_pca$scores
)

# Split the PCA data using the same indices for a fair comparison
train_set_pca <- pca_data[train_indices, ]
test_set_pca <- pca_data[-train_indices, ]

ctrl <- trainControl(method = "cv", number = 10)

model_orig <- train(
  Type ~ .,  # For this one, we use all the features
  data = train_set_orig,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 10  # Automatically tests 10 different 'k' values
)

# Show the best k that was chosen
print(model_orig)

model_pca <- train(
  Type ~ Comp.1 + Comp.2,  # Formula: "Predict Type using only PC1 and PC2"
  data = train_set_pca,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 10
)

# Show the best k that was chosen
print(model_pca)

# Get predictions on the test set
pred_orig <- predict(model_orig, newdata = test_set_orig)
pred_pca <- predict(model_pca, newdata = test_set_pca)

# Generate the full comparison report
cm_orig <- confusionMatrix(data = pred_orig, reference = test_labels_actual)
cm_pca <- confusionMatrix(data = pred_pca, reference = test_labels_actual)

# Print the results
print("Orginal Dataset Model Performance:")
print(cm_orig)

print("PCA Dataset Model Performance:")
print(cm_pca)
