library(tidyverse)
library(ggplot2)


NY_data <- read.csv("NY-House-Dataset.csv")

View(NY_data)

Price <- NY_data$PRICE
PropertySqFt <- NY_data$PROPERTYSQFT
Beds <- NY_data$BEDS
Bath <- NY_data$BATH

# NA values
na.indices <- is.na(Price) | is.na(PropertySqFt) | is.na(Beds) | is.na(Bath)
# drop NAs
Price.compl <- Price[!na.indices]
PropertySqFt.compl <- PropertySqFt[!na.indices]
Beds.compl <- Beds[!na.indices]
Bath.compl <- Bath[!na.indices]
NY_data.compl <- data.frame(Price = Price.compl, PropertySqFt = PropertySqFt.compl, Beds = Beds.compl, Bath = Bath.compl)

# Log transform price for a better fit
NY_data.compl$LogPrice <- log(NY_data.compl$Price)
NY_data.compl$LogProperty <- log(NY_data.compl$PropertySqFt)
NY_data.compl$LogBeds <- log(NY_data.compl$Beds)

# The +1 is to avoid log(0) == inf errors
NY_data.compl$LogBath <- log(NY_data.compl$Bath + 1)

#Fit 3 linear models with Price as the response variable and with combinations of PropertySqFt, Beds, and Bath as predictors.
model1 = lm(LogPrice ~ LogProperty, data = NY_data.compl)
model2 = lm(LogPrice ~ LogProperty + LogBeds, data = NY_data.compl)
model3 = lm(LogPrice ~ LogProperty + LogBeds + LogBath, data = NY_data.compl)

# Boxplot for outliers in price
boxplot(NY_data.compl$Price, main="Boxplot for Price", ylab="Price")

# Clean the ouliers
# There are still outliers in the data but they are not as egregious
price_outliers <- boxplot.stats(NY_data.compl$Price)$out
NY_data.compl <- NY_data.compl %>% filter(!(Price %in% price_outliers))
boxplot(NY_data.compl$Price, main="Boxplot for Price", ylab="Price")


### MODEL 1: PRICE + PROPERTYSQFT ###

# The p < 2.2e-16 meaning that this model is statistically significant
summary(model1)

ggplot(NY_data.compl, aes(x = LogProperty, y = LogPrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  theme_bw()+
  labs(title = "Log Price vs. Log Property Square Footage",
       x = "Property Square Footage",
       y = "Price")


### MODEL 2: PROPRTYSQFT + BEDS ###
summary(model2)
ggplot(NY_data.compl, aes(x = LogBeds, y = LogPrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  theme_bw() +
  labs(title = "Log Price vs. Log Beds",
       x = "Beds",
       y = "Price")

### MODEL 3: PROPRTYSQFT + BEDS + BATH ###
summary(model3)
ggplot(NY_data.compl, aes(x = LogBath, y = LogPrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "green") +
  theme_bw() +
  labs(title = "Log Price vs. Log Bath",
       x = "Bath",
       y = "Price")

