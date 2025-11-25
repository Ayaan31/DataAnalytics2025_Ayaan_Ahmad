library(tidyverse)
library(randomForest)
library(MASS)
library(caret)

# First part of code is to load in, filter data, and save filtered data
# fatigue_data <- read.csv("post_pandemic_remote_work_health_impact_2025.csv")
# 
# unique(fatigue_data$Job_Role)
# 
# # These jobs are the ones where people most likely are writing and/or reading code
# coding_jobs <- c('Data Analyst', "DevOps Engineer", "Software Engineer",
#                  "IT Support", "Data Scientist", "Quality Assurance", 
#                  "Technical Writer", "UX Desginer")
# 
# programmer_data <- fatigue_data %>% 
#   filter(Job_Role %in% coding_jobs) %>% 
#   mutate(Survey_Date = as.Date(Survey_Date)) %>% 
#   drop_na()
# 
# 
# unique(programmer_data$Job_Role)
# 
# write.csv(programmer_data, file = "Programmer_Fatigue_Data.csv", row.names = FALSE)


programmer_data <- read.csv("Programmer_Fatigue_Data.csv")
programmer_data <- programmer_data %>% mutate(Survey_Date = as.Date(Survey_Date))

# Need to factorize the labels
# Also create a separate column containing the number of physical issues
data_clean <- programmer_data %>% 
  mutate(Num_Physical_Issues = ifelse(Physical_Health_Issues == "None", 0, 
                                str_count(Physical_Health_Issues, ";") + 1)) %>% 
  mutate(
        Burnout_Level = factor(Burnout_Level, levels = c("Low", "Medium", "High"), 
                               ordered = TRUE),
        Gender = as.factor(Gender),
        Region = as.factor(Region),
        Industry = as.factor(Industry),
        Job_Role = as.factor(Job_Role),
        Work_Arrangement = as.factor(Work_Arrangement),
        Salary_Range = factor(Salary_Range, 
                              levels = c("$40K-60K", "$60K-80K", "$80K-100K", 
                                         "$100K+"), ordered = TRUE)
        ) %>% drop_na()

model_data <- data_clean %>%
  select(Burnout_Level, Age, Gender, Region, Industry, Job_Role, 
         Work_Arrangement, Hours_Per_Week, Work_Life_Balance_Score, 
         Social_Isolation_Score, Num_Physical_Issues, Salary_Range)

set.seed(2025)
rf_model <- randomForest(Burnout_Level ~ ., data = model_data)
print(rf_model)

# Plot showing the importance of each feature according to random forest
varImpPlot(rf_model,
           main = "Feature Importance: What drives Burnout?",
           col = "blue",
           pch = 19)

# Plot showing partial dependence of Hours/Week on High Burnout Risk
partialPlot(rf_model,
            pred.data = model_data,
            x.var = Hours_Per_Week,
            which.class = "High",
            main = "Effect of Working Hours on High Burnout Risk",
            xlab = "Hours Per Week",
            ylab = "Log-odds of High Burnout")

# Plot showing Hours/Week and its affect on Burnout Level
ggplot(data_clean, aes(x = Burnout_Level, y = Hours_Per_Week, fill = Burnout_Level)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Working Hours by Burnout Level",
       subtitle = "Do higher burnout levels correlate with higher median hours",
       x = "Burnout Level",
       y = "Hours Per Week") +
  scale_fill_manual(values = c("Low" = "green", "Medium" = "orange", "High" = "red"))


# The plot with hours per weekend burnout doesn't show much
# It could be that multiple factors in combination affect burnout level
data_clean$Age_Group <- cut(data_clean$Age, 
                          breaks = c(20, 30, 40, 50, 60, 70), 
                          labels = c("20s", "30s", "40s", "50s", "60s+"))

# Plot with Hours/Week and Age Groups
ggplot(data_clean, aes(x = Burnout_Level, y = Hours_Per_Week, fill = Burnout_Level)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Age_Group) + 
  theme_bw() +
  labs(title = "Impact of Hours on Burnout by Age Group",
       subtitle = "Does age group in combination with Hours/Week correlate with high burnout level?",
       x = "Burnout Level",
       y = "Hours Per Week") +
  scale_fill_manual(values = c("Low" = "green", "Medium" = "orange", "High" = "red"))



