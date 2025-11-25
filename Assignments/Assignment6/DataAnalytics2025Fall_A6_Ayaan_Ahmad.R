library(tidyverse)
library(pacman)
library(purrr)
library(randomForest)

dat <- read.csv("DATA.csv")

column_names <- c(
  "id",
  "student_age",    
  "sex",            
  "hs_type",        
  "scholarship",    
  "additional_work",
  "activity",       
  "partner",      
  "total_salary", 
  "transport",    
  "accommodation",  
  "mother_edu",     
  "father_edu",     
  "siblings",       
  "parental_status",
  "mother_job",     
  "father_job",     
  "study_hours",    
  "read_freq",      
  "read_freq_sci",  
  "attend_seminars",
  "impact",         
  "attend_classes", 
  "prep_midterm_1", 
  "prep_midterm_2", 
  "taking_notes",   
  "listen_classes", 
  "discussion",     
  "flip_classroom", 
  "gpa_last_sem",   
  "gpa_expected",   
  "course_id",      
  "grade"           
)

colnames(dat) <- column_names

# Cleaning data
dat_clean <- dat %>% distinct() %>%  drop_na()
dat_clean <- dat_clean[, -1]

# I want to change the data to be more expressive rather than categorical
dat_fact <- dat_clean %>%
  mutate(
    student_age = factor(student_age, levels=1:3, labels=c("18-21", "22-25", ">26")),
    sex = factor(sex, levels=1:2, labels=c("Female", "Male")),
    hs_type = factor(hs_type, levels=1:3, labels=c("Private", "State", "Other")),
    scholarship = factor(scholarship, levels=1:5, labels=c("None", "25%", "50%", "75%", "Full")),
    total_salary = factor(total_salary, levels=1:5, labels=c("Low", "Low-Mid", "Mid", "Mid-High", "High")),
    grade = factor(grade, levels=0:7, labels=c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA")),
    additional_work = factor(additional_work, levels=1:2, labels=c("Yes", "No")),
    activity = factor(activity, levels=1:2, labels=c("Yes", "No"))
  )

summary(dat_fact)

# PLOT 1: Total Grade Counts
ggplot(dat_fact, aes(x = grade)) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Student Grades", x = "Grade", y = "Count")

# PLOT 2: Grade Count by Sex
ggplot(dat_fact, aes(x = grade, fill = sex)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(title = "Grades by Sex", x = "Grade", y = "Count")

# Numeric data
dat_num <- lapply(dat_clean, as.factor)

# Model 1:
# Anova for each label, checking which fetures affect gpa
anova_results <- list()
for (col in names(dat_num)[sapply(dat_num, is.factor)]) {
  formula <- as.formula(paste("gpa_expected ~", col))
  anova_results[[col]] <- summary(aov(formula, data = dat_clean))
}

sig_results <- list()

for (col in names(anova_results)) {
  
  # Add to list, if p-value < 0.05
  p_value <- anova_results[[col]][[1]][["Pr(>F)"]][1]
  if (p_value < 0.05 && !is.na(p_value)) {
    sig_results[[col]] <- anova_results[[col]]
  }
  
}

final_df <- sig_results %>%
  map_dfr(~ .x[[1]], .id = "Variable") %>% drop_na()

final_df

# MODEL 2:
ggplot(data = dat_fact, aes(x = total_salary, y = gpa_expected, fill = total_salary)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Total Salary", y= "Expected GPA")


# Perform the Kruskal-Wallis test. The data does not look nomral according to
# the above plot
kruskal_result <- kruskal.test(gpa_expected ~ total_salary, data = dat_fact)
kruskal_result


# Model 3: Predicting grade based on other variables

features_rf <- dat_fact %>% select(gpa_last_sem, sex, grade, scholarship, taking_notes, father_edu, mother_edu, parental_status, attend_classes, discussion)

rf_model_grade <- randomForest(
  grade ~ .,
  data = features_rf,
  importance = TRUE
  )

print(rf_model_grade)
importance(rf_model_grade)
varImpPlot(rf_model_grade, main = "Variable Importance for Predicting Student Grade")

grade_counts <- dat_fact %>%
  count(grade)

grade_counts
