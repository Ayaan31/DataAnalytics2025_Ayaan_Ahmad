####################################
##### Abalone Data Preparation #####
####################################

# read dataset
abalone_data <- read.csv("abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone_data$age.group <- cut(abalone_data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## alternative way of setting age.group
abalone_data$age.group[abalone_data$rings <= 8] <- "young"
abalone_data$age.group[abalone_data$rings > 8 & abalone_data$rings<=11] <- "adult"
abalone_data$age.group[abalone_data$rings > 11 & abalone_data$rings<=35] <- "old"


