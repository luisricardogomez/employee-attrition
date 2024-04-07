###
#
#
# Project: Employee Attrition
#
# Created by Jordan Larot
#
# Created on 5 November 2022
#
#
###

###  Import libraries #### 
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caTools)
library(pROC)
library(cowplot)
library(car)
library(caret)
library(broom)
library(pscl) 
library(Hmisc)
library(oddsratio)

### Importing and Cleaning Data ####

# Define file path
FILE <- file.choose()

# Import data
df <- read.csv(FILE) %>%
  data.frame()

# Get list of column names
colnames(df)

# Preview data
head(df)

# Display column data types and values
str(df)

# Check unique values for 'Over18'
unique(df$Over18)

# Check unique values for 'StandardHours'
unique(df$StandardHours)

# Drop columns: EmployeeCount, EmployeeNumber, Over18, and StandardHours
cols <- c('EmployeeCount', "EmployeeNumber", "Over18", 'StandardHours')
df <- select(df, -cols)

# Get column names
colnames(df)

# Convert column data types into nominal factors
df$Attrition <-ifelse(df$Attrition == "Yes", 1, 0)
df$OverTime <- ifelse(df$OverTime == "Yes", 1, 0)
df$BusinessTravel <- as.factor(df$BusinessTravel)
df$Department <- as.factor(df$Department)
df$EducationField <- as.factor(df$EducationField)
df$Gender <- as.factor(df$Gender)
df$JobRole <- as.factor(df$JobRole)
df$MaritalStatus <- as.factor(df$MaritalStatus)

# Specify ordered factors
df$Education <- factor(df$Education, ordered = TRUE, levels=c(1, 2, 3, 4, 5))
df$JobInvolvement <- factor(df$JobInvolvement, ordered = TRUE, levels=c(1, 2, 3, 4))
df$JobSatisfaction <- factor(df$JobSatisfaction, ordered = TRUE, levels=c(1, 2, 3, 4))
df$WorkLifeBalance <- factor(df$WorkLifeBalance, ordered= TRUE, levels=c(1, 2, 3, 4))
df$EnvironmentSatisfaction <- factor(df$EnvironmentSatisfaction, ordered= TRUE, levels=c(1, 2, 3, 4))
df$JobLevel <- factor(df$JobLevel, ordered= TRUE, levels=c(1, 2, 3, 4, 5))
df$PerformanceRating <- factor(df$PerformanceRating, ordered= TRUE, levels=c(1, 2, 3, 4))
df$RelationshipSatisfaction <- factor(df$RelationshipSatisfaction, ordered= TRUE, levels=c(1, 2, 3, 4))
df$StockOptionLevel <- factor(df$StockOptionLevel, ordered= TRUE, levels=c(0, 1, 2, 3))

### Descriptive Statistics ####

# Preview summary
summary(df)

# Interpretation:
# The median age of employees at the company is 37 years
# Around 16.12% of the employees left the company.
# The company is mainly compromised of Research & Development Workers
# 28.3% of employees work overtime
# Work Life balance at the company is good
# The employees mainly have Bachelor's / Master's degree (3 & 4)
# 60% of employees are male
# The median hourly rate is high at $66
# Employees are generally satisfied with their job

# Count number of columns
sprintf("We are working with %s features", ncol(df) - 1)

### Data Visualization ####

# We made visualizations on Tableau for the presentation 

# Age histogram 
ggplot(df, aes(x=Age)) + 
  geom_histogram(binwidth=5) 

# Interpretation
# Employees at this company are mainly within 30 to 45 years old
# Age is skewed to the right

# YearsAtCompany histogram 
ggplot(df, aes(x=YearsAtCompany)) + 
  geom_histogram(bins=15)

# Interpretation 
# YearsAtCompany is skewed to the right
# Most employees at the company stay for 0 to 10 years

# Histogram for hourly rate 
ggplot(df, aes(x=HourlyRate)) + 
  geom_histogram(bins=20)

# Interpretation
# The 'HourlyRate' is somewhat evenly distributed at the company

# Attrition by Department
ggplot(data=df, aes(x=Department, y=Attrition)) + 
  geom_bar(stat="identity", width=0.5, fill='steelblue') + 
  theme_minimal() +
  coord_flip()

# Interpretation 
# Research & Development workers tended to leave the most, while employees at HR had the lowest attrition

# Attrition by Gender
ggplot(data=df, aes(x=Gender, y=Attrition, color="Attrition")) +
  geom_bar(stat="identity", width=0.5, color='steelblue') + 
  theme_minimal() + 
  coord_flip()

# Interpretation
# More males left the companies than females

# Attrition by Job Satisfaction  
ggplot(data=df, aes(x=JobSatisfaction, y=Attrition, color="Attrition")) +
  geom_bar(stat="identity", width=0.5, color='steelblue') + 
  theme_minimal() + 
  coord_flip()

# Interpretation
# Employees that had 'High' job satisfaction left the most

# Attrition by Job Role
ggplot(data=df, aes(x=JobRole, y=Attrition)) + 
  geom_bar(stat="identity", width=0.5, fill='steelblue') + 
  theme_minimal() + 
  coord_flip()

# Interpretation 
# Lab technicians, sales executives, and research scientists 
# are the job roles that have the highest attrition

# Attrition by Education 
ggplot(data=df, aes(x=Education, y=Attrition, color="Attrition")) +
  geom_bar(stat="identity", width=0.5, color='steelblue') + 
  theme_minimal() + 
  coord_flip()

# Interpretation
# People with bachelor's and master's in Life Sciences tended to leave the most

### Data Preparation ####

#### Create dummy variables ####
df$BusinessTravel <- ifelse(df$BusinessTravel == "Non-Travel", 1, 0) +
  ifelse(df$BusinessTravel == "Travel_Frequently", 2, 0) +
  ifelse(df$BusinessTravel == "Travel_Rarely", 3, 0)
df$Department <- ifelse(df$Department == "Human Resources", 1, 0) + 
  ifelse(df$Department == "Research & Development", 2, 0) + 
  ifelse(df$Department == "Sales", 3, 0)
df$EducationField <- ifelse(df$EducationField == "Human Resources", 1, 0) +
  ifelse(df$EducationField == "Life Sciences", 2, 0) + 
  ifelse(df$EducationField == "Marketing", 3, 0) + 
  ifelse(df$EducationField == "Medical", 4, 0) + 
  ifelse(df$EducationField == "Other", 5, 0) +
  ifelse(df$EducationField == "Technical Degree", 6, 0)
df$Gender <- ifelse(df$Gender == "Female", 1, 0) + 
  ifelse(df$Gender == "Male", 2, 0)
df$JobRole <- ifelse(df$JobRole == "Healthcare Representative", 1, 0) +
  ifelse(df$JobRole == "Human Resources", 2, 0) +
  ifelse(df$JobRole == 'Laboratory Technician', 3, 0) + 
  ifelse(df$JobRole == 'Manager', 4, 0) +
  ifelse(df$JobRole == 'Manufacturing Director', 5, 0) + 
  ifelse(df$JobRole == 'Research Director', 6, 0) +
  ifelse(df$JobRole == 'Research Scientist', 7, 0) +
  ifelse(df$JobRole == 'Sales Executive', 8, 0) + 
  ifelse(df$JobRole == 'Sales Representative', 9, 0)
df$MaritalStatus <- ifelse(df$MaritalStatus == "Divorced", 1, 0) +
  ifelse(df$MaritalStatus == "Married", 2, 0) +
  ifelse(df$MaritalStatus == "Single", 3, 0)

# Convert categorical columns to numeric
df$Education <- as.numeric(df$Education)
df$JobInvolvement <- as.numeric(df$JobInvolvement)
df$JobSatisfaction <- as.numeric(df$JobSatisfaction)
df$WorkLifeBalance <- as.numeric(df$WorkLifeBalance)
df$EnvironmentSatisfaction <- as.numeric(df$EnvironmentSatisfaction)
df$JobLevel <- as.numeric(df$JobLevel)
df$PerformanceRating <- as.numeric(df$PerformanceRating)
df$RelationshipSatisfaction <- as.numeric(df$RelationshipSatisfaction)
df$StockOptionLevel <- as.numeric(df$StockOptionLevel)

#### Check for extreme outliers ####

# Create model to use Cook's Distance
model <- glm(Attrition ~.,
             data=df, 
             family='binomial')

# Extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 

# Plot standardized residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Attrition), alpha = .5) +
  theme_bw()

# Get number of values greater than 3
n <- sum(model.data$.std.resid > 3)

# Identify values greater than 3
plot(model, which = 4, id.n = n)

# Drop observations with standardized residuals greater than 3
df <- df[-c(34, 46, 127, 750, 1112),]

# Verify
summary(df)

# Check for missing values
table(is.na(df))

### Feature selection ####

# Get only the independent variable (features)
features <- select(df, -Attrition)

# Perform multicollinearity 
corr <- abs(cor(features))

# Find rows that are greater than 0.7
m <- (corr >= 0.7) & (corr < 1)
m

# Specify columns to drop 
drop_cols <- c('JobLevel', 
               'TotalWorkingYears', 
               'PercentSalaryHike', 
               'YearsInCurrentRole', 
               'YearsWithCurrManager')

# Drop columns
df <- select(df, -drop_cols)

# Check correlation results
corr <- rcorr(as.matrix(df))
corr$P # all p-values are less than 0.05

### Model Building ####

# Set seed for reproducibility
set.seed(42) # the answer to the universe - the number 42

# Split data into training and testing set
sample <- sample.split(df, SplitRatio = .80)

df_train <- subset(df, sample == TRUE)
df_test <- subset(df, sample == FALSE)

#### Model #1 (all variables) ####
log_reg <- glm(Attrition ~ .,
               data=df_train,
               family='binomial')

# See model summary
summary(log_reg)

# List insignificant variables
insig_cols <- c('BusinessTravel',
                'DailyRate',
                'Education',
                'EducationField',
                'HourlyRate', 
                'JobRole',
                'MonthlyRate',
                'PerformanceRating',
                'StockOptionLevel',
                'TrainingTimesLastYear',
                'WorkLifeBalance')

# Drop insignificant variables
df_sig <- select(df_train, -insig_cols)

#### Model #2  ####
log_reg_2 <- glm(Attrition ~ .,
                 data=df_sig,
                 family='binomial')

# Confirm all variables are significant
summary(log_reg_2)

# Drop Department
df_sig_2 <- select(df_sig, -Department)

#### Model #3 ####
log_reg_3 <- glm(Attrition ~ ., 
                 data=df_sig_2,
                 family='binomial')

# Confirm all variables are significant
summary(log_reg_3)

#### Cut model down to 10 variables ####

# View correlation results
corr <- rcorr(as.matrix(df_sig_2))
corr$r

# Specify columns to drop 
drop_cols <- c('Gender',
               'YearsSinceLastPromotion',
               'RelationshipSatisfaction')

# Drop columns
df_sig_3 <- select(df_sig_2, -drop_cols)

#### Model #4 ####
log_reg_4 <- glm(Attrition ~ .,
                   data=df_sig_3,
                   family='binomial')

summary(log_reg_4)

# Drop YearsAtCompany
df_final <- select(df_sig_3, -YearsAtCompany)

#### Final model ####
final_model <- glm(Attrition ~ .,
                 data=df_final,
                 family='binomial')

summary(final_model)

### Evaluating Model Performance #### 

#### Calculate McFadden's R-square ####
pR2(final_model)
print("McFadden's R-square is 0.23")

#### Calculate Model Significance ####
p_value <- 1 - pchisq(2*(final_model$deviance/-2 - final_model$null.deviance/-2), 
                      df=length(final_model$coefficients)-1)
p_value

# Make predictions
prediction <- predict(final_model, df_test, type="response")

#### Calculate accuracy ####
predicted.classes <- ifelse(prediction > 0.5, "1", "0")
accuracy <- mean(predicted.classes == df_test$Attrition)
sprintf('The accuracy of the model is %s%%', round(accuracy * 100, 2))

#### Create confusion matrix #### 
confusionMatrix(as.factor(df_test$Attrition), as.factor(predicted.classes))

### Odds Ratio ####

### log odds ###
summary(final_model)

#### Odds ratio only ####
exp(coef(final_model))

#### Odds Ratio and 95% Confidence Interval ####
exp(cbind(OR = coef(final_model), confint(final_model)))

#### Calculate Odds Ratio for Specific Increment Step of Continuous Variable ####
or_glm(data=df_final, 
       model=final_model,
       incr = list(Age = 1, 
                        DistanceFromHome=1, 
                        EnvironmentSatisfaction=1,
                        JobInvolvement=1,
                        JobSatisfaction=1,
                        MaritalStatus=1,
                        MonthlyIncome=2500,
                        NumCompaniesWorked=1,
                        OverTime=1))

#### Odds Ratio Interpretation ####
sprintf('A one unit increase in Age, makes the odds of attrition 0.96 times less likely to happen')
sprintf('A one unit increase in DistanceFromHome, increases the  odds of attrition by 1.03')
sprintf('A one unit increase in EnvironmentSatisfaction, decreases the odds of attrition by 0.67')
sprintf('A one unit increase in JobInvolvement, decreases the odds of attrition by 0.59')
sprintf('A one unit increase in JobSatisfaction, decreases the odds that an employee leave by 0.70')
sprintf('A one unit increase in MaritalStatus, increases the odds of attrition by 1.84')
sprintf('A $2500 increase in MonthlyIncome, makes the odds of attrition 0.72 times less likely to happen')
sprintf('A one unit increase in NumCompaniesWorked, increases the odds of attrition by 1.19')
sprintf('A one unit increase in OverTime, increases the odds of attrition by 5.21')

### Checking Model Assumptions ####

#### Assumption #1 ####

# The response variable is binary
# Only two possible outcomes: Yes (1) or No (0)

#### Assumption #2 ####

# The observations are independent 
# There are no duplicate rows in the data
sum(duplicated(df_final) == TRUE)

#### Assumption #3 ####

# There is no multicollinearity among explanatory variables
corr <- abs(cor(select(df_final, -Attrition)))

# Find rows that are greater than 0.7
mc <- (corr >= 0.7) & (corr < 1)
mc

#### Assumption #4 #### 

# There are no extreme outliers
# Cook's Distance

# Extract model results
model.data <- augment(final_model) %>% 
  mutate(index = 1:n()) 

# Plot standardized residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Attrition), alpha = .5) +
  theme_bw()

# Get number of values greater than 3
n <- sum(model.data$.std.resid > 3)

# Identify values greater than 3
plot(model, which = 4, id.n = n)

#### Assumption #5 #### 

# There is a linear relationship between explanatory variables and the logit of the response variable
corr <- rcorr(as.matrix(df_final))
corr$r

# Predict the probability (p) of Attrition (Yes)
probabilities <- predict(final_model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Select only numeric predictors
mydata <- df_final %>%
  dplyr::select_if(is.numeric)

predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y")

#### Assumption #6 ####

# The sample size is sufficiently large

# Calculate sample size needed
sample_needed <- (10 * 9) / 0.16 # (minimum cases * number of explanatory variables) / least frequent outcome

# Check if data has enough observations
if (nrow(df_final) > sample_needed) {
  print('There are enough samples')
} else {
  print('There are not enough samples')
}
