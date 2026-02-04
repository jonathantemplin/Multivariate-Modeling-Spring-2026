rm(list = ls()) # Clear workspace

# reset scientific notation to print up to 5 decimal places
options(scipen = 999, digits = 5)

# Load necessary package for regularization
if (!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
} 



# ---------------------------------------------------------
# 1. Data Simulation
# ---------------------------------------------------------
set.seed(123) # Reproducibility

# Number of observations
n = 1000

# Generating Predictors
# 1. Race/Ethnicity (3 levels)
race_ethnicity = sample(c("GroupA", "GroupB", "GroupC"), n, replace = TRUE)

# 2. Student Sex (Binary)
sex = sample(c("Male", "Female"), n, replace = TRUE)

# 3. Previous Year's Test Scores (Continuous, standardized)
prev_test_scores = rnorm(n, mean = 75, sd = 10)

# 4. Curriculum Type (Binary)
curriculum = sample(c("Standard", "Advanced"), n, replace = TRUE)

# 5. Study Hours per Week (Continuous)
study_hours = rpois(n, lambda = 5)

# 6. Parent Education Level (Ordinal/Continuous proxy)
parent_ed = sample(1:5, n, replace = TRUE)

# Create a base dataframe
student_data = data.frame(
  race = as.factor(race_ethnicity),
  sex = as.factor(sex),
  prev_scores = prev_test_scores,
  curriculum = as.factor(curriculum),
  study_hours = study_hours,
  parent_ed = parent_ed
)

# ---------------------------------------------------------
# 2. Creating the "True" Model
# ---------------------------------------------------------
# We define the outcome 'y' (Assessment Score) based on specific rules.
# Most interactions will have NO effect (coefficient = 0).
# only a few specific interactions will matter.

# Base noise
error = rnorm(n, 0, 5)

# True Model Calculation:
# - Main effects for Prev Scores and Parent Ed
# - Interaction: Sex * Curriculum (Females in Advanced do better)
# - Interaction: Prev_Scores * Study_Hours (Effort multiplies ability)
# - 3-Way: GroupC * Advanced * Study_Hours (Specific boost)

# Helper for binary logicals
is_female = ifelse(student_data$sex == "Female", 1, 0)
is_advanced = ifelse(student_data$curriculum == "Advanced", 1, 0)
is_groupC = ifelse(student_data$race == "GroupC", 1, 0)

# Calculate outcome
assessment_score = 
  50 +                                            # Intercept
  0.5 * student_data$prev_scores +                # Main Effect
  1.5 * student_data$parent_ed +                  # Main Effect
  3.0 * (is_female * is_advanced) +               # 2-way Interaction (True Non-Zero)
  0.1 * (student_data$prev_scores * student_data$study_hours) + # 2-way Interaction (True Non-Zero)
  2.0 * (is_groupC * is_advanced * student_data$study_hours) +  # 3-way Interaction (True Non-Zero)
  error

# ---------------------------------------------------------
# 3. Generating the High-Dimensional Interaction Matrix
# ---------------------------------------------------------
# In standard regression, manually coding 2-way and 3-way interactions 
# for all variables is tedious. Here we use model.matrix 
# to explode the number of predictors to include ALL combinations.

# Formula requesting all 3-way interactions (^3)
X_matrix = model.matrix( ~ .^3, data = student_data)

# Remove intercept column (glmnet adds its own)
X_matrix = X_matrix[, -1]

# Convert outcome to vector
y_vector = assessment_score

print(paste("Number of predictors (including interactions):", ncol(X_matrix)))

# This simulates the scenario where predictors are many, potentially 
# causing OLS to overfit or fail.

# ---------------------------------------------------------
# 4. Ridge Regression (Alpha = 0)
# ---------------------------------------------------------
# Ridge shrinks coefficients toward zero but rarely sets them exactly to zero.
# Useful for handling multicollinearity.

# Fit Ridge with Cross-Validation
cv_ridge = cv.glmnet(
  x = X_matrix, 
  y = y_vector, 
  alpha = 0,       # alpha=0 is Ridge
  standardize = TRUE # Important step for regularization 
)

# Plotting the MSE against Lambda
plot(cv_ridge, main = "Ridge Regression Cross-Validation")

# Extract coefficients at the best lambda
ridge_coefs = coef(cv_ridge, s = "lambda.min")
coef(cv_ridge)

# Ridge results: Many small, non-zero coefficients
print("Ridge Non-Zero Coefficients Summary:")
# Sum of non-zero coefficients (Ridge usually keeps all)
print(sum(ridge_coefs != 0))

# ---------------------------------------------------------
# 5. LASSO Regression (Alpha = 1)
# ---------------------------------------------------------
# LASSO assumes a "sparse" model and will zero out irrelevant 
# interactions.

# Fit LASSO with Cross-Validation
cv_lasso = cv.glmnet(
  x = X_matrix, 
  y = y_vector, 
  alpha = 1,       # alpha=1 is LASSO 
  standardize = TRUE
)

# Plotting the LASSO path
plot(cv_lasso, main = "LASSO Regression Cross-Validation")

# ---------------------------------------------------------
# 6. Extracting and Comparing Results
# ---------------------------------------------------------
# We use the "One Standard Error Rule" for a more parsimonious model [cite: 245, 255]
lasso_coefs = coef(cv_lasso, s = "lambda.1se")
print(lasso_coefs)

# Identify which variables LASSO selected
selected_indices = which(lasso_coefs != 0)
selected_names = rownames(lasso_coefs)[selected_indices]

print("------------------------------------------------")
print("True drivers of the model were:")
print("1. prev_scores (Main)")
print("2. parent_ed (Main)")
print("3. sexFemale:curriculumAdvanced (Interaction)")
print("4. prev_scores:study_hours (Interaction)")
print("5. raceGroupC:curriculumAdvanced:study_hours (3-way)")
print("------------------------------------------------")
print("LASSO Selected Variables (Lambda 1SE):")
print(selected_names)

# Note: In the output, you should see that LASSO successfully eliminated 
# the vast majority of the useless 2-way and 3-way interactions generated 
# by the model.matrix step.

