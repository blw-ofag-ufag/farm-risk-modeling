# =========================================================================================================
#       title:  CONTRIBUTION OF FARM ATTRIBUTES TO INSPECTION OUTCOMES
#      author:  Marc Beringer (marc.beringer@blw.admin.ch)
#        date:  2025-09-02
# description:  Uses carefully curated data from the FOAG production systems AGIS (structural attributes),
#               ACONTROL (on-farm inspections) and HODUFLU (manure and recycling-fertilizer flows),
#               assembled by Damian Oswald (damian.oswald@blw.admin.ch). Here this data frame serves as
#               the input for logistic models that predict the probability of failing an inspection and
#               enable analysis of the contribution of attributes of Swiss farms to predicted probabilities.
#               All steps are reproducible, although access to the data is strictly limited to FOAG collaborators.
# =========================================================================================================

# =========================================================================================================
# PACKAGES
# =========================================================================================================

{
  library(tidyverse)
  library(patchwork)
  library(emmeans)
  library(lme4)
  library(MASS)
  library(sjPlot)
  library(lmerTest) # adds type III sums of squares significance evaluation to summary() output
  library(effects)
  library(MuMIn) # allows for approximation of R² values in mixed effect models
  library(glmnet) # enables Lasso and Ridge regression
  library(summarytools)
  library(pscl) # McFadden's pseudo R² for GLMs
  library(performance) # Tjur's R² for GLMs
  library(pROC) # enables computation of AUC
  library(ResourceSelection) # enables Hosmer-Lemeshow goodness-of-fit test to assess model calibration
  library(caret) # enables confusionMatrix()
  library(car)
}

# =========================================================================================================
# SOURCE HELPER FUNCTIONS AND LOAD DATA
# =========================================================================================================

getwd()
setwd("./farm-risk-modeling/")

source("scripts/helpers.R")

# Inspection results and farm attributes data
data <- readRDS("data/data.rds")

# A character vector with the variables (farm attributes)
# Alternatively, use predictors of your choice for subsequent statistics
predictors <- readRDS("resources/predictors.rds")


# =========================================================================================================
# INSPECT DATA
# ---------------------------------------------------------------------------------------------------------
# Learn about the structure of the data set, variables and factor levels.
# =========================================================================================================

# Structure of the data set
str(data)
names(data)

# Inspect factor levels
levels(data$result) # The result of the inspection is either Pass (zero flaws), or Fail (at least one flaw).
levels(data$farmType) # 5 types of farms
levels(data$year) # data of 7 years
levels(data$theme) # 10 themes of inspections
levels(data$legalForm) # Only two legal forms

# What is the meaning of the variable "SB"?
# Probably it's "Sömmerungsbeitrag", 14'963/14'997 Sömmerungs- and Gemeinschaftsweidebetriebe have SB == 1
sb_dat <- data %>%
  subset( farmType %in% c('Sömmerungsbetrieb', 'Gemeinschaftsweidebetrieb'))

sb_dat <- data %>%
  subset( SB == 1)


# =========================================================================================================
# CLEAN DATA AND SPLIT INTO TRAIN- AND TEST-DATA
# ---------------------------------------------------------------------------------------------------------
# Uses the "drop_unique_factors" and "data_split" functions from "helpers.R".
# =========================================================================================================

# Clean and split data into a training data set (80% of the data) and a test data set (20% of the data)
dat1 <- data %>%
  data_split( "Tierschutz", predictors, training_size = 0.8)

# Check if farms in the training data are present in the test data (should return an empty data frame)
dat1$train[dat1$train$farm %in% dat1$test$farm, ]

# =========================================================================================================
# SCALE AND CENTER DATA
# ---------------------------------------------------------------------------------------------------------
# Scale and center the continuous variables in the data. If the variables in the data are on vastly
# different scales (like LN and hofNtot), we need to scale and center them in order for subsequent models
# to work. Scaling sets the mean of every variable to 0 and the standard deviation (SD) to 1.
# Scaling has to be done on the training data and the test data. However, the test data has to be scaled
# with mean and SD of the training data variables, since in the real world, we would not have test data available.
# Therefore, keep track of the mean and SD of each continuous variable of the training data.
# =========================================================================================================

# Scale continuous (numerical) variables of the training data.
# Compute means & SDs from training data
train_stats <- dat1$train %>%
  summarise(
    across(
      where(is.numeric), # only scale numeric variables
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Function to scale any dataset with train_stats
scale_with_train <- function(df, stats) {
  df %>%
    mutate(
      across(
        where(is.numeric),
        ~ (.x - stats[[paste0(cur_column(), "_mean")]]) /
          stats[[paste0(cur_column(), "_sd")]]
      )
    )
}

# Scale training and test data using the training data means and SD
train_scaled <- scale_with_train(dat1$train, train_stats)
test_scaled  <- scale_with_train(dat1$test, train_stats)

# =========================================================================================================
# CONDUCT VARIABLE SELECTION BY LASSO REGRESSION
# ---------------------------------------------------------------------------------------------------------
# Lasso Regression is a linear modeling technique that adds a penalty equal to the absolute values of the
# coefficients, it shrinks some of them to zero. Variables with a coefficient close to zero contribute
# little to the change of the response variable (here inspection result pass, or fail). If two or more
# variables are highly correlated with each other, all but one of them will be reduced to zero. Therefore,
# variables with a coefficient of zero, or close to zero, are not required to increase the explained
# variance of the model in the data. This doesn't mean the variables with coefficients = 0 are not
# important, but they're not important if the variables with larger coefficients are included in the model.
# =========================================================================================================

# For LASSO regression, create a matrix of predictor variables (x),
# this must be a numeric matrix to fit the cv.glmnet() function.
# Remove the response variable (here "result").
# Remove factor variables with a lot of levels (here "farm") so the numeric matrix will not become too large.
train_scaled_lasso <- train_scaled %>%
  dplyr::select(-farm, -result)

# model.matrix() expands factors into dummy variables (one column per factor level).
# Remove the first column (Intercept), which we don't need.
x <- model.matrix(~., data = train_scaled_lasso)[, -1]

# Convert the two-level factor "result" from "Pass/Fail" to "1/0"
y <- ifelse(dat1$train$result == "Pass", 1, 0)

# Perform k-fold cross-validation (default nfolds = 10) to find an optimal lambda (λ) value.
# Splits the data into 10 training and test data sets and rotates the role of "test" across them.
# λ controls how much the coefficients are shrunk. For many λ this runs 10-fold cross-validation
# and computes the average mean squared error (MSE).
model1 <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)

# Identify the optimal λ
# For maximum predictive power, choose "lambda.min", which will shrink fewer variables to zero.
# For a simpler model and harsher variable selection, choose "lambda.1se", which will shrink as many variables to zero as possible.
# Our model previously had too many variables, therefore we choose "lambda.1se" to remove as many as possible.
opt_lambda <- model1$lambda.1se
opt_lambda

# Produce plot of test MSE by lambda value
plot(model1)

# Coefficients of the optimal model
optimal_model <- glmnet(x, y, alpha = 1, lambda = opt_lambda)
coef(optimal_model)

# Visualize LASSO model coefficients
# Create a dataframe from the coefficients of the model.
# Remove the intercept, its coefficient isn't penalized by Lasso reggression
# and can be large because most inspection results are "Pass".
coef_dat <- coef(optimal_model) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Predictor") %>%
  rename(Coefficient = 2) %>%
  subset(Predictor != "(Intercept)")

ggplot(coef_dat, aes(x = reorder(Predictor, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "#52b788", color = "black") +
  labs(title = "Lasso Regression Coefficients", x = "Predictor", y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Select variables with a non-zero coefficient
coef_dat_nz <- coef_dat %>%
  subset(Coefficient != 0)

coef_dat_nz$Predictor

# Visualize the remaining variables and their coefficients
ggplot(coef_dat_nz, aes(x = reorder(Predictor, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "#52b788", color = "black") +
  labs(title = "Lasso Regression Coefficients", x = "Predictor", y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Map dummy variable names back to original variable names to insert them more conveniently into the models.
matched_vars <- sapply(coef_dat_nz$Predictor, function(x) {
  predictors[startsWith(x, predictors)]
})

# Keep only unique original variables
# According to LASSO regression, these are the variables we select for our generalized linear mixed-effects model.
matched_vars <- unique(unlist(matched_vars))
matched_vars


# =========================================================================================================
# Build a generalized linear model (GLM)
# ---------------------------------------------------------------------------------------------------------
# Build a GLM using scaled training data.
# For binary response variables (here "result") we can use a GLM of the family "binomial".
# =========================================================================================================

# Fit the full model using the variables selected with LASSO regression based on the training data
glm1 <- glm(result ~ type + previous + canton + SAK + GVE + DZ + proofMet + organic + milk + trees +
                     oilseeds + artificialGrassland + cattle + sheep,
                     data = train_scaled,
                     family = binomial,
                     na.action = na.fail
                     )

# Summarise the model and identify significant variables
summary(glm1)

# Assess collinearity of explanatory variables in the full model using the Variance Inflation Factor (VIF)
# VIF = 1 -> no collinearity
# VIF 2–5 -> moderate, usually acceptable
# VIF > 5 -> problematic
# The explanatory variables "DZ" and "proofMet" show rather high collinearity.
# This is not a surprise, since "proofMet" (OELN erfüllt) is a prerequisite for the DZ == 1 attribute of a farm.
vif(glm1)

# Further select variables to create a minimal model
# Conduct likelihood ratio tests for each variable
drop1(glm1, test = "LRT")

# Run a minimal model with the non-significant variables removed
glm2 <- update(glm1, . ~ . - SAK - DZ - milk)

glm2 <- glm(result ~ type + previous + canton + GVE + proofMet + organic + trees +
                     oilseeds + artificialGrassland + cattle + sheep,
                     data = train_scaled,
                     family = binomial,
                     na.action = na.fail
                     )

# Summarise the model and identify significant variables
summary(glm2)

# Assess collinearity of explanatory variables in the minimal model using the Variance Inflation Factor (VIF)
# Indeed, DZ is redundant, since all DZ-farms are also farms with "proofMet == 1", but not vice-versa
# "proofMet" == 1 requires the fulfillment of basic animal protection standards and,
# unsurprisingly, is therefore relevant for "Tierschutz" inspection results.
vif(glm2)

# Compare the full model (glm1) and the minimal model (glm2)
# Akaike Information Criterion (AIC) and Bayesian Information Criterion
# are metrics that show the balance between goodness-of-fit and model complexity.
# AIC and BIC are slightly lower in the minimal model (lower = better).
AIC(glm1, glm2)
BIC(glm1, glm2)

# Compare the full and minimal models using a likelihood ratio test
# Conclusion: The minimal and full model's predictive power are equal.
# To keep interpretation of the model simple, we will choose the minimal model due to its fewer variables.
anova(glm2, glm1, test = "Chisq")

# Model calibration
# Check how well the model's predicted probabilities match the observed outcomes.
# Keep in mind that the variable "result" is a two-level factor ("Pass" and "Fail")
levels(train_scaled$result)

# and that the glm() function by default models the probabilities of the second factor level (i.e. "Fail").
# Therefore, in order to match the predicted outcome (i.e. "Fail") with the observed outcome,
# we here need to convert "Fail" into 1 and "Pass" into 0.
train_scaled$result_num <- ifelse(train_scaled$result == "Fail", 1, 0)

# Use the Hosmer-Lemeshow goodness-of-fit test to assess model calibration.
# H₀: The model’s predicted probabilities match the observed event frequencies.
# Therefore, p < 0.05, the predicted probabilities and observed event frequencies are significantly different.
hoslem.test(train_scaled$result_num, fitted(glm1)) # full model
hoslem.test(train_scaled$result_num, fitted(glm2)) # minimal model

# Full and minimal model predictions
pred_prob_glm1 <- fitted(glm1)
pred_prob_glm2 <- fitted(glm2)

# Calibration plot of the minimal model
calibration_data <- data.frame(
  observed = train_scaled$result_num,
  predicted = pred_prob_glm1 # insert predicted probabilities of the model
) %>%
  mutate(bin = ntile(predicted, 10)) %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(predicted),
    mean_obs = mean(observed)
  )

ggplot(calibration_data, aes(x = mean_pred, y = mean_obs)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Mean predicted probability",
    y = "Observed proportion (Fail)",
    title = "Calibration plot"
  ) +
  theme_minimal(base_size = 16)

# ROC curve comparison between full and minimal model
# Both models have similar AUC (~0.67)
roc_glm1 <- roc(train_scaled$result_num, pred_prob_glm1)
roc_glm2 <- roc(train_scaled$result_num, pred_prob_glm2)

plot(roc_glm1, col = "blue", lwd = 2, main = "ROC Curve Comparison")
plot(roc_glm2, col = "red", lwd = 2, add = TRUE)
legend("bottomright",
       legend = c(
         paste0("Full model (AUC = ", round(auc(roc_glm1), 3), ")"),
         paste0("Minimal model (AUC = ", round(auc(roc_glm2), 3), ")")
       ),
       col = c("blue", "red"), lwd = 2)

# Confusion matrix
# True Positive (TP): predicted 1, actually 1
# False Negative (FN): predicted 0, actually 1
# False Positive (FP): predicted 1, actually 0
# True Negative (TN): predicted 0, actually 0
# Set a probability threshold to convert probabilities into predicted classes Fail = 1, Pass = 0
# Threshold 0.5
pred_class_glm1 <- ifelse(pred_prob_glm1 >= 0.5, 1, 0)
pred_class_glm2 <- ifelse(pred_prob_glm2 >= 0.5, 1, 0)

# Full model confusion matrix
# Remember: Inspection Fail = 1 and Inspection Pass = 0
confusionMatrix(
  factor(pred_class_glm1, levels = c(0,1)),
  factor(train_scaled$result_num, levels = c(0,1))
)

# Minimal model confusion matrix
# Remember: Inspection Fail = 1 and Inspection Pass = 0
# Both models have a high accuracy (~0.85) but a low balanced accuracy (~0.51)
confusionMatrix(
  factor(pred_class_glm2, levels = c(0,1)),
  factor(train_scaled$result_num, levels = c(0,1))
)

# Check whether "Pass" and "Fail" outcomes are balanced in the data
# Only ~16% of the data are failed inspections
sum(train_scaled$result == 'Fail') / sum(train_scaled$result == 'Pass')
sum(test_scaled$result == 'Fail') / sum(test_scaled$result == 'Pass')

# At what predicted probability should a farm be considered to fail an inspection?
# Find the optimal threshold of predicted probability at which to consider an inspection to fail
# Because the model was trained on unbalanced data, already a predicted probability of 0.14
# should be classified as a high risk of failing the inspection.
roc_obj <- roc(train_scaled$result, predict(glm2, newdata = train_scaled, type = "response"))
coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Check the confusion matrix with the new threshold
pred_class_glm2 <- ifelse(pred_prob_glm2 >= 0.1382668, 1, 0)

# Accuracy dropped to 0.67, balanced accuracy increased to 0.63
confusionMatrix(
  factor(pred_class_glm2, levels = c(0,1)),
  factor(train_scaled$result_num, levels = c(0,1))
)

# Odds-ratios of individual variables to the predicted outcome
summary(glm2)
exp(glm2$coefficients)

# Compute odds ratios of EMMs for categorical variables with binary outcomes
# E.g. farms that previously failed have about 1.9 times the odds (1/0.53) of failing again
# compared to farms that previously passed.

# List of the binary categorical variables in the model
cat_vars <- c("type", "previous", "proofMet", "organic")

# Create a list to store the summary tables
summary_list <- list()

for (v in cat_vars) {
  # compute estimated marginal means
  emm <- emmeans(glm2, as.formula(paste0("~ ", v)), type = "response")

  # pairwise contrasts
  contr <- contrast(emm, method = "pairwise")

  # summary with confidence intervals and p-values
  summary_list[[v]] <- summary(contr, infer = TRUE, type = "response")
}

# Example: view summary for "type"
summary_list[["type"]]

cat_var_or_summary <- bind_rows(summary_list, .id = "variable")





# =========================================================================================================
# Predict probabilities to fail an inspection using the built GLM on test data
# ---------------------------------------------------------------------------------------------------------
# =========================================================================================================

glm_final1 <- glm(result ~ type + previous + canton + GVE + proofMet + organic + trees +
                  oilseeds + artificialGrassland + cattle + sheep,
                  data = test_scaled, # use scaled test data
                  family = binomial, # for binary response variables
                  na.action = na.fail
)

# ROC curve and AUC
# convert "Fail" into 1 and "Pass" into 0.
roc_response <- ifelse(test_scaled$result == "Fail", 1, 0)
pred_prob_glm_final1 <- fitted(glm_final1)
roc_glm_final1 <- roc(roc_response, pred_prob_glm_final1)

plot(roc_glm_final1, col = "blue", lwd = 2, main = "ROC Curve")
legend("bottomright",
       legend = c(
         paste0("Test data (AUC = ", round(auc(roc_glm_final1), 3), ")")
       ),
       col = c("blue"), lwd = 2)

# Confusion matrix
pred_class_glm_final1 <- ifelse(pred_prob_glm_final1 >= 0.1456292, 1, 0)

# Accuracy dropped to 0.67, balanced accuracy increased to 0.63
confusionMatrix(
  factor(pred_class_glm_final1, levels = c(0,1)),
  factor(roc_response, levels = c(0,1))
)





# =========================================================================================================
# Introductionary plots to linear and logistic regression
# ---------------------------------------------------------------------------------------------------------
#
# =========================================================================================================

# Easy example of linear regression
df <- data.frame(
  x = c(1, 2, 3),
  y = c(2, 3, 5)
)

lm0 <- lm(y ~ x, data = df)
r2 <- summary(lm0)$r.squared
summary(lm0)

# simple dot plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 10) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# plot regression line and R²
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 10) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text",
           x = 2.3, y = Inf,
           label = bquote(R^2 == 1 - frac(sum((y - hat(y))^2),
                                          sum((y - bar(y))^2)) ~ " = " ~ .(round(r2, 3))),
           hjust = 1.1, vjust = 2,
           size = 10) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# Simple linear regression of LN and GVE on all data
lm1 <- lm(LN ~ GVE, data = data)
r2 <- summary(lm1)$r.squared

# simple dot plot
ggplot(data, aes(x = GVE, y = LN)) +
  geom_point() +
  labs(x = "GVE", y = "LN [ha]") +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# plot regression line and R² # 1000 x 700
ggplot(data, aes(x = GVE, y = LN)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(x = "GVE", y = "LN [ha]") +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("R² = ", round(r2, 3)),
           hjust = 1.1, vjust = 2,
           size = 10) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# Simple linear regression of LN and GVE on data subset "Tierwohl"
lm2 <- lm(LN ~ GVE, data = dat1$train)
r2 <- summary(lm2)$r.squared

# simple dot plot
ggplot(dat1$train, aes(x = GVE, y = LN)) +
  geom_point() +
  labs(x = "GVE", y = "LN [ha]") +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# plot regression line and R² # 1000 x 700
ggplot(dat1$train, aes(x = GVE, y = LN)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(x = "GVE", y = "LN [ha]") +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("R² = ", round(r2, 3)),
           hjust = 1.1, vjust = 2,
           size = 10) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# Easy example of logistig regression
# Simulate a tiny dataset (5 observations)
df <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(0, 0, 0, 1, 1)   # binary response
)

# Fit logistic regression model
glm0 <- glm(y ~ x, data = df, family = binomial)

# Create a smooth prediction curve for plotting
x_seq <- data.frame(x = seq(1, 5, length.out = 100))
x_seq$predicted_prob <- predict(glm0, newdata = x_seq, type = "response")

# Plot data and fitted logistic curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 5) +
  geom_line(data = x_seq, aes(y = predicted_prob), color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    x = "Explanatory Variable (x)",
    y = "Predicted Probability"
  ) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))

# Simple logistic regression of "result" (outcome of inspection) and LN
df <- data %>%
  mutate(result_num = ifelse(result == "Pass", 1, 0)) %>%
  dplyr::select(result_num, LN)

# number of pass and fails in df
sum(df$result_num == 1)   # counts how many 1s
sum(df$result_num == 0)   # counts how many 0s

sum(df$result_num == 0) / (sum(df$result_num == 1) + sum(df$result_num == 0))

glm01 <- glm(result_num ~ LN, data = df, family = binomial)

# Create a smooth prediction curve for plotting
df$predicted_prob <- predict(glm01, newdata = df, type = "response")

# plot
ggplot(df, aes(x = LN, y = result_num)) +
  geom_jitter(height = 0.05, width = 0, size = 2, alpha = 0.7) +
  geom_line(data = df, aes(x = LN, y = predicted_prob), color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    y = "result"
  ) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))


# =========================================================================================================
# Plots
# ---------------------------------------------------------------------------------------------------------
# Compute and plot estimated marinal means (EMMs) of probabilities for a variable of your choice.
# The built models have the response variable "result", which has the two levels "Pass" and "Fail".
# They model the probabilities to fail an inspection, because "Pass" is the reference of the
# response variable "result".
# The EMMs estimate the marginal (average) probability of the outcome (i.e. probability of failing an inspection)
# for each variable (e.g. "canton") while adjusting for the other variables in your model
# by holding them at their mean or reference values. It thus provides an effect estimate of a variable
# free from confounding due to the distribution of other variables, like GVE, organic, trees etc.
# =========================================================================================================

# EMMs of binary categorical variables
# Categorical variables
cat_vars <- c("type", "previous", "proofMet", "organic")

# Create a list to store the summary tables
summary_list <- list()

for (v in cat_vars) {
  # compute estimated marginal means
  emm <- emmeans(glm2, as.formula(paste0("~ ", v)), type = "response")

  # summary with confidence intervals and p-values
  summary_list[[v]] <- summary(emm, infer = TRUE, type = "response")
}

# Example: view summary for "type"
summary_list[["type"]]

cat_var_or_summary <- bind_rows(summary_list, .id = "variable")


plot_dat <- cat_var_or_summary %>%
  mutate(
    level = coalesce(type, previous, proofMet, organic)  # take whichever column has the value
  )


  select(variable, type, prob, SE, asymp.LCL, asymp.UCL)


#####
cat_vars <- c("type", "previous", "proofMet", "organic")

summary_list <- list()

for (v in cat_vars) {
  emm <- emmeans(glm2, as.formula(paste0("~ ", v)), type = "response")

  # Convert to data frame
  df <- as.data.frame(emm)

  # Rename the column of the factor variable dynamically
  df <- df %>%
    rename(level = !!sym(v)) %>%
    mutate(variable = v) %>%   # add the variable name
    dplyr::select(variable, level, response, SE, df, asymp.LCL, asymp.UCL) %>%
    rename(prob = response)    # rename response to prob for clarity

  summary_list[[v]] <- df
}

cat_var_or_summary <- bind_rows(summary_list)

cat_var_or_summary



# Variable "type" (was the inspection announced, or unannounced?)
emm_dat1 <- as.data.frame(emmeans(glm2, ~ type, type = "response"))

p1 <- emm_dat1 %>%
  ggplot( aes(x = type, y = prob)) +
  geom_bar(stat = "identity", color = "black", fill = "#77DDAA", width = 0.8) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, size = 1.0) +
  labs(x = "Type", y = "Predicted probability of Fail") +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))
p1

# Variable "previous" (outcome of the previous inspection)
emm_dat1 <- as.data.frame(emmeans(glm2, ~ previous, type = "response"))

p1 <- emm_dat1 %>%
  ggplot( aes(x = previous, y = prob)) +
  geom_bar(stat = "identity", color = "black", fill = "#77DDAA", width = 0.8) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, size = 1.0) +
  labs(x = "Vorherige Kontrolle", y = "Predicted probability of Fail") +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))
p1

# Variable "canton"
emm_dat1 <- as.data.frame(emmeans(glm2, ~ canton, type = "response"))

p1 <- emm_dat1 %>%
  ggplot( aes(x = canton, y = prob)) +
  geom_bar(stat = "identity", color = "black", fill = "#77DDAA", width = 0.8) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, size = 1.0) +
  labs(x = "Kanton", y = "Predicted probability of Fail") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))
p1

# Variable "proofMet" (Ökologischer Leistungsnachweis erfüllt, oder nicht erfüllt)
emm_dat1 <- as.data.frame(emmeans(glm2, ~ proofMet, type = "response"))

p1 <- emm_dat1 %>%
  ggplot( aes(x = proofMet, y = prob)) +
  geom_bar(stat = "identity", color = "black", fill = "#77DDAA", width = 0.8) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, size = 1.0) +
  labs(x = "ÖLN erfüllt", y = "Predicted probability of Fail") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))
p1

# Variable "organic" (handelt es sich um einen Bio Betrieb?)
emm_dat1 <- as.data.frame(emmeans(glm2, ~ organic, type = "response"))

p1 <- emm_dat1 %>%
  ggplot( aes(x = organic, y = prob)) +
  geom_bar(stat = "identity", color = "black", fill = "#77DDAA", width = 0.8) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, size = 1.0) +
  labs(x = "Bio Betrieb", y = "Predicted probability of Fail") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1))
p1































