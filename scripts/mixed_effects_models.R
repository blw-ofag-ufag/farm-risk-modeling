# =========================================================================================================
#       title:  CONTRIBUTION OF FARM ATTRIBUTES TO INSPECTION OUTCOMES
#      author:  Marc Beringer (marc.beringer@blw.admin.ch)
#        date:  2025-09-02
# description:  Uses carefully curated data from the FOAG production systems AGIS (structural attributes),
#               ACONTROL (on-farm inspections) and HODUFLU (manure and recycling-fertilizer flows),
#               assembled by Damian Oswald (damian.oswald@blw.admin.ch). Here this data frame serves as
#               the input for statistical approaches that predict inspection pass/fail outcomes and
#               analyses the contribution of attributes of Swiss farms. All steps are reproducible,
#               although access to the data is strictly limited to FOAG collaborators.
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
  library(MuMIn) # allows for approximation of R^2 values in mixed effect models
  library(glmnet) # enables Lasso and Ridge regression
  library(summarytools)
  library(pscl) # McFadden's pseudo R² for GLMs
  library(performance) # Tjur's R² for GLMs
  library(pROC) # enables computation of AUC
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

# Clean and split data into a training data set and a test data set
dat1 <- data %>%
  data_split( "Tierschutz", predictors, training_size = 0.8)

# Check if farms in the training data are present in the test data (should return an empty data frame)
dat1$train[dat1$train$farm %in% dat1$test$farm, ]


# =========================================================================================================
# ASSESS COLLINEARITY
# ---------------------------------------------------------------------------------------------------------
# Collinearity occurs when two or more predictor variables are highly correlated,
# which impairs the precision in estimating each variable's contribution to the model’s predicted outcome.
# To accurately assess the influence of individual farm attributes, we need to evaluate collinearity.
# =========================================================================================================



# =========================================================================================================
# SCALE DATA AND CONDUCT VARIABLE SELECTION BY LASSO REGRESSION
# ---------------------------------------------------------------------------------------------------------
# Scale and center the continuous variables in the data. If the variables in the data are on vastly
# different scales (like LN and hofNtot), we need to scale and center them in order for subsequent models
# to work. Scaling sets the mean of every variable to 0 and the standard deviation (SD) to 1.
# Scaling has to be done on the training data and the test data. However, the test data has to be scaled
# with mean and SD of the training data, since in the real world, we would not have test data avialable.
# Therefore, keep track of the mean and SD of each continuous variable of the training data.
#
# Lasso Regression is a linear modeling technique that adds a penalty equal to the absolute values of the
# coefficients, it shrinks some of them to zero. Variables with a coefficient close to zero contribute
# little to the change of the response variable (here inspection result pass, or fail). If two or more
# variables are highly correlated with each other, all but one of them will be reduced to zero. Therefore,
# variables with a coefficient of zero, or close to zero, are not required to increase the explained
# variance of the model in the data. This doesn't mean the variables with coefficients = 0 are not
# important, but they're not important if the variables with larger coefficients are included in the model.
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


# For LASSO regression, create a matrix of predictor variables (x),
# this must be a numeric matrix to fit the cv.glmnet() function.
# Remove the response variable (here "result").
# Remove factor variables with a lot of levels (here "farm") so the numeric matrix will not become too large.
train_scaled <- train_scaled %>%
  dplyr::select(-farm, -result)

# model.matrix() expands factors into dummy variables (one column per factor level).
# Remove the first column (Intercept), which we don't need.
x <- model.matrix(~., data = train_scaled)[, -1]

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

# Visualize Lasso model coefficients
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

# Map dummy variable names back to original variable names
matched_vars <- sapply(coef_dat_nz$Predictor, function(x) {
  predictors[startsWith(x, predictors)]
})

# Keep only unique original variables
# According to LASSO regression, these are the variables we select for our generalized linear mixed-effects model.
matched_vars <- unique(unlist(matched_vars))
matched_vars


# =========================================================================================================
# Generalized linear mixed-effects model (GLMM)
# ---------------------------------------------------------------------------------------------------------
# Use the test data (test_dat) in a generalized linear mixed-effects model, with the variables selected
# using LASSO regression. Since variables were selected using the training data (train_dat), we prevent
# overfitting the model to the data. This way we can test the actual contribution of selected variables
# to the inspection result.
#
# =========================================================================================================

# Check whether both outcomes of the "result"-variable are well represented
sum(test_scaled$result == 'Pass')
sum(test_scaled$result == 'Fail')

# Run the model with selected variables
# Use a GLMM if you want to include random effects
glmm1 <- lme4::glmer(result ~ type + previous + SAK + GVE + DZ + proofMet + organic + milk + trees +
                     oilseeds + artificialGrassland + cattle + sheep
                     + (1 | canton), # canton as random effect
                     data = test_scaled, # use scaled test data
                     family = binomial, # for binary response variables
                     na.action = na.fail
                     )

summary(glmm1)
ranef(glmm1) # random effects
AIC(glmm1, glm1) #compare AIC of the GLMM and the GLM
BIC(glmm1, glm1) #compare AIC of the GLMM and the GLM
pred_probs <- predict(glmm1, type = "response")
emmeans(glmm1, ~ type)

# Compute AUC
glmm_pred <- predict(glmm1, type = "response")
roc_obj_glmm <- roc(test_scaled$result, glmm_pred)
auc(roc_obj_glmm)

# Marginal and Conditional R² of mixed effect models
# Theoretical and delta are two approaches to estimating R-squared in GLMMs;
# delta method is usually more robust for GLMMs with non-Gaussian error structures.
r.squaredGLMM(glmm1)

# Check whether the model can be simplified by dropping one of the fixed effects
# GVE, DZ, milk, trees, artificialGrassland, cattle, sheep
drop1(glmm1, test = "Chisq")

# Create a model with the minimum amount of variables
glmm2 <- lme4::glmer(result ~ type + previous + SAK + proofMet + organic +
                     oilseeds
                     + (1 | canton), # canton as random effect
                     data = test_scaled, # use scaled test data
                     family = binomial, # for binary response variables
                     na.action = na.fail
)

summary(glmm2)

# Compute AUC
glmm_pred <- predict(glmm2, type = "response")
roc_obj_glmm <- roc(test_scaled$result, glmm_pred)
auc(roc_obj_glmm)

r.squaredGLMM(glmm2)

# =========================================================================================================
# Generalized linear model (GLM)
# ---------------------------------------------------------------------------------------------------------
#
# =========================================================================================================

# Use a Generalized Linear Model (GLM) if you don't want to include random effects
glm1 <- glm(result ~ type + previous + canton + SAK + GVE + DZ + proofMet + organic + milk + trees +
                          oilseeds + artificialGrassland + cattle + sheep,
                          data = test_scaled, # use scaled test data
                          family = binomial, # for binary response variables
                          na.action = na.fail
                          )
# Explained variation
pscl::pR2(glm1) # McFaddens pseudo-R²
performance::r2_tjur(glm1) # McFaddens pseudo-R²

# Compute AUC
# Predicted probabilities
glm_pred <- predict(glm1, type = "response")

# True outcome (must be 0/1 or two-level factor)
y <- test_scaled$result

# ROC and AUC
roc_obj <- roc(y, glm_pred)
auc_val <- auc(roc_obj)

print(auc_val)
plot(roc_obj, col = "blue", main = "ROC Curve for GLM")

# Check minimal GLM
glm2 <- glm(result ~ type + previous + canton + GVE + DZ + proofMet + organic + trees +
              oilseeds + artificialGrassland + sheep,
            data = test_scaled, # use scaled test data
            family = binomial, # for binary response variables
            na.action = na.fail
)

summary(glm2)
exp(coef(glm2))

# Explained variation
pscl::pR2(glm2) # McFaddens pseudo-R²
performance::r2_tjur(glm2) # McFaddens pseudo-R²

# Compute AUC
# Predicted probabilities
glm_pred <- predict(glm2, type = "response")

# True outcome (must be 0/1 or two-level factor)
y <- test_scaled$result

# ROC and AUC
roc_obj <- roc(y, glm_pred)
auc_val <- auc(roc_obj)

print(auc_val)
plot(roc_obj, col = "blue", main = "ROC Curve for GLM")






### ========================== data formatting ========================== ###
#factorize variables
data <- pr_extended %>%

  as_tibble() %>%

  dplyr::mutate(individual = factor(individual),
                family = factor(family),
                reciprocal_family = factor(reciprocal_family),
                taxon = factor(taxon),
                origin = factor(origin),
                ploidy = factor(ploidy),
                rte_location = factor(rte_location),
                rte_elevation.m. = factor(rte_elevation.m.),
                phenotyping_run = factor(phenotyping_run),
                type = factor(type),
                type_date = case_when(year == "2020" & type == "early season" ~ "early season 2020",
                                      year == "2020" & type == "full bloom" ~ "full bloom 2020",
                                      year == "2020" & type == "end season" ~ "end season 2020",
                                      year == "2021" & type == "early season" ~ "early season 2021",
                                      year == "2021" & type == "full bloom" ~ "full bloom 2021",
                                      year == "2021" & type == "end season" ~ "end season 2021"),
                year = factor(year)) %>%

  #and relevel factors rte_location (the reference will be ostermundigen),
  #rte_elevation (the reference will be 585m a.s.l., which is also ostermundigen)
  #and origin (the reference will be taxa originating in lowland populations)

  dplyr::mutate(taxon = factor(taxon, levels = c("A2H", "A2L", "A2LH", "L4H", "L4L", "L4LH", "V2L"))) %>%
  dplyr::mutate(rte_location = factor(rte_location, levels = c("ostermundigen", "pont de nant", "schynige platte"))) %>%
  dplyr::mutate(rte_elevation.m. = factor(rte_elevation.m., levels = c("585", "1236", "1967"))) %>%
  dplyr::mutate(origin = factor(origin, levels = c("lowland", "alpine", "hybrid"))) %>%
  dplyr::mutate(phenotyping_run = factor(phenotyping_run, levels = c("pr1", "pr2", "pr3", "pr4", "pr5", "pr6", "pr7", "pr8"))) %>%
  dplyr::mutate(type = factor(type, levels = c("transplant check1", "transplant check2", "early season", "full bloom", "end season"))) %>%
  dplyr::mutate(type_date = factor(type_date, levels = c("early season 2020", "full bloom 2020", "end season 2020",
                                                         "early season 2021", "full bloom 2021", "end season 2021"))) %>%
  dplyr::mutate(year = factor(year, levels = c("2019", "2020", "2021")))


#check structure
str(data)
data
#=#

### === transform rosette area from mm^2 to cm^2
data$rosette_size_cm2 <- data$rosette_size_mm2/100

### === Subset data of each elevation level
data_omu <- subset(data, data$rte_location == "ostermundigen")
data_pdn <- subset(data, data$rte_location == "pont de nant")
data_schypla <- subset(data, data$rte_location == "schynige platte")

### ===== ###

### ========================= linear mixed models ================================= ###
### LEAF NUMBER ###
names(data)

#full model
full_model <- lmer(n_leaves ~ ploidy + origin + phenotyping_run +
                     ploidy:origin + ploidy:phenotyping_run + origin:phenotyping_run +
                     ploidy:origin:phenotyping_run +
                     (1 | taxon:reciprocal_family), data = data_pdn, REML = T)

anova(full_model)

#check whether the model can be simplified by dropping one of the fixed effects
drop1(full_model, test="Chisq")

#CORRELATION BETWEEN FITTED AND OBSERVED VALUES (AKIN R^2)
r.squaredGLMM(full_model)

#write model results table
write.table(anova(full_model), file = "./Model_results_tables/leaf_number_pr_OMU_anova_table.txt", row.names = T, col.names = T, quote = F, sep = "\t")
write.table(anova(full_model), file = "./Model_results_tables/leaf_number_pr_PDN_anova_table.txt", row.names = T, col.names = T, quote = F, sep = "\t")
write.table(anova(full_model), file = "./Model_results_tables/leaf_number_pr_SCHYPLA_anova_table.txt", row.names = T, col.names = T, quote = F, sep = "\t")

### ===== ###

### ========================= interaction plots ======================== ###
ploidy_col <- c("#343148FF", "#D7C49EFF")

### #2000x500
data_emms <- as.data.frame(emmeans(full_model, c("ploidy", "phenotyping_run", "origin")))

p3 <- data_emms %>%
  ggplot( aes(x = phenotyping_run, y = emmean, group = ploidy, color = ploidy)) +
  geom_line(aes(linetype = ploidy), size = 1.5) +
  geom_point(size = 7.5) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width = 0.2, size = 1.5) +
  labs(x = NULL, y = "number of leaves") +
  theme_minimal() +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 1)) +
  facet_wrap(~origin)

p3 + scale_color_manual(values = ploidy_col)

#Assemble leaf number end season 2020 and 2021 in one figure with same y-axis scale #1100x600
p1 + ylim(-10, 180)  + scale_color_manual(values = ploidy_col)
p2 + ylim(-10, 180)  + scale_color_manual(values = ploidy_col)
p3 + ylim(-10, 180)  + scale_color_manual(values = ploidy_col)

#=#

































