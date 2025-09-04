#' =============================================================================
#'       title:  MODELLING FARM INSPECTION OUTCOMES USING RANDOM FORESTS
#'      author:  Damian Oswald
#'        date:  2025-09-04
#' description:  This script builds a one random forest model for each theme in
#'               the separately prepared dataset to predict farm inspection
#'               results.
#' =============================================================================


#' =============================================================================
#' PREPARATIONS
#' =============================================================================

#' We set a seed for reproducibility.
set.seed(42)

#' Attaching and listing of necessary packages to the search path
library(magrittr)
library(randomForest)
library(caret)
library(ggplot2)
library(reshape2)

#' Read helper functions
source("scripts/helpers.R")

#' =============================================================================
#' PREPARE THE TRAINING AND TEST DATA
#' -----------------------------------------------------------------------------
#' Data split is performed by the `data_split` function, which returns a list
#' with four objects, `original` (the original data frame), `train` (the
#' training data), `test` (the test data) and `indices`
#' =============================================================================

#' Read full data and split it it into train/test
readRDS("data/data.rds") %>%

  data_split(
    theme = "Tierschutz",
    predictors = readRDS("resources/predictors.rds"),
    training_size = 0.8
  ) %>%

  base::list2env(envir = .GlobalEnv)

#' Read predictors
variables <- readRDS("resources/variables.rds")

#' Vector with predictors (remove the ones dropped by the split, this might be
#' the case because there is only one factor left in the training data)
predictors <- setdiff(variables[variables %in% colnames(train)], "result")

#' =============================================================================
#' TRAIN THE RANDOM FOREST MODEL
#' -----------------------------------------------------------------------------
#' A random forest model is trained on the prepared `train` dataset to predict
#' the inspection `result`.
#' =============================================================================

#' The summary shows some predictors have `Inf` values, which must be removed.
#' We'll replace them with NA and impute them in the next step.
train[sapply(train, is.infinite)] <- NA
test[sapply(test, is.infinite)] <- NA

#' Random forests cannot handle missing values (`NA`). We'll use a fast
#' imputation method, `na.roughfix`, which replaces numeric NAs with the
#' column median and factor NAs with the mode.
train <- randomForest::na.roughfix(train)
test <- randomForest::na.roughfix(test)

#' Create the model formula. The goal is to predict `result` using all other
#' variables defined in the `variables` vector.
formula <- as.formula(paste("result ~", paste(predictors, collapse = " + ")))

#' The target variable `result` is imbalanced (~ 85% "Pass" vs. 15% "Fail").
#' To correct for this, we'll use stratified sampling by setting the `sampsize`
#' argument. We'll sample from each class an equal number of times, equivalent
#' to the size of the minority class ("Fail").
n_fail <- sum(train$result == "Fail")
sample_sizes <- c(Pass = n_fail, Fail = n_fail)

#' Train the random forest model
model <- randomForest(
  formula = formula,
  data = train,
  ntree = 101, #' Number of trees (odd number to avoid ties)
  sampsize = sample_sizes, #' Balanced sample sizes
  importance = TRUE #' Calculate variable importance
)

#' =============================================================================
#' MODEL EVALUATION
#' -----------------------------------------------------------------------------
#' Evaluate the model's performance on the unseen test dataset using a
#' confusion matrix and related metrics.
#' =============================================================================

#' Generate predictions on the test set
predictions <- predict(model, newdata = test)

#' Generate the confusion matrix and a comprehensive set of statistics.
#' We set `positive = "Fail"` to get metrics like sensitivity and specificity
#' from the perspective of correctly identifying failed inspections.
confusionMatrix(
  data = predictions,
  reference = test$result,
  positive = "Fail"
)