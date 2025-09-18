#' =============================================================================
#'       title:  AGGREGATION OF RANDOM FOREST PERFORMANCE RESULTS
#'      author:  Damian Oswald
#'        date:  2025-09-05
#' =============================================================================

#' libraries
library(magrittr)
library(knitr)

#' Read helper functions
source("scripts/helpers.R")

#' Read metadata
meta <- yaml::read_yaml("resources/schema.yaml")

#' Get theme names
themes <- meta$fields[[6]]$constraints$enum

#' Function to read all confusion matrix objects
read_confusion_matrix <- function(theme) {
  M <- readRDS(file.path("results", theme, "confusion-matrix.rds"))
  c(M$overall[1], M$byClass)
}

sapply(themes, read_confusion_matrix) %>%
  t() %>%
  knitr::kable(digits = 3)