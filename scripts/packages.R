#' =============================================================================
#'       title:  R PACKAGE INSTALLATION SCRIPT
#' description:  This script checks for the existence of required packages
#'               before installing them.
#' =============================================================================

#' Set required packages
packages <- base::c(
  "caret",
  "dplyr",
  "forcats",
  "ggplot2",
  "ggtext",
  "iml",
  "knitr",
  "magrittr",
  "openssl",
  "pROC",
  "PRROC",
  "randomForest",
  "randomForestExplainer",
  "readr",
  "showtext",
  "summarytools",
  "tibble",
  "tidyr",
  "yaml"
)

#' Loop through the list of CRAN packages
for (package in packages) {
  if (!base::requireNamespace(package, quietly = TRUE)) {
    utils::install.packages(package, repos = "https://cloud.r-project.org")
  }
}
