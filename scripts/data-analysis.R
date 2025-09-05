#' =============================================================================
#'       title:  ANALYZING THE INSPECTION DATA SET
#'      author:  Damian Oswald
#'        date:  2025-09-04
#' =============================================================================

#' =============================================================================
#' PREPARATIONS
#' =============================================================================

#' Attaching and listing of necessary packages to the search path
library(magrittr)
library(summarytools)

#' Read helper functions
source("scripts/helpers.R")

#' Read data from RDS object
df <- data <- readRDS("data/data.rds")

#' Read metadata from frictionless data table schema
meta <- yaml::read_yaml("resources/schema.yaml")

#' Translate labels in `df` just for fully automatic processing
colnames(df) <- get_titles(colnames(data), meta, "de")

#' create folder for overall results, if it doesn't already exist
if (!dir.exists("results/overall")) dir.create("results/overall")

#' =============================================================================
#' DATA ANALYIS
#' =============================================================================

#' Write results into some text file
sink("results/overall/data-analysis.txt")

#' Create a summary of the data using summarytools
summarytools::dfSummary(
  x = df,
  varnumbers = FALSE,
  valid.col = FALSE,
  display.labels = TRUE,
  silent = TRUE,
  style = "multiline",
)

#' Close writing file
sink()
