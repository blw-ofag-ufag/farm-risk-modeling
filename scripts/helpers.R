#' =============================================================================
#'       title:  HELPER FUNCTIONS FOR THE FARM RISK MODELING TASK
#'      author:  Damian Oswald (damian.oswald@blw.admin.ch)
#'        date:  2025-06-02
#' description:  This script contains various helper functions used by other
#'               scripts.
#' =============================================================================


#' =============================================================================
#' DROP FACTOR VARIABLES WHERA ALL FACTORS ARE THE SAME
#' -----------------------------------------------------------------------------
#' This function changes a data frame in place and drops all factor variables
#' with only one unique element.
#' =============================================================================

drop_unique_factors <- function(data) {
  x <- apply(data, 2, function(x) length(unique(x)))
  data[, names(x)[x >= 2], drop = FALSE]
}


#' =============================================================================
#' TRAIN-TEST DATA PARTITIONING BASED WHILE CONSIDERING GROUPS
#' -----------------------------------------------------------------------------
#' This function filters and cleans the input data for a given theme (removing
#' missing values and uninformative columns), then splits the remaining rows by
#' farm into training and testing sets (optionally stratifying by one or more
#' grouping variables), finally returning the predictor columns (without the
#' farm ID) in two separate data frames.
#'
#' @param data           The dataset to be split.
#' @param theme          The theme to be selected.
#' @param predictors     The predictos to be selected.
#' @param training_size  Proportion (from 0 to 1) of the training data set.
#' =============================================================================

data_split <- function(data, theme, predictors, training_size = 0.8) {
  #' Subset the data by theme, and select "farm", predictors, and any groups
  df <- data[
    i    = data[["theme"]] == theme,
    j    = base::unique(c("farm", predictors)),
    drop = FALSE
  ] %>%

    #' Drop any rows with missing data
    stats::na.omit() %>%

    #' Drop any columns that have fewer than 2 unique values
    #' (farm will usually have >1, so it stays).
    drop_unique_factors()

  #' select only unique farms (one farm belongs to test or train, not both)
  x <- base::unique(df$farm)

  #' sample some train farms
  train_farms <- base::sample(x, training_size * base::length(x))

  #' save indices of train farms in the `df` data frame
  i <- df[["farm"]] %in% x[x %in% train_farms]

  #' return two data sets, one train, one test, one original and indices
  list(
    train = df[i,  , drop = FALSE],
    test  = df[!i, , drop = FALSE],
    original = df,
    indices = i
  )
}


#' =============================================================================
#' GET A VARIABLE'S TITLE FROM FRICTIONLESS METADATA
#' -----------------------------------------------------------------------------
#'
#' @param variable   The 'name' of the field you want the title for.
#' @param metadata   An R list object read from a Frictionless YAML file.
#' @param lang       The desired language for the title (e.g., "en", "de").
#' @param unit       Should the unit be concatenated to the returned title?
#'
#' @return The title string, or NULL if not found.
#' =============================================================================

get_title <- function(variable, metadata, lang = "en", unit = FALSE) {
  i <- which(lapply(metadata$fields, getElement, "name") == variable)
  if (length(i) == 0) {
    warning(sprintf("`%s` not found in the metadata\n", variable))
  }
  if (length(i) > 1) {
    warning(sprintf("`%s` found multiple times in the metadata\n", variable))
  }
  title <- metadata$fields[[i]]$title[[lang]]
  if (unit) {
    unit <- metadata$fields[[i]]$unit
    if (is.null(unit)) break
    title <- sprintf("%s [%s]", title, unit)
  }
  title
}
