#' =============================================================================
#'       title:  HELPER FUNCTIONS FOR THE FARM RISK MODELING TASK
#'      author:  Damian Oswald
#'        date:  2025-06-02
#' description:  This script contains various helper functions used by other
#'               scripts.
#' =============================================================================

library(magrittr)

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
#' @param data            The dataset to be split.
#' @param theme           The theme to be selected.
#' @param predictors      The predictos to be selected.
#' @param training_size   Proportion (from 0 to 1) of the data that ought to be
#'                        assigned to the training data set.
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

  #' Select only unique farms (one farm belongs to test or train, not both)
  x <- base::unique(df$farm)

  #' Sample some train farms
  train_farms <- base::sample(x, training_size * base::length(x))

  #' Save indices of train farms in the `df` data frame
  i <- df[["farm"]] %in% x[x %in% train_farms]

  #' Return two data sets, one train, one test, one original and indices
  list(
    train = df[i,  , drop = FALSE],
    test  = df[!i, , drop = FALSE],
    original = df,
    indices = i
  )
}


#' =============================================================================
#' GET OBJECT FROM FRICTIONLESS METADATA LIST BY KEY
#' =============================================================================

get_object <- function(meta, key) {
  meta$fields[[
    which(lapply(meta$fields, function(x) getElement(x, "name"))==key)
  ]]
}

#' =============================================================================
#' GET VARIABLE TITLES FROM FRICTIONLESS METADATA (VECTORIZED)
#' -----------------------------------------------------------------------------
#'
#' @param x             A character vector of field 'names' to get titles for.
#' @param metadata      An R list object read from a Frictionless YAML file.
#' @param lang          The desired language for the title (e.g., "en", "de").
#' @param unit          Should the unit be concatenated to the returned title?
#'
#' @return A character vector of constructed title strings. If a title cannot
#'         be constructed for given variable(s), the variable name itself is
#'         returned as a fallback.
#' =============================================================================

get_titles <- function(x, metadata, lang = "en", unit = TRUE) {

  #' For efficiency, create a named list for quick lookups. The names of the
  #' list are the variable names, and the values are the field objects.
  lookup <- setNames(
    metadata$fields,
    sapply(metadata$fields, `[[`, "name")
  )

  #' Iterate over each variable name provided.
  titles <- lapply(x, function(key) {

    #' Attempt to retrieve the entire field object for the current variable 'v'.
    field <- lookup[[key]]

    #' Fallback Condition 1: Variable not found
    #' (If the field doesn't exist in lookup, return the variable name itself.)
    if (is.null(field)) {
      return(key)
    }

    #' Attempt to retrieve the title for the specified language.
    title <- field$title[[lang]]

    #' Fallback Condition 2: Title in specified language not found
    #' If the title is NULL (e.g., lang 'fr' doesn't exist), return the key.
    if (is.null(title)) {
      return(key)
    }

    #' If the 'unit' flag is TRUE, append the unit if one exists.
    if (unit && !is.null(field$unit)) {
      title <- sprintf("%s [%s]", title, field$unit)
    }

    #' Return the successfully constructed title.
    title
  })

  #' convert the list back into a simple character vector.
  titles <- unlist(titles)

  #' name the vector using the provided keys
  names(titles) <- x

  #' Return the constructed titles
  titles
}


#' =============================================================================
#' LABEL AND TRANSLATE A DATASET USING THE FRICTIONLESS METADATA
#' -----------------------------------------------------------------------------
#'
#' @param data          The data set to be labelled/translated
#' @param metadata      An R list object read from a Frictionless YAML file.
#' @param lang          The desired language for the title (e.g., "en", "de").
#' @param unit          Should the unit be concatenated to the returned title?
#'
#' @return A dataset with replaced column names and TODO: replaced level names
#' =============================================================================

label_dataset <- function(data, metadata, lang = "de", unit = TRUE) {

  #' Replace column names
  colnames(data) <- get_titles(
    x = colnames(data),
    metadata = metadata,
    lang = "de",
    unit = unit
  )

  #' TODO: Also replace level names (requires labelled levels in metadata)

  #' Return labelled/translated data set
  data
}

#' =============================================================================
#' PRINT A TITLE
#' -----------------------------------------------------------------------------

print_title <- function(x) {
  cat("\n\n", toupper(x), ":\n", strrep("=", 1+nchar(x)), "\n\n", sep = "")
}
