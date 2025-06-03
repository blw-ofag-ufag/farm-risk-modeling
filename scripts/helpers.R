# =========================================================================================================
#       title:  HELPER FUNCTIONS FOR THE FARM RISK MODELING TASK
#      author:  Damian Oswald (damian.oswald@blw.admin.ch)
#        date:  2025-06-02
# description:  This script contains various helper functions used by other scripts.
# =========================================================================================================


# =========================================================================================================
# DROP FACTOR VARIABLES WHERA ALL FACTORS ARE THE SAME
# ---------------------------------------------------------------------------------------------------------
# This function changes a data frame in place and drops all factor variables with only one unique element.
# =========================================================================================================

drop_unique_factors <- function(data)
{
  x <- apply(data, 2, function(x) length(unique(x)))
  data[, names(x)[x >= 2], drop = FALSE]
}


# =========================================================================================================
# TRAIN-TEST DATA PARTITIONING BASED WHILE CONSIDERING GROUPS AND PANEL DATA CHARACTER
# ---------------------------------------------------------------------------------------------------------
# This function filters and cleans the input data for a given theme (removing missing values and
# uninformative columns), then splits the remaining rows by farm into training and testing sets (optionally
# stratifying by one or more grouping variables), finally returning the predictor columns (without the farm
# ID) in two separate data frames.
# =========================================================================================================

data_split <- function(data, theme, predictors, training_size = 0.8)
{
  # Subset the data by theme, and select "farm", predictors, and any groups
  df <- data[
    i    = data[["theme"]] == theme,
    j    = base::unique(c("farm", predictors, groups)),
    drop = FALSE
    ] %>%
    
    # Drop any rows with missing data
    stats::na.omit() %>%
  
    # Drop any columns that have fewer than 2 unique values (farm will usually have >1, so it stays).
    drop_unique_factors()
  
  # select only unique farms (one farm should belong to test or train, not both)
  x <- base::unique(df$farm)
  
  # sample some train farms
  train_farms <- base::sample(x, training_size * base::length(x))
  
  # save indices of train farms in the `df` data frame
  i <- df[["farm"]] %in% x[x %in% train_farms]
  
  # return two data sets, one for train, one for test, one original and the indices
  list(
    train = df[ i, , drop = FALSE],
    test  = df[!i, , drop = FALSE],
    original = df,
    indices = i
  )
}
