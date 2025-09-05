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

#' Get command line arguments
theme <- commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments were provided
if (length(theme) != 1) {
  stop("Incorrect number of arguments.", call. = FALSE)
}

#' We set a seed for reproducibility.
set.seed(42)

#' Attaching and listing of necessary packages to the search path
library(caret) # classification and regression training
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(iml) # interpretable machine learning
library(knitr)
library(magrittr)
library(randomForest)
library(randomForestExplainer)
library(showtext)
library(summarytools)
library(tibble)
library(tidyr)
library(yaml) # for reading the metadata

#' Read helper functions
source("scripts/helpers.R")

#' =============================================================================
#' PREPARE THE TRAINING AND TEST DATA
#' -----------------------------------------------------------------------------
#' Data split is performed by the `data_split` function, which returns a list
#' with four objects, `original` (the original data frame), `train` (the
#' training data), `test` (the test data) and `indices`
#' =============================================================================

#' Read metadata
meta <- yaml::read_yaml("resources/schema.yaml")

#' Read full data and split it it into train/test
data <- readRDS("data/data.rds") %>%

  data_split(
    theme = theme,
    predictors = readRDS("resources/predictors.rds"),
    training_size = 0.8
  )

#' Read predictors
variables <- readRDS("resources/variables.rds")

#' Open a "log" file to write the results to
results_directory <- file.path("results", theme)
if (!dir.exists(results_directory)) dir.create(results_directory)
sink(file.path("results", theme, "results.txt"))

#' Make a summary of the specific subset of the data used here (subset by theme)
df <- label_dataset(data$original, metadata = meta, lang = "de", unit = TRUE)
summarytools::dfSummary(
  x = df,
  varnumbers = FALSE,
  valid.col = FALSE,
  display.labels = TRUE,
  silent = TRUE,
  style = "multiline",
)

#' Vector with predictors (remove the ones dropped by the split, this might be
#' the case because there is only one factor left in the training data)
predictors <- setdiff(variables[variables %in% colnames(data$train)], "result")

#' List the variables that are *not* used for modelling due to incompleteness
unused_variables <- variables[!(variables %in% predictors)][-1]
cat(sprintf("
Variables not used for modeling:
================================
%s
", paste(get_titles(unused_variables, meta, "de", FALSE), collapse = "\n")))

#' Create the model formula. The goal is to predict `result` using all other
#' variables defined in the `variables` vector.
formula <- as.formula(paste("result ~", paste(predictors, collapse = " + ")))
cat("
FORMULA:
========
")
print(formula)

#' =============================================================================
#' TRAIN THE RANDOM FOREST MODEL
#' -----------------------------------------------------------------------------
#' A random forest model is trained on the prepared `train` dataset to predict
#' the inspection `result`.
#' =============================================================================

#' The summary shows some predictors have `Inf` values, which must be removed.
#' We'll replace them with NA and impute them in the next step.
data$train[sapply(data$train, is.infinite)] <- NA
data$test[sapply(data$test, is.infinite)] <- NA

#' Random forests cannot handle missing values (`NA`). We'll use a fast
#' imputation method, `na.roughfix`, which replaces numeric NAs with the
#' column median and factor NAs with the mode.
data$train <- randomForest::na.roughfix(data$train)
data$test <- randomForest::na.roughfix(data$test)

#' The target variable `result` is almost always imbalanced, as there are many
#' more farms that completely pass an inspection than fail anything. To correct
#' for this, we'll use stratified sampling by setting the `sampsize` argument.
#' We'll sample from each class an equal number of times, equivalent to the size
#' of the minority class (`"Fail"`).
n_fail <- sum(data$train$result == "Fail")
sample_sizes <- c(Pass = n_fail, Fail = n_fail)

#' Train the random forest model
model <- randomForest(
  formula = formula,
  data = data$train,
  ntree = 500,
  sampsize = sample_sizes,
  importance = TRUE
)

#' =============================================================================
#' MODEL PERFORMANCE EVALUATION
#' -----------------------------------------------------------------------------
#' Evaluate the model's performance on the unseen test dataset using a confusion
#' matrix and related metrics.
#' =============================================================================

#' Generate predictions on the test set
predictions <- predict(model, newdata = data$test)

#' Generate the confusion matrix and a comprehensive set of statistics. We set
#' `positive = "Fail"` to get metrics like sensitivity and specificity from the
#' perspective of correctly identifying failed inspections.
confusion_matrix <- confusionMatrix(
  data = predictions,
  reference = data$test$result,
  positive = "Fail"
)

#' Save confusion matrix with test scores
saveRDS(confusion_matrix, file.path("results", theme, "confusion-matrix.rds"))

#' Print results to text summary
print(formula)
print(model)
print(confusion_matrix)

#' =============================================================================
#' POST-HOC MODEL ANALYSIS
#' -----------------------------------------------------------------------------
#' In this step, we'll analyze not the model performance, but the model itself
#' to gain insights into patterns behind the data.
#' =============================================================================

#' Access the importance scores
importance_scores <- importance(model)

#' Sort the scores for easier interpretation
#' We'll sort by Mean Decrease in Accuracy (column 4 for classification)
sorted_importance_scores <- importance_scores[
  order(importance_scores[, "MeanDecreaseAccuracy"], decreasing = TRUE),
] %>% as.data.frame()

#' Add labels that are easier to read for humans
rownames(sorted_importance_scores) <- get_titles(
  x = rownames(sorted_importance_scores),
  metadata = meta,
  lang = "de",
  unit = FALSE
)

#' Print the sorted scores to the console
print(knitr::kable(sorted_importance_scores, digits = 3))

#' TODO: Detect interaction effects using `iml` functions...

#' Close file with results again
sink()

#' =============================================================================
#' PLOT THE CONFUSION MATRIX
#' -----------------------------------------------------------------------------
#' Create a visual representation of the model's performance on the test set
#' using the previously generated confusion matrix.
#' =============================================================================

#' Prepare the confusion matrix data for ggplot
confusion_matrix_df <- as.data.frame(confusion_matrix$table) %>%
  mutate(
    Percentage = Freq / sum(Freq) * 100,
    Label = paste0(Freq, "\n(", round(Percentage, 1), "%)")
  )

#' Create the plot
plot_cm <- ggplot(
  confusion_matrix_df,
  aes(x = Prediction, y = Reference, fill = Freq)
) +
  geom_tile(color = "white", lwd = 1.5) +
  geom_text(
    aes(label = Label),
    color = ifelse(confusion_matrix_df$Percentage > 40, "white", "black"),
    size = 5
  ) +
  scale_fill_gradient(low = "#EBF5F4", high = "#2A9D8F") +
  scale_y_discrete(limits = rev) +
  coord_fixed() +
  labs(
    title = paste("Fehlermatrix für den Themenbereich", theme),
    subtitle = "Modellperformance berechnet auf dem ungesehenen Test-Datensatz",
    x = "Vorhergesagte Klasse",
    y = "Tatsächliche Klasse"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.3)),
    plot.subtitle = element_text(margin = ggplot2::margin(b = 15)),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = rel(1)),
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )

#' Save the confusion matrix plot
ggsave(
  file.path("results", theme, "confusion_matrix.png"),
  plot = plot_cm,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300
)

#' =============================================================================
#' PLOT THE IMPORANCE SCORES
#' -----------------------------------------------------------------------------
#' In this step, we'll analyze not the model performance, but the model itself
#' to gain insights into patterns behind the data.
#' =============================================================================

#' Prepare data for visualization
importance_df_tidy <- importance_scores %>%
  as.data.frame() %>%
  select(Fail, Pass, MeanDecreaseAccuracy) %>%
  rownames_to_column(var = "variable_name") %>%
  mutate(Variable = get_titles(
    x = variable_name,
    metadata = meta,
    lang = "de",
    unit = FALSE
  )) %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  slice_head(n = 15) %>%
  pivot_longer(
    cols = c(Fail, Pass),
    names_to = "Klasse",
    values_to = "Importance"
  ) %>%
  mutate(
    Klasse = recode(
      Klasse,
      "Fail" = "Mängel",
      "Pass" = "Konform"
    )
  ) %>%
  mutate(Variable = forcats::fct_reorder(
    Variable,
    Importance,
    .fun = max,
    .desc = TRUE
  ))

horizontal_plot <- ggplot(
  importance_df_tidy,
  aes(x = Variable, y = Importance, fill = Klasse)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c(
    "Mängel" = "#E76F51",
    "Konform" = "#2A9D8F"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = paste(
      "Schlüsselfaktoren für Ergebnisse von Betriebskontrollen im Bereich",
      theme
    ),
    subtitle = paste0(
      "Relative Wichtigkeit für die Vorhersage von **",
      "<span style='color:#E76F51;'>Mängel</span>** vs. **",
      "<span style='color:#2A9D8F;'>Konform</span>** (Top 15 Variablen).",
      "Die dargestellte Wichtigkeit (Mean Decrease in Accuracy) misst,<br>",
      "wie stark die Vorhersagegenauigkeit des Modells sinkt,",
      "wenn der Einfluss einer einzelnen Variable durch",
      "zufälliges Vermischen ihrer Werte neutralisiert<br>wird.",
      "Ein grosser Wert bedeutet also, dass die Variable",
      "für korrekte Vorhersagen entscheidend ist."
    ),
    x = NULL,
    y = "Wichtigkeit"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.3)),
    plot.subtitle = element_markdown(margin = ggplot2::margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(size = rel(0.9)),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      margin = ggplot2::margin(t = 5)
    ),
    axis.title.y = element_text(margin = ggplot2::margin(r = 10)),
    plot.margin = ggplot2::margin(20, 20, 20, 20),
  )

#' Save the plot with a 16:9 aspect ratio
ggsave(
  file.path("results", theme, "variable_importance.png"),
  plot = horizontal_plot,
  width = 16,
  height = 9,
  units = "in",
  dpi = 300
)
