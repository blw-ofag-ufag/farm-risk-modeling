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
library(pROC) # for ROC curve analysis
library(PRROC) # for Precision-Recall curve analysis
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
data <- base::readRDS("data/data.rds") %>%

  data_split(
    theme = theme,
    predictors = readRDS("resources/predictors.rds"),
    training_size = 0.8
  )

#' Read predictors
variables <- base::readRDS("resources/variables.rds")

#' Open a "log" file to write the results to
results_directory <- base::file.path("results", theme)
if (!base::dir.exists(results_directory)) base::dir.create(results_directory)
base::sink(base::file.path("results", theme, "results.txt"))

#' Make a summary of the specific subset of the data used here (subset by theme)
df <- label_dataset(data$original, metadata = meta, lang = "de", unit = TRUE)
print_title("summary of the specific subset of the data used")
summarytools::dfSummary(
  x = df,
  varnumbers = FALSE,
  valid.col = FALSE,
  display.labels = TRUE,
  silent = TRUE,
  style = "multiline",
)

#' The summary shows some predictors have `Inf` values, which must be removed.
#' We'll replace them with NA and impute them in the next step.
data$train[sapply(data$train, is.infinite)] <- NA
data$test[sapply(data$test, is.infinite)] <- NA

#' Vector with predictors (remove the ones dropped by the split, this might be
#' the case because there is only one factor left in the training data)
predictors <- base::setdiff(
  x = variables[variables %in% base::colnames(data$train)],
  y = "result"
)

#' List the variables that are *not* used for modelling due to incompleteness
unused_variables <- variables[!(variables %in% predictors)][-1]
print_title("unused variables")
cat(paste(get_titles(unused_variables, meta, "de", FALSE), collapse = "\n"))

#' Create the model formula. The goal is to predict `result` using all other
#' variables defined in the `variables` vector.
formula <- as.formula(paste("result ~", paste(predictors, collapse = " + ")))
print_title("formula")
print(formula)

#' =============================================================================
#' HYPERPARAMETER TUNING AND MODEL TRAINING
#' -----------------------------------------------------------------------------
#' A random forest model is trained on the prepared `train` dataset to predict
#' the inspection `result`.
#' =============================================================================

#' Define the cross-validation method. We use 10-fold CV to get a robust
#' estimate of performance. We also tell caret to calculate metrics needed for
#' the ROC curve. The `sampling = "down"` argument handles class imbalance by
#' down-sampling the majority class ("Pass") in each resample to be the same
#' size as the minority class ("Fail").
train_control <- caret::trainControl(
  method = "cv", # cross validation
  number = 10,
  summaryFunction = twoClassSummary, # calculates ROC, Sens, Spec
  classProbs = TRUE,
  verboseIter = FALSE,
  sampling = "down" # handles class imbalance
)

#' Define the grid of hyperparameters to search; for this random forest model,
#' that is only 'mtry', the the number of variables randomly sampled at each
#' split. We'll test a sequence of values to see which one performs best.
tune_grid <- base::expand.grid(
  mtry = seq(from = 2, to = floor(sqrt(length(predictors)))*2, by = 2)
)

#' Train the model using caret's `train` function
#' This will automatically test all mtry values in our tune_grid using
#' 10-fold CV and select the best one based on the ROC metric.
model_tuned <- caret::train(
  form = formula,
  data = data$train,
  preProcess = "medianImpute",
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  metric = "ROC",
  importance = TRUE
)

#' Print the results of the tuning process
print_title("Results of the tuning process")
print(model_tuned)

#' The best model found during cross-validation is stored in the `finalModel`
#' element
model <- model_tuned$finalModel

#' =============================================================================
#' MODEL PERFORMANCE EVALUATION
#' -----------------------------------------------------------------------------
#' Evaluate the model's performance on the unseen test dataset using a confusion
#' matrix and related metrics.
#' =============================================================================

#' Generate predictions on the test set
predictions <- predict(model_tuned, newdata = data$test)

# Generate class probabilities needed for ROC and PR curves
predictions_prob <- predict(model_tuned, newdata = data$test, type = "prob")

#' Generate the confusion matrix and a comprehensive set of statistics. We set
#' `positive = "Fail"` to get metrics like sensitivity and specificity from the
#' perspective of correctly identifying failed inspections.
confusion_matrix <- caret::confusionMatrix(
  data = predictions,
  reference = data$test$result,
  positive = "Fail"
)

#' Save confusion matrix with test scores
saveRDS(confusion_matrix, file.path("results", theme, "confusion-matrix.rds"))

#' Print results to text summary
print_title("model summary")
print(model)
print_title("confusion matrix")
print(confusion_matrix)


#' =============================================================================
#' ROC-AUC AND PRECISION-RECALL ANALYSIS
#' -----------------------------------------------------------------------------
#' We calculate and save the ROC and Precision-Recall curve data.
#' =============================================================================

#' Calculate ROC curve and AUC
roc_obj <- pROC::roc(data$test$result, predictions_prob[, "Fail"], quiet = TRUE)
auc_value <- pROC::auc(roc_obj)

#' Save ROC object
saveRDS(roc_obj, file.path("results", theme, "roc_object.rds"))

#' Calculate Precision-Recall curve and AUC
pr_obj <- PRROC::pr.curve(
  scores.class0 = predictions_prob[, "Fail"],
  weights.class0 = as.numeric(data$test$result == "Fail"),
  curve = TRUE
)

#' Save PR object
saveRDS(pr_obj, file.path("results", theme, "pr_object.rds"))

#' Print AUC values to the summary file
print_title("ROC-AUC and Precision-Recall AUC")
print(roc_obj)
print(pr_obj)

#' =============================================================================
#' EVALUATION OF PREDICTIONS CONSIDERING NON-AVAILABLE PREDICTORS
#' -----------------------------------------------------------------------------
#' Here, we evaluate the model's performance on subsets of the test data,
#' grouped by canton and inspection type. This helps us understand how well the
#' model predicts outcomes based on inherent farm features, controlling for
#' these contextual variables.
#' =============================================================================

# create a data frame with true values, predictions, and grouping variables
evaluation_df <- tibble::tibble(
  canton = data$test$canton,
  type = data$test$type,
  result = data$test$result,
  pred_prob_fail = predictions_prob[, "Fail"]
)

# compute ROC-AUC for each combination of inspection canton and type
performance_by_group <- evaluation_df %>%
  dplyr::group_by(canton, type) %>%
  dplyr::summarise(
    N = dplyr::n(),
    fails = sum(result == "Fail"),
    roc_auc = if (length(unique(result)) > 1) {
      as.numeric(pROC::auc(pROC::roc(result, pred_prob_fail, quiet = TRUE)))
    } else {
      NA
    },
    pr_auc = if (length(unique(result)) > 1) {
      PRROC::pr.curve(
        scores.class0 = pred_prob_fail[result == "Fail"],
        scores.class1 = pred_prob_fail[result == "Pass"],
        curve = FALSE
      )$auc.integral
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  dplyr::arrange(desc(roc_auc)) # Sort by performance

# print the results to the console and the log file
print_title("Grouped Model Performance (by Canton and Inspection Type)")
print(knitr::kable(performance_by_group, digits = 3, format = "markdown"))

# Also save results as a CSV
utils::write.csv(
  x = performance_by_group,
  file = file.path("results", theme, "cross-wise-results.csv")
)

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
print_title("sorted importance scores")
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
  scale_fill_gradient(low = "#EBF5F4", high = "#3366CC") +
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
#' PLOT METRICS VS. THRESHOLD
#' -----------------------------------------------------------------------------
#' Create a plot to visualize how key performance metrics change as the
#' probability threshold for classification is adjusted. This helps in
#' understanding the trade-offs and selecting an optimal threshold.
#' =============================================================================

#' Define a sequence of thresholds to evaluate from 0 to 1
thresholds <- seq(0, 1, by = 0.01)

#' Calculate metrics for each threshold
#' We iterate over each threshold, calculate the confusion matrix, and return
#' a tidy data frame with the results, using German expressions for the metrics.
metrics_by_threshold <- purrr::map_dfr(thresholds, ~{

  # Predict classes based on the current threshold (.x)
  predicted <- ifelse(predictions_prob[, "Fail"] >= .x, "Fail", "Pass") %>%
    factor(levels = levels(data$test$result))

  # Generate the confusion matrix for the current threshold
  cm <- caret::confusionMatrix(predicted, data$test$result, positive = "Fail")

  # Return a tibble with the threshold and the desired metrics
  tibble::tibble(
    Schwellenwert = .x,
    Sensitivität = cm$byClass["Sensitivity"],
    Spezifität = cm$byClass["Specificity"],
    Präzision = cm$byClass["Precision"],
    `F1-Wert` = cm$byClass["F1"],
    `Ausgewogene Genauigkeit` = cm$byClass["Balanced Accuracy"],
    Genauigkeit = cm$overall["Accuracy"]
  )
})

#' At a threshold of 1, no "Fail" is predicted, leading to TP=0 and FP=0.
#' This makes Precision (TP/(TP+FP)) undefined (NaN). We'll replace NaN with 0.
metrics_by_threshold <- metrics_by_threshold %>%
  mutate(
    Präzision = ifelse(is.nan(Präzision), 0, Präzision),
    `F1-Wert` = ifelse(is.nan(`F1-Wert`), 0, `F1-Wert`)
  )

#' Pivot the data into a long format for easy plotting with ggplot2
metrics_long <- metrics_by_threshold %>%
  tidyr::pivot_longer(
    cols = -Schwellenwert,
    names_to = "Metrik",
    values_to = "Wert"
  )

#' Find the threshold that maximizes Balanced Accuracy to highlight on the plot
best_threshold_obj <- metrics_by_threshold %>%
  slice(which.max(`Ausgewogene Genauigkeit`))

#' Create the plot
plot_thresholds <- ggplot(
    metrics_long,
    aes(x = Schwellenwert, y = Wert, color = Metrik)
  ) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(
    "Sensitivität" = "#E76F51",
    "Spezifität" = "#264653",
    "Präzision" = "#F4A261",
    "F1-Wert" = "#E9C46A",
    "Ausgewogene Genauigkeit" = "#2A9D8F",
    "Genauigkeit" = "#8ab17d"
  )) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2), name = "Schwellenwert"
  ) +
  scale_y_continuous(labels = scales::percent, name = "Metrik-Wert") +
  labs(
    title = paste("Performance-Metriken vs. Schwellenwert für", theme),
    subtitle = "Vergleich von Metriken über gesamten Schwellenwertbereich.",
    color = "Metrik:"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.3)),
    plot.subtitle = element_text(margin = ggplot2::margin(b = 15)),
    legend.position = "bottom",
    panel.grid = element_line(linetype = "dotted"),
    axis.title = element_text(face = "bold"),
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )

#' Save the metrics-vs-threshold plot
ggsave(
  file.path("results", theme, "metrics_vs_threshold.png"),
  plot = plot_thresholds,
  width = 11,
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
    "Konform" = "#3366CC"
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
      "<span style='color:#3366CC;'>Konform</span>** (Top 15 Variablen).",
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


#' =============================================================================
#' PLOT THE ROC CURVE
#' -----------------------------------------------------------------------------
#' Create a visual representation of the model's ability to distinguish between
#' the positive and negative classes.
#' =============================================================================

plot_roc <- ggroc(roc_obj, colour = "#3366CC", linewidth = 1.2) +
  annotate(
    "segment",
    x = 1, xend = 0, y = 0, yend = 1,
    color = "grey",
    linetype = "dashed"
  ) +
  coord_fixed() +
  labs(
    title = paste("ROC-Kurve für den Themenbereich", theme),
    subtitle = paste0(
      "Modellperformance auf dem Test-Datensatz. AUC = ",
      round(auc_value, 3)
    ),
    x = "Falsch-Positiv-Rate (1 - Spezifität)",
    y = "Richtig-Positiv-Rate (Sensitivität)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.3)),
    plot.subtitle = element_text(margin = ggplot2::margin(b = 15)),
    legend.position = "none",
    panel.grid = element_line(linetype = "dotted"),
    axis.title = element_text(face = "bold"),
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )

#' Save the ROC curve plot
ggsave(
  file.path("results", theme, "roc_curve.png"),
  plot = plot_roc,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300
)

#' =============================================================================
#' PLOT THE PRECISION-RECALL CURVE
#' -----------------------------------------------------------------------------
#' Create a visual representation of the model's performance, which is often
#' more informative than ROC curves for imbalanced datasets.
#' =============================================================================

pr_df <- data.frame(
  Recall = pr_obj$curve[, 1],
  Precision = pr_obj$curve[, 2]
)

plot_pr <- ggplot(pr_df, aes(x = Recall, y = Precision)) +
  geom_line(color = "#3366CC", linewidth = 1.2) +
  labs(
    title = paste("Precision-Recall-Kurve für den Themenbereich", theme),
    subtitle = paste0(
      "Modellperformance auf dem Test-Datensatz. AUC = ",
      round(pr_obj$auc.integral, 3)
    ),
    x = "Recall (Sensitivität)",
    y = "Precision (Positiver Vorhersagewert)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.3)),
    plot.subtitle = element_text(margin = ggplot2::margin(b = 15)),
    legend.position = "none",
    panel.grid = element_line(linetype = "dotted"),
    axis.title = element_text(face = "bold"),
    plot.margin = ggplot2::margin(20, 20, 20, 20)
  )

#' Save the Precision-Recall curve plot
ggsave(
  file.path("results", theme, "pr_curve.png"),
  plot = plot_pr,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300
)
