# Configuration
options(dplyr.summarise.inform = FALSE)
thresholds <- c(0.50, 0.30, 0.20, 0.10)
split <- 0.7
useUpBalancing <- TRUE
useRemoveAllNA <- FALSE

# RDS Files
dataDir <- "./Data/BankData/Processed"
dataFiles <- list.files(dataDir, pattern = "\\.rds$", full.names = TRUE)

out_dir <- paste0("./Data/BankData/CalculationResults", ifelse(useRemoveAllNA, "/DropNA", ""))
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(arrow)
library(skimr)
library(janitor)

# Set the seed for reproducibility
set.seed(123)

# Helper functions
validate_input_data <- function(df) {
  if (!"fraud_bool" %in% names(df)) {
    stop("fraud_bool column not found in input data.")
  }

  if (!all(df$fraud_bool %in% c(0, 1), na.rm = TRUE)) {
    stop("fraud_bool must contain only 0/1 values.")
  }

  if (any(is.na(df$fraud_bool))) {
    stop("fraud_bool contains NA values. Clean target before classification.")
  }

  invisible(TRUE)
}

prepare_model_frame <- function(df) {
  df %>%
    mutate(
      fraud_status = if_else(fraud_bool == 1, "Fraud", "Not_Fraud"),
      fraud_status = factor(fraud_status, levels = c("Not_Fraud", "Fraud"))
    )
}

evaluate_at_threshold <- function(actual, predicted_prob, threshold, model_name, train_variant) {
  predicted_class <- if_else(predicted_prob >= threshold, "Fraud", "Not_Fraud")
  predicted_class <- factor(predicted_class, levels = c("Not_Fraud", "Fraud"))

  cm <- caret::confusionMatrix(
    data = predicted_class,
    reference = actual,
    positive = "Fraud"
  )

  tibble(
    train_variant = train_variant,
    model = model_name,
    threshold = threshold,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    precision = as.numeric(cm$byClass["Pos Pred Value"]),
    recall = as.numeric(cm$byClass["Sensitivity"]),
    f1 = as.numeric(cm$byClass["F1"])
  )
}

build_logit_importance <- function(logit_model, predictor_names) {
  coef_tbl <- as.data.frame(summary(logit_model)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      z_value = `z value`,
      p_value = `Pr(>|z|)`
    ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      odds_ratio = exp(estimate),
      feature = vapply(
        term,
        function(x) {
          matches <- predictor_names[vapply(predictor_names, function(p) startsWith(x, p), logical(1))]
          if (length(matches) == 0) x else matches[which.max(nchar(matches))]
        },
        character(1)
      )
    )

  coef_tbl %>%
    group_by(feature) %>%
    arrange(desc(abs(estimate)), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      feature,
      logit_estimate = estimate,
      logit_odds_ratio = odds_ratio,
      logit_p_value = p_value,
      logit_abs_effect = abs(estimate),
      direction = if_else(estimate > 0, "Increase fraud odds", "Decrease fraud odds")
    )
}

build_tree_importance <- function(tree_model) {
  vi <- tree_model$variable.importance
  if (is.null(vi) || length(vi) == 0) {
    return(tibble(feature = character(), tree_importance = numeric(), tree_importance_pct = numeric()))
  }

  tibble(
    feature = names(vi),
    tree_importance = as.numeric(vi)
  ) %>%
    mutate(tree_importance_pct = 100 * tree_importance / sum(tree_importance))
}

build_consensus_importance <- function(logit_importance, tree_importance) {
  consensus <- full_join(logit_importance, tree_importance, by = "feature")

  max_logit_rank <- nrow(consensus) + 1
  max_tree_rank <- nrow(consensus) + 1

  consensus %>%
    mutate(
      logit_rank = dense_rank(desc(logit_abs_effect)),
      tree_rank = dense_rank(desc(tree_importance_pct)),
      logit_rank = if_else(is.na(logit_abs_effect), max_logit_rank, as.numeric(logit_rank)),
      tree_rank = if_else(is.na(tree_importance_pct), max_tree_rank, as.numeric(tree_rank)),
      consensus_score = (logit_rank + tree_rank) / 2,
      consensus_rank = dense_rank(consensus_score)
    ) %>%
    arrange(consensus_rank, consensus_score)
}

run_models <- function(train_df, test_df, thresholds, train_variant_label) {
  predictor_names <- setdiff(names(train_df), c("fraud_status"))

  logit_model <- glm(
    fraud_status ~ . - fraud_bool,
    data = train_df,
    family = binomial()
  )
  logit_prob <- as.numeric(predict(logit_model, newdata = test_df, type = "response"))

  tree_model <- rpart::rpart(
    fraud_status ~ . - fraud_bool,
    data = train_df,
    method = "class",
    control = rpart::rpart.control(cp = 0.001, maxdepth = 6, minsplit = 40)
  )
  tree_prob <- as.numeric(predict(tree_model, newdata = test_df, type = "prob")[, "Fraud"])

  metrics <- bind_rows(
    bind_rows(lapply(thresholds, function(th) evaluate_at_threshold(test_df$fraud_status, logit_prob, th, "Logistic", train_variant_label))),
    bind_rows(lapply(thresholds, function(th) evaluate_at_threshold(test_df$fraud_status, tree_prob, th, "DecisionTree", train_variant_label)))
  )

  logit_roc <- pROC::roc(
    response = test_df$fraud_status,
    predictor = logit_prob,
    levels = c("Not_Fraud", "Fraud"),
    quiet = TRUE
  )
  tree_roc <- pROC::roc(
    response = test_df$fraud_status,
    predictor = tree_prob,
    levels = c("Not_Fraud", "Fraud"),
    quiet = TRUE
  )

  auc_tbl <- tibble(
    train_variant = train_variant_label,
    model = c("Logistic", "DecisionTree"),
    auc = c(as.numeric(pROC::auc(logit_roc)), as.numeric(pROC::auc(tree_roc)))
  )

  logit_importance <- build_logit_importance(logit_model, predictor_names)
  tree_importance <- build_tree_importance(tree_model)
  consensus_importance <- build_consensus_importance(logit_importance, tree_importance)

  list(
    models = list(logit = logit_model, tree = tree_model),
    probs = list(logit = logit_prob, tree = tree_prob),
    roc = list(logit = logit_roc, tree = tree_roc),
    metrics = metrics,
    auc = auc_tbl,
    importance = consensus_importance
  )
}

# Plotting safeguard: if a tree makes no split, keep a placeholder row for visualization
build_tree_importance <- function(tree_model) {
  vi <- tree_model$variable.importance
  if (is.null(vi) || length(vi) == 0) {
    return(tibble(
      feature = "No split selected (original train)",
      tree_importance = 0,
      tree_importance_pct = 0
    ))
  }

  tibble(
    feature = names(vi),
    tree_importance = as.numeric(vi)
  ) %>%
    mutate(tree_importance_pct = 100 * tree_importance / sum(tree_importance))
}

# Drop rows with NA for prediction
naDroper <- function(df) {
  rows_before <- nrow(df)
  df <- df %>% tidyr::drop_na()
  rows_after <- nrow(df)
  cat(sprintf("Rows removed due to NA: %d\n", rows_before - rows_after))
  cat(sprintf("Rows Before removed due to NA: %d\n", rows_before))
  cat(sprintf("Rows After removed due to NA: %d\n", rows_after))
  return(df)
}

# Build Receiver Operating Characteristic (ROC) plot data
build_roc_plot_data <- function(results_list) {
  roc_plot_df <- bind_rows(lapply(results_list, function(res) {
    bind_rows(
      tibble(
        FalsePositiveRate = 1 - res$roc$logit$specificities,
        TruePositiveRate = res$roc$logit$sensitivities,
        curve = paste0("Logistic | ", unique(res$auc$train_variant), " (AUC=", round(res$auc$auc[res$auc$model == "Logistic"], 3), ")")
      ),
      tibble(
        FalsePositiveRate = 1 - res$roc$tree$specificities,
        TruePositiveRate = res$roc$tree$sensitivities,
        curve = paste0("DecisionTree | ", unique(res$auc$train_variant), " (AUC=", round(res$auc$auc[res$auc$model == "DecisionTree"], 3), ")")
      )
    )
  }))

  return(roc_plot_df)
}

dropMostNAColumn <- function(df) {
  df <- df %>% dplyr::select(-prev_address_months_count)
  df <- df %>% dplyr::select(-intended_balcon_amount)
  df <- df %>% dplyr::select(-bank_months_count)
  return(df)
}

# run though all the files.
for (file in dataFiles) {
  print(sprintf("--------------- FILE %s START ---------------", file))

  df_raw <- readRDS(file)
  head(df_raw)
  validate_input_data(df_raw)
  cat(sprintf("Loaded rows: %d | columns: %d\n", nrow(df_raw), ncol(df_raw)))
  df_model <- prepare_model_frame(df_raw)
  if (!useRemoveAllNA) {
    print("Dropping columns with most NA values...")
    df_model <- dropMostNAColumn(df_model)
  } else {
    print("Dropping ALL rows with NA values...")
  }
  df_model <- naDroper(df_model)

  # split the data into train and test sets
  trainIdx <- caret::createDataPartition(df_model$fraud_status, p = split, list = FALSE)
  train_data <- df_model[trainIdx, , drop = FALSE]
  test_data <- df_model[-trainIdx, , drop = FALSE]

  cat("Train class distribution:\n")
  print(prop.table(table(train_data$fraud_status)))
  cat("Test class distribution:\n")
  print(prop.table(table(test_data$fraud_status)))

  # Run models on original train
  result_original <- run_models(
    train_df = train_data,
    test_df = test_data,
    thresholds = thresholds,
    train_variant_label = "OriginalTrain"
  )

  results_list <- list(result_original)

  if (useUpBalancing) {
    # Optional balanced-train branch
    train_data_over <- caret::upSample(
      x = dplyr::select(train_data, -fraud_status),
      y = train_data$fraud_status,
      yname = "fraud_status"
    ) %>% dplyr::mutate(fraud_status = factor(fraud_status, levels = levels(train_data$fraud_status)))

    cat("Balanced-train class distribution:\n")
    print(prop.table(table(train_data_over$fraud_status)))
    # Run models on balanced train if the overbalancing is successful
    if (isTRUE(useUpBalancing)) {
      result_balanced <- run_models(
        train_df = train_data_over,
        test_df = test_data,
        thresholds = thresholds,
        train_variant_label = "BalancedTrain"
      )
      # Both balanced and original results are stored in a list for further analysis
      results_list <- append(list(result_original), list(result_balanced))
    }
  }

  model_metrics <- bind_rows(lapply(results_list, function(x) x$metrics)) %>%
    arrange(model, train_variant, threshold)
  auc_table <- bind_rows(lapply(results_list, function(x) x$auc)) %>%
    arrange(desc(auc))
  print(model_metrics)
  print(auc_table)

  roc_plot_df <- build_roc_plot_data(results_list)

  base_name <- paste0(
    tools::file_path_sans_ext(basename(file)),
    ifelse(useUpBalancing, "_balanced", "_original"),
    ifelse(useRemoveAllNA, "_DropNA", "")
  )

  roc_plot <- ggplot2::ggplot(roc_plot_df, ggplot2::aes(x = FalsePositiveRate, y = TruePositiveRate, color = curve)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste("ROC Curves on Untouched Test Set -", base_name),
      x = "False Positive Rate",
      y = "True Positive Rate",
      color = "Model Variant"
    )

  importance_table <- bind_rows(lapply(results_list, function(x) x$importance)) %>%
    arrange(consensus_rank, consensus_score)

  top_tree <- importance_table %>%
    filter(!is.na(tree_importance_pct)) %>%
    arrange(desc(tree_importance_pct)) %>%
    slice_head(n = 10)

  top_logit <- importance_table %>%
    filter(!is.na(logit_abs_effect)) %>%
    arrange(desc(logit_abs_effect)) %>%
    slice_head(n = 10)

  tree_plot <- ggplot2::ggplot(top_tree, ggplot2::aes(x = reorder(feature, tree_importance_pct), y = tree_importance_pct)) +
    ggplot2::geom_col(fill = "#1b9e77") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top 10 Decision Tree Features (Original Train) -", base_name),
      x = "Feature",
      y = "Importance (%)"
    )

  logit_plot <- ggplot2::ggplot(top_logit, ggplot2::aes(x = reorder(feature, logit_abs_effect), y = logit_abs_effect, fill = direction)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Top 10 Logistic Features by Absolute Effect -", base_name),
      x = "Feature",
      y = "|Coefficient|",
      fill = "Direction"
    )

  roc_plot_path <- file.path(out_dir, paste0(base_name, "_roc_curve.png"))
  tree_plot_path <- file.path(out_dir, paste0(base_name, "_top_tree_features.png"))
  logit_plot_path <- file.path(out_dir, paste0(base_name, "_top_logit_features.png"))

  ggplot2::ggsave(filename = roc_plot_path, plot = roc_plot, width = 9, height = 6, dpi = 300)
  ggplot2::ggsave(filename = tree_plot_path, plot = tree_plot, width = 9, height = 6, dpi = 300)
  ggplot2::ggsave(filename = logit_plot_path, plot = logit_plot, width = 9, height = 6, dpi = 300)

  cat("Saved plot:", roc_plot_path, "\n")
  cat("Saved plot:", tree_plot_path, "\n")
  cat("Saved plot:", logit_plot_path, "\n")

  best_threshold_table <- model_metrics %>%
    group_by(model, train_variant) %>%
    slice_max(order_by = f1, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(f1))

  top_factors <- importance_table %>% slice_head(n = 10)

  cat("Best threshold per model variant (by F1):\n")
  print(best_threshold_table)

  cat("\nTop consensus factors for fraud_bool == 1:\n")
  print(top_factors)

  # Export report tables to CSV
  metrics_out <- file.path(out_dir, sprintf("%s_classification_metrics_all_thresholds.csv", base_name))
  best_out <- file.path(out_dir, sprintf("%s_classification_metrics_best_threshold.csv", base_name))
  auc_out <- file.path(out_dir, sprintf("%s_classification_auc_summary.csv", base_name))
  top_factors_out <- file.path(out_dir, sprintf("%s_classification_top_factors.csv", base_name))
  top_tree_out <- file.path(out_dir, sprintf("%s_classification_top_tree_features.csv", base_name))
  top_logit_out <- file.path(out_dir, sprintf("%s_classification_top_logit_features.csv", base_name))

  readr::write_csv(model_metrics, metrics_out)
  readr::write_csv(best_threshold_table, best_out)
  readr::write_csv(auc_table, auc_out)
  readr::write_csv(top_factors, top_factors_out)
  readr::write_csv(top_tree, top_tree_out)
  readr::write_csv(top_logit, top_logit_out)

  cat("\nSaved CSV files:\n")
  cat("-", metrics_out, "\n")
  cat("-", best_out, "\n")
  cat("-", auc_out, "\n")
  cat("-", top_factors_out, "\n")
  cat("-", top_tree_out, "\n")
  cat("-", top_logit_out, "\n")
  print(sprintf("--------------- FILE %s DONE ---------------", file))
}

print("Classification analysis Script Complete")
