1 + 1

# Loading of da library

library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(arrow)
library(skimr)
library(janitor)

# Set the seed for reproducibility
set.seed(123)


# Data Baseline info Check
baselineInfo <- function(df, file) {
  print(paste("File:", file))
  print(paste("Number of rows:", nrow(df)))
  print(paste("Number of columns:", ncol(df)))
  print(paste("Column names:", paste(colnames(df), collapse = ", ")))
  print(paste("Data types:", paste(sapply(df, class), collapse = ", ")))
  print(paste("Missing values:", sum(is.na(df))))
  print(paste("Unique values per column:"))
  print(sapply(df, function(x) length(unique(x))))
  head(df, n = 3)
}


# check Fraud Data
checkFraudData <- function(df) {
  # Make the fraud_bool column as integer
  df <- df %>%
    mutate(fraud_bool = as.integer(.data[["fraud_bool"]]))

  # check for values that are not 0 or 1 in the fraud_bool column
  invalid_values <- df %>%
    filter(fraud_bool != 0 & fraud_bool != 1 | is.na(fraud_bool))

  if (nrow(invalid_values) > 0) {
    print("Invalid values found in fraud_bool column:")
    print(invalid_values)
  } else {
    print("All values in fraud_bool column are valid (0 or 1).")
  }
}


# Based on Data Sheet https://github.com/feedzai/bank-account-fraud/blob/main/documents/datasheet.pdf

set_missing_to_na <- function(df) {
  minus1_is_missing <- c(
    "prev_address_months_count",
    "current_address_months_count",
    "bank_months_count",
    "session_length_in_minutes",
    "device_distinct_emails"
  )

  binary_cols <- c(
    "email_is_free", "phone_home_valid", "phone_mobile_valid",
    "has_other_cards", "foreign_request", "keep_alive_session",
    "device_fraud_count", "fraud_bool"
  )

  df %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(minus1_is_missing), ~ dplyr::na_if(., -1)),
      intended_balcon_amount = dplyr::if_else(
        intended_balcon_amount < 0, NA_real_, as.numeric(intended_balcon_amount)
      ),
      dplyr::across(
        dplyr::any_of(binary_cols),
        ~ dplyr::if_else(. %in% c(0, 1), as.integer(.), NA_integer_)
      )
    )
}

# Data Quality Check
dataQualityCheck <- function(df) {
  # Update missing values based on the datasheet
  df <- set_missing_to_na(df)


  # Check for missing values
  missing_values <- sapply(df, function(x) sum(is.na(x)))
  print("Missing values per column:")
  print(missing_values)

  # Check for duplicate rows
  duplicate_rows <- df[duplicated(df), ]
  if (nrow(duplicate_rows) > 0) {
    print(paste("Number of duplicate rows:", nrow(duplicate_rows)))
  } else {
    print("No duplicate rows found.")
  }

  # Check for outliers in numeric columns using IQR method
  numeric_cols <- sapply(df, is.numeric)
  # remove the fraud_bool column from numeric_cols
  numeric_cols["fraud_bool"] <- FALSE
  outlier_info <- lapply(df[, numeric_cols], function(x) {
    x_no_na <- x[!is.na(x)]
    Q1 <- quantile(x_no_na, 0.25)
    Q3 <- quantile(x_no_na, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- x_no_na[x_no_na < lower_bound | x_no_na > upper_bound]
    # return(list(lower_bound = lower_bound, upper_bound = upper_bound, outliers = outliers))
    return(list(lower_bound = lower_bound, upper_bound = upper_bound))
  })

  print("Outlier information for numeric columns:")
  print(outlier_info)
  return(df)
}
# Data Preprocessing based on this file https://github.com/feedzai/bank-account-fraud/blob/main/documents/datasheet.pdf

# Categorical Factors
categorical_factor_vars <- c(
  "payment_type",
  "employment_status",
  "housing_status",
  "source",
  "device_os"
)

# Y/N Factors
binary_factor_vars <- c(
  "email_is_free",
  "phone_home_valid",
  "phone_mobile_valid",
  "has_other_cards",
  "foreign_request",
  "keep_alive_session"
)

precessData <- function(df) {
  # Convert categorical variables to factors
  for (v in categorical_factor_vars) {
    df[[v]] <- as.factor(df[[v]])
  }

  # Convert Binary variables to factors
  for (v in binary_factor_vars) {
    df[[v]] <- factor(
      df[[v]],
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  }

  # Get da numeric variables
  numeric_vars <- names(df)[sapply(df, is.numeric)]

  # remove fraud_bool and month from numeric_vars
  numeric_predictors <- setdiff(numeric_vars, c("fraud_bool", "month"))
  scaling_parameters <- df %>%
    summarise(across(all_of(numeric_predictors),
      list(mean = mean, sd = sd),
      na.rm = TRUE
    ))

  # Normalize numeric variables
  baf_labeled_scaled <- df %>%
    mutate(across(
      all_of(numeric_predictors),
      ~ {
        mu <- mean(.x, na.rm = TRUE)
        sigma <- sd(.x, na.rm = TRUE)
        if (is.na(sigma) || sigma == 0) {
          ifelse(is.na(.x), NA_real_, 0)
        } else {
          (.x - mu) / sigma
        }
      }
    ))

  return(baf_labeled_scaled)
}

saveAsRDS <- function(df, filePath) {
  saveRDS(df, file = filePath)
  print(paste("Data saved as RDS file at:", filePath))
}

# Over Sample of the fraud instances
overSample_df <- function(df, train_p = 0.70) {
  if (!"fraud_bool" %in% names(df)) {
    stop("fraud_bool column not found in input dataframe.")
  }

  if (any(is.na(df$fraud_bool))) {
    stop("fraud_bool contains NA values. Clean target column before oversampling.")
  }

  if (!all(df$fraud_bool %in% c(0, 1))) {
    stop("fraud_bool must contain only 0/1 values.")
  }

  # Same prep logic as your oversampling script
  baf_model <- df %>%
    mutate(
      fraud_bool = as.integer(fraud_bool),
      fraud_status = ifelse(fraud_bool == 1, "Fraud", "Not_Fraud"),
      fraud_status = factor(fraud_status, levels = c("Not_Fraud", "Fraud"))
    ) %>%
    mutate(across(where(is.character), as.factor))

  # Same split logic
  train_index <- createDataPartition(
    baf_model$fraud_status,
    p = train_p,
    list = FALSE
  )

  train_data <- baf_model[train_index, , drop = FALSE]

  # Same oversampling logic
  train_data_over <- caret::upSample(
    x = dplyr::select(train_data, -fraud_bool, -fraud_status), # predictors only
    y = factor(train_data$fraud_bool, levels = c(0, 1)),
    yname = "fraud_bool"
  ) %>%
    dplyr::mutate(
      fraud_bool = as.integer(as.character(fraud_bool)),
      fraud_status = factor(ifelse(fraud_bool == 1, "Fraud", "Not_Fraud"), levels = c("Not_Fraud", "Fraud"))
    ) %>%
    dplyr::relocate(fraud_bool, .before = 1)

  # Safety check
  stopifnot("fraud_bool" %in% names(train_data_over))
  return(train_data_over)
}





# Prep for Regression Analysis

# CSV Files
dataDir <- "./Data/BankData"
dataFiles <- list.files(dataDir, pattern = "\\.csv$", full.names = TRUE)

out_dir <- "./Data/BankData/Processed"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

for (file in dataFiles) {
  print(sprintf("--------------- FILE %s START ---------------", file))
  df_file <- readr::read_csv(file, show_col_types = FALSE)
  baselineInfo(df_file, file)
  checkFraudData(df_file)
  df_file <- dataQualityCheck(df_file)
  processed_df <- precessData(df_file)
  print(paste("Column names:", paste(colnames(processed_df), collapse = ", ")))
  saveAsRDS(processed_df, filePath = paste0(out_dir, "/", basename(file), "_PCD.rds"))
  overSampled_df <- overSample_df(processed_df)
  stopifnot("fraud_bool" %in% names(overSampled_df))
  over_path <- paste0(out_dir, "/", basename(file), "_PCD_OS.rds")
  saveAsRDS(overSampled_df, filePath = over_path)
  reloaded_over <- readRDS(over_path)
  stopifnot("fraud_bool" %in% names(reloaded_over))
  cat("fraud_bool present after save/reload:", "fraud_bool" %in% names(reloaded_over), "\n")
  print(paste("Column names:", paste(colnames(overSampled_df), collapse = ", ")))
  print(sprintf("--------------- FILE %s DONE ---------------", file))

  # break
}


print("All files processed and saved as RDS.")
