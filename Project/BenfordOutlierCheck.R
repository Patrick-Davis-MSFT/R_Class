library(dplyr)

# Inputs
columns_to_check <- c(
  "credit_risk_score",
  "velocity_6h",
  "velocity_24h",
  "bank_branch_count_8w",
  "month",
  "customer_age",
  "days_since_request",
  "intended_balcon_amount",
  "zip_count_4w"
)

benford_z_cutoff <- 150.0 # Yeah thats dumb but I got something less than flagging 100%

rds_path <- "./Data/BankData/Processed/Base.csv_PCD.rds"

# Load data
df <- readRDS(rds_path)
head(df, 3)
# Validate required columns

# Benford expected first-digit probabilities
benford_probs <- log10(1 + 1 / (1:9))
print("Benford expected first-digit probabilities:")
print(benford_probs)

first_digit <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- abs(x)
  x[!is.finite(x) | x <= 0] <- NA_real_
  floor(x / (10^floor(log10(x))))
}

find_benford_outliers <- function(x, z_cutoff = 2.58) {
  d <- first_digit(x)
  valid <- !is.na(d)
  n <- sum(valid)

  if (n == 0) {
    return(list(
      outlier_rows = rep(FALSE, length(x)),
      digit_table = tibble(
        digit = 1:9,
        expected_prop = benford_probs,
        observed_prop = NA_real_,
        z_score = NA_real_,
        outlier_digit = FALSE
      )
    ))
  }

  observed_counts <- tabulate(d[valid], nbins = 9)
  observed_prop <- observed_counts / n
  se <- sqrt(benford_probs * (1 - benford_probs) / n)
  z_score <- (observed_prop - benford_probs) / se

  outlier_digits <- which(abs(z_score) > z_cutoff)

  outlier_rows <- rep(FALSE, length(x))
  if (length(outlier_digits) > 0) {
    outlier_rows[valid] <- d[valid] %in% outlier_digits
  }

  digit_table <- tibble(
    digit = 1:9,
    expected_prop = benford_probs,
    observed_prop = observed_prop,
    z_score = z_score,
    outlier_digit = (1:9) %in% outlier_digits
  )

  list(outlier_rows = outlier_rows, digit_table = digit_table)
}

safe_pct <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }
  round(100 * mean(x, na.rm = TRUE), 2)
}

fraud_flag <- df$fraud_bool == 1
overall_fraud_count <- sum(fraud_flag, na.rm = TRUE)
overall_fraud_pct <- safe_pct(fraud_flag)

summary_rows <- list()
digit_rows <- list()
all_outlier_flags <- list()

for (col_name in columns_to_check) {
  out <- find_benford_outliers(df[[col_name]], z_cutoff = benford_z_cutoff)
  outlier_flag <- out$outlier_rows
  non_outlier_flag <- !outlier_flag

  outlier_n <- sum(outlier_flag, na.rm = TRUE)
  valid_n <- sum(!is.na(first_digit(df[[col_name]])))

  summary_rows[[col_name]] <- tibble(
    column = col_name,
    cnt = valid_n,
    outlier_rows = outlier_n,
    outlier_pct_of_cnt = round(100 * outlier_n / max(valid_n, 1), 2),
    fraud_count_in_outliers = sum(fraud_flag & outlier_flag, na.rm = TRUE),
    fraud_pct_in_outliers = safe_pct(fraud_flag[outlier_flag]),
    fraud_pct_in_non_outliers = safe_pct(fraud_flag[non_outlier_flag]),
    overall_fraud_count = overall_fraud_count,
    overall_fraud_pct = overall_fraud_pct
  )

  digit_rows[[col_name]] <- out$digit_table %>%
    mutate(column = col_name, .before = 1)

  all_outlier_flags[[col_name]] <- outlier_flag
}

summary_table <- bind_rows(summary_rows) %>% arrange(desc(outlier_rows))
digit_level_table <- bind_rows(digit_rows)

# Combined outlier flag across any checked column
any_outlier <- Reduce(`|`, all_outlier_flags)
combined_summary <- tibble(
  rows_total = nrow(df),
  rows_outlier_any_checked_column = sum(any_outlier, na.rm = TRUE),
  rows_outlier_any_pct = round(100 * mean(any_outlier, na.rm = TRUE), 2),
  fraud_count_total = overall_fraud_count,
  fraud_pct_total = overall_fraud_pct,
  fraud_pct_in_any_outlier_rows = safe_pct(fraud_flag[any_outlier]),
  fraud_pct_in_no_outlier_rows = safe_pct(fraud_flag[!any_outlier])
)

cat("\n=== Benford Outlier Summary (Per Column) ===\n")
print(summary_table)

cat("\n=== Benford Digit-Level Diagnostics ===\n")
print(digit_level_table)

cat("\n=== Combined Comparison (Any Outlier Across Checked Columns) ===\n")
print(combined_summary)

# Optional exports
write.csv(summary_table, "./Data/BankData/CalculationResults/benford_outlier_summary.csv", row.names = FALSE)
write.csv(digit_level_table, "./Data/BankData/CalculationResults/benford_digit_diagnostics.csv", row.names = FALSE)
write.csv(combined_summary, "./Data/BankData/CalculationResults/benford_outlier_combined_summary.csv", row.names = FALSE)

cat("\nSaved CSV files to ./Data/BankData/CalculationResults\n")
