library(dplyr)

baseDF <- readRDS("./Data/BankData/Processed/Base.csv_PCD.rds")

total_fraud_count <- sum(baseDF$fraud_bool == 1, na.rm = TRUE)

fraud_by_os <- baseDF %>%
  filter(!is.na(device_os), (fraud_bool == 1)) %>%
  group_by(device_os) %>%
  summarise(
    total_records = n(),
    fraud_1_count = sum(fraud_bool == 1, na.rm = TRUE),
    fraud_1_pct = round(100 * fraud_1_count / total_fraud_count, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(fraud_1_count))

print(fraud_by_os)
