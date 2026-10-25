
# ============================================
# FLASH FLOOD RESEARCH - PROJECT SETUP
# ============================================

# Current project directory
project_dir <- getwd()

# Create required folders
dir.create("dataset", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)

# Check folders
cat("Project directory:\n", project_dir, "\n\n")

cat("Folders created:\n")
print(list.files())

# Locate the dataset without relying only on one machine-specific path.
# Recommended layout: PaperIEEE/dataset/ or sibling ../dataset/.
# The absolute path is retained only as a fallback for the original PC.
dataset_filename <- "bacia_rio_bengalas_2019_2023_out01h00m.csv"

dataset_candidates <- c(
  file.path(project_dir, "dataset", dataset_filename),
  file.path(project_dir, "..", "dataset", dataset_filename),
  file.path(
    "E:/Mtech/3rdsem/PredictiveAnalysis/CA2",
    "CA2_pred_paper", "dataset", dataset_filename
  )
)
dataset_candidates <- normalizePath(
  dataset_candidates, winslash = "/", mustWork = FALSE
)
existing_candidates <- dataset_candidates[file.exists(dataset_candidates)]

if (length(existing_candidates) == 0L) {
  stop(
    "Dataset file was not found. Put ", dataset_filename,
    " in the project's dataset/ folder or its parent dataset/ folder.\n",
    "Checked paths:\n", paste(dataset_candidates, collapse = "\n")
  )
}

data_file <- existing_candidates[[1]]
original_data_dir <- dirname(data_file)
cat("\nDataset file selected:\n", data_file, "\n")
cat("Dataset directory exists:", dir.exists(original_data_dir), "\n")

csv_files <- list.files(
  original_data_dir, pattern = "\\.csv$", full.names = TRUE,
  ignore.case = TRUE
)
cat("CSV files found in selected directory:", length(csv_files), "\n")
print(basename(csv_files))





# ============================================
# STEP 2: DATASET AUDIT (R ONLY)
# ============================================

# Install required packages if missing
required_packages <- c("data.table", "lubridate")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace,
          logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(lubridate)

# `data_file` is resolved in the project setup block above.
if (!file.exists(data_file)) {
  stop("Resolved dataset path no longer exists: ", data_file)
}

cat("\n========== FILE INFORMATION ==========\n")
cat("File:", basename(data_file), "\n")
cat("File size (MB):",
    round(file.info(data_file)$size / 1024^2, 2), "\n")

# Read only this CSV
df <- fread(data_file, na.strings = c("", "NA", "NaN"))

cat("\n========== DATA DIMENSIONS ==========\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

cat("\n========== COLUMN NAMES ==========\n")
print(names(df))

cat("\n========== FIRST 6 ROWS ==========\n")
print(head(df[, 1:min(8, ncol(df)), with = FALSE]))

cat("\n========== DATA TYPES ==========\n")
print(sapply(df[, 1:min(8, ncol(df)), with = FALSE], class))

# Check essential columns
required_columns <- c(
  "datahora",
  "chuva_conselheiro",
  "nivel_conselheiro",
  "out01h00m"
)

cat("\n========== REQUIRED COLUMNS ==========\n")
print(setNames(
  required_columns %in% names(df),
  required_columns
))

# Missing values
cat("\n========== MISSING VALUES ==========\n")
cat("Total missing cells:", sum(is.na(df)), "\n")
cat("Rows with any missing values:", sum(!complete.cases(df)), "\n")

# Timestamp audit
if ("datahora" %in% names(df)) {
  timestamp <- parse_date_time(
    as.character(df$datahora),
    orders = c(
      "Ymd HMS", "Ymd HM",
      "dmY HMS", "dmY HM",
      "mdY HMS", "mdY HM"
    ),
    tz = "UTC",
    quiet = TRUE
  )
  
  cat("\n========== TIMESTAMP AUDIT ==========\n")
  cat("Unparsed timestamps:", sum(is.na(timestamp)), "\n")
  cat("First timestamp:",
      as.character(min(timestamp, na.rm = TRUE)), "\n")
  cat("Last timestamp:",
      as.character(max(timestamp, na.rm = TRUE)), "\n")
  cat("Duplicate timestamps:",
      sum(duplicated(timestamp[!is.na(timestamp)])), "\n")
  cat("Chronologically sorted:",
      all(diff(as.numeric(timestamp[!is.na(timestamp)])) >= 0),
      "\n")
}

# Target and sensor summaries
numeric_columns <- intersect(
  c("chuva_conselheiro", "nivel_conselheiro", "out01h00m"),
  names(df)
)

cat("\n========== NUMERIC SUMMARIES ==========\n")
print(summary(df[, ..numeric_columns]))

# Save audit summary
dir.create("results", showWarnings = FALSE)

audit_summary <- data.table(
  rows = nrow(df),
  columns = ncol(df),
  missing_cells = sum(is.na(df)),
  rows_with_missing = sum(!complete.cases(df)),
  file_size_MB = round(file.info(data_file)$size / 1024^2, 2)
)

fwrite(audit_summary, "results/data_audit_summary.csv")

cat("\nAudit summary saved to results/data_audit_summary.csv\n")
cat("\n========== AUDIT COMPLETE ==========\n")






# ============================================
# STEP 3: VERIFY ORIGINAL TIMESTAMPS
# ============================================

library(data.table)
library(lubridate)

# Read only the timestamp column as text
time_check <- fread(
  data_file,
  select = "datahora",
  colClasses = c(datahora = "character"),
  na.strings = c("", "NA", "NaN")
)

raw_time <- time_check$datahora

# Parse timestamps independently
parsed_time <- parse_date_time(
  raw_time,
  orders = c(
    "Ymd HMS", "Ymd HM",
    "dmY HMS", "dmY HM",
    "mdY HMS", "mdY HM"
  ),
  tz = "UTC",
  quiet = TRUE
)

# Report missing and unparsed values
cat("\n========== RAW TIMESTAMP CHECK ==========\n")
cat("Total rows:", length(raw_time), "\n")
cat("Missing raw timestamps:", sum(is.na(raw_time)), "\n")
cat(
  "Blank timestamps:",
  sum(!is.na(raw_time) & trimws(raw_time) == ""),
  "\n"
)
cat("Unparsed timestamps:", sum(is.na(parsed_time)), "\n")

# Display examples of nonblank values that failed parsing
bad_rows <- which(
  !is.na(raw_time) &
    trimws(raw_time) != "" &
    is.na(parsed_time)
)

cat("\n========== UNPARSED EXAMPLES ==========\n")

if (length(bad_rows) > 0) {
  print(
    data.table(
      row_number = head(bad_rows, 20),
      raw_timestamp = head(raw_time[bad_rows], 20)
    )
  )
} else {
  cat("No nonblank timestamps failed parsing.\n")
}

# Check time gaps between consecutive valid timestamps
valid_time <- parsed_time[!is.na(parsed_time)]
time_gaps <- as.numeric(diff(valid_time), units = "mins")

cat("\n========== TIME INTERVALS ==========\n")
cat("30-minute intervals:", sum(time_gaps == 30), "\n")
cat("Intervals below 30 minutes:", sum(time_gaps < 30), "\n")
cat("Intervals above 30 minutes:", sum(time_gaps > 30), "\n")
cat("Nonpositive intervals:", sum(time_gaps <= 0), "\n")

if (length(time_gaps) > 0) {
  cat("Minimum gap (minutes):", min(time_gaps), "\n")
  cat("Maximum gap (minutes):", max(time_gaps), "\n")
}

# Save timestamp audit
timestamp_audit <- data.table(
  row_number = seq_along(raw_time),
  raw_timestamp = raw_time,
  parsed_timestamp = parsed_time
)

fwrite(
  timestamp_audit,
  "results/timestamp_audit.csv"
)

cat("\nSaved: results/timestamp_audit.csv\n")









# ============================================
# STEP 4: VERIFY FORECAST TARGET ALIGNMENT
# ============================================

library(data.table)

# df was loaded in Step 2
setDT(df)

# Sort chronologically
setorder(df, datahora)

# Create a timestamp lookup table for observed water levels
level_lookup <- df[
  ,
  .(
    future_time = datahora,
    future_level = nivel_conselheiro
  )
]

# For each row, find the observation exactly one hour later
target_check <- data.table(
  row_id = seq_len(nrow(df)),
  datahora = df$datahora,
  current_level = df$nivel_conselheiro,
  provided_target = df$out01h00m
)

target_check[, future_time := datahora + 3600]

target_check <- merge(
  target_check,
  level_lookup,
  by.x = "future_time",
  by.y = "future_time",
  all.x = TRUE,
  sort = FALSE
)

# Restore order after merge
setorder(target_check, row_id)

# Match only rows where the exact future timestamp exists
matched <- !is.na(target_check$future_level)

difference <- target_check$provided_target[matched] -
  target_check$future_level[matched]

cat("\n========== TARGET ALIGNMENT ==========\n")
cat("Total rows:", nrow(target_check), "\n")
cat("Rows with exact +1-hour observation:", sum(matched), "\n")
cat("Rows without exact +1-hour observation:", sum(!matched), "\n")

if (sum(matched) > 0) {
  cat("Exact target matches:",
      sum(abs(difference) < 1e-8), "\n")
  cat("Mean absolute difference:",
      mean(abs(difference)), "\n")
  cat("Maximum absolute difference:",
      max(abs(difference)), "\n")
  cat("Correlation:",
      cor(
        target_check$provided_target[matched],
        target_check$future_level[matched]
      ), "\n")
}

# Save a compact report
alignment_report <- data.table(
  total_rows = nrow(target_check),
  exact_future_matches = sum(matched),
  rows_without_future_match = sum(!matched),
  exact_value_matches = if (sum(matched) > 0) {
    sum(abs(difference) < 1e-8)
  } else {
    NA_integer_
  },
  mean_absolute_difference = if (sum(matched) > 0) {
    mean(abs(difference))
  } else {
    NA_real_
  },
  maximum_absolute_difference = if (sum(matched) > 0) {
    max(abs(difference))
  } else {
    NA_real_
  }
)

fwrite(
  alignment_report,
  "results/target_alignment_report.csv"
)

cat("\nSaved: results/target_alignment_report.csv\n")










# ============================================
# STEP 5: CHRONOLOGICAL DATA SPLIT
# ============================================

library(data.table)

setDT(df)
setorder(df, datahora)

# Confirm required columns
stopifnot(all(c(
  "datahora",
  "chuva_conselheiro",
  "nivel_conselheiro",
  "out01h00m"
) %in% names(df)))

# Chronological 70/15/15 split
n <- nrow(df)

train_end <- floor(0.70 * n)
validation_end <- floor(0.85 * n)

df[, split := "test"]
df[seq_len(train_end), split := "train"]
df[(train_end + 1):validation_end, split := "validation"]

# Report split sizes and date ranges
split_report <- df[, .(
  rows = .N,
  start_time = min(datahora),
  end_time = max(datahora),
  target_missing = sum(is.na(out01h00m)),
  level_missing = sum(is.na(nivel_conselheiro))
), by = split]

split_report[, split_order := match(
  split, c("train", "validation", "test")
)]
setorder(split_report, split_order)
split_report[, split_order := NULL]

cat("\n========== CHRONOLOGICAL SPLIT ==========\n")
print(split_report)

# Verify split chronology
train_last <- max(df[split == "train", datahora])
validation_first <- min(df[split == "validation", datahora])
validation_last <- max(df[split == "validation", datahora])
test_first <- min(df[split == "test", datahora])

cat("\nTraining ends before validation:",
    train_last < validation_first, "\n")
cat("Validation ends before test:",
    validation_last < test_first, "\n")

# Save split report
fwrite(
  split_report,
  "results/chronological_split_report.csv"
)

cat("\nSaved: results/chronological_split_report.csv\n")






# ============================================
# STEP 6: PERSISTENCE BASELINE
# ============================================

library(data.table)

# Check required columns
stopifnot(all(c(
  "datahora",
  "nivel_conselheiro",
  "out01h00m",
  "split"
) %in% names(df)))

# Predict next-hour level using current level
eval_df <- copy(df[
  split %in% c("validation", "test"),
  .(
    datahora,
    split,
    actual = out01h00m,
    predicted = nivel_conselheiro
  )
])

# Keep rows with valid actual and predicted values
eval_df <- eval_df[
  is.finite(actual) & is.finite(predicted)
]

# Calculate metrics
metric_function <- function(actual, predicted) {
  error <- actual - predicted
  
  data.table(
    n = length(actual),
    MAE = mean(abs(error)),
    RMSE = sqrt(mean(error^2)),
    Bias = mean(predicted - actual)
  )
}

baseline_metrics <- eval_df[
  ,
  metric_function(actual, predicted),
  by = split
]

# Consistent reporting order
baseline_metrics[, split_order :=
                   match(split, c("validation", "test"))]
setorder(baseline_metrics, split_order)
baseline_metrics[, split_order := NULL]

cat("\n========== PERSISTENCE BASELINE ==========\n")
print(baseline_metrics)

# Save results
fwrite(
  baseline_metrics,
  "results/persistence_baseline_metrics.csv"
)

cat("\nSaved: results/persistence_baseline_metrics.csv\n")






# ============================================
# STEP 7: EXACT +1-HOUR ALIGNMENT CHECK
# ============================================

library(data.table)

setDT(df)
setorder(df, datahora)

# Ensure timestamps are unique before matching
duplicate_times <- df[, sum(duplicated(datahora))]

cat("\nDuplicate timestamps:", duplicate_times, "\n")

if (duplicate_times > 0) {
  stop("Duplicate timestamps found. Resolve them before alignment.")
}

# Locate the row at exactly t + 1 hour
timestamp_num <- as.numeric(df$datahora)
future_idx <- match(timestamp_num + 3600, timestamp_num)

df[, exact_future_exists := !is.na(future_idx)]

# Compare stored target with observed level at exactly t + 1 hour
future_level <- rep(NA_real_, nrow(df))
matched <- which(!is.na(future_idx))

future_level[matched] <- df$nivel_conselheiro[future_idx[matched]]

df[, future_level_exact := future_level]

df[, alignment_error := fifelse(
  exact_future_exists,
  abs(out01h00m - future_level_exact),
  NA_real_
)]

# Alignment audit by split
alignment_report <- df[
  split %in% c("train", "validation", "test"),
  .(
    rows = .N,
    exact_future_rows = sum(exact_future_exists),
    no_exact_future_rows = sum(!exact_future_exists),
    target_mismatches = sum(
      exact_future_exists & alignment_error > 1e-8,
      na.rm = TRUE
    ),
    max_abs_alignment_error = if (
      any(exact_future_exists)
    ) {
      max(alignment_error, na.rm = TRUE)
    } else {
      NA_real_
    }
  ),
  by = split
]

alignment_report[, split_order :=
                   match(split, c("train", "validation", "test"))]
setorder(alignment_report, split_order)
alignment_report[, split_order := NULL]

cat("\n========== EXACT +1-HOUR ALIGNMENT ==========\n")
print(alignment_report)

fwrite(
  alignment_report,
  "results/exact_1hour_alignment_report.csv"
)

# Recalculate persistence baseline only where exact future
# observations are available
aligned_eval <- df[
  split %in% c("validation", "test") &
    exact_future_exists &
    is.finite(out01h00m) &
    is.finite(nivel_conselheiro),
  .(
    split,
    actual = out01h00m,
    predicted = nivel_conselheiro
  )
]

aligned_baseline_metrics <- aligned_eval[
  ,
  {
    error <- actual - predicted
    
    .(
      n = .N,
      MAE = mean(abs(error)),
      RMSE = sqrt(mean(error^2)),
      Bias = mean(predicted - actual)
    )
  },
  by = split
]

aligned_baseline_metrics[, split_order :=
                           match(split, c("validation", "test"))]
setorder(aligned_baseline_metrics, split_order)
aligned_baseline_metrics[, split_order := NULL]

cat("\n========== ALIGNED PERSISTENCE BASELINE ==========\n")
print(aligned_baseline_metrics)

fwrite(
  aligned_baseline_metrics,
  "results/persistence_baseline_aligned.csv"
)

cat("\nSaved alignment and aligned-baseline reports.\n")







# ============================================
# STEP 8B: LEAKAGE-AWARE MODEL DATA
# ============================================

setDT(df)
setorder(df, datahora)

# Split boundaries
split_ends <- df[
  ,
  .(split_end = max(datahora)),
  by = split
]

# Add exact future timestamp for the one-hour target
df[, target_time := datahora + 3600]

df <- merge(
  df,
  split_ends,
  by = "split",
  all.x = TRUE,
  sort = FALSE
)

setorder(df, datahora)

# A row is eligible only if:
# 1. Its exact future observation exists.
# 2. Its target timestamp remains inside its own split.
df[, eligible := (
  exact_future_exists &
    target_time <= split_end &
    is.finite(out01h00m) &
    is.finite(nivel_conselheiro)
)]

eligibility_report <- df[
  ,
  .(
    total_rows = .N,
    exact_future_rows = sum(exact_future_exists),
    eligible_rows = sum(eligible),
    excluded_rows = sum(!eligible)
  ),
  by = split
]

eligibility_report[, split_order :=
                     match(split, c("train", "validation", "test"))]
setorder(eligibility_report, split_order)
eligibility_report[, split_order := NULL]

cat("\n========== MODEL ELIGIBILITY ==========\n")
print(eligibility_report)

fwrite(
  eligibility_report,
  "results/model_eligibility_report.csv"
)

# Confirm that eligible rows do not cross split boundaries
stopifnot(all(
  df[eligible == TRUE, target_time <= split_end]
))

cat("\nSaved: results/model_eligibility_report.csv\n")

# ============================================
# STEP 8C: RANDOM FOREST
# ============================================

library(ranger)
library(data.table)

# Explicit predictor list
precip_cols <- paste0("precip", 0:503)

predictor_cols <- c(
  precip_cols,
  "chuva_conselheiro",
  "nivel_conselheiro"
)

missing_predictors <- setdiff(predictor_cols, names(df))

if (length(missing_predictors) > 0) {
  stop(
    "Missing predictors: ",
    paste(missing_predictors, collapse = ", ")
  )
}

# Build modelling subsets using the same eligibility rule
train_data <- df[
  split == "train" & eligible == TRUE,
  c("out01h00m", predictor_cols),
  with = FALSE
]

validation_data <- df[
  split == "validation" & eligible == TRUE,
  c("out01h00m", predictor_cols),
  with = FALSE
]

test_data <- df[
  split == "test" & eligible == TRUE,
  c("out01h00m", predictor_cols),
  with = FALSE
]

# Check data quality before fitting
if (anyNA(train_data) ||
    anyNA(validation_data) ||
    anyNA(test_data)) {
  stop(
    "Missing values detected. Resolve using a training-only ",
    "imputation strategy before fitting."
  )
}

stopifnot(
  nrow(train_data) > 0,
  nrow(validation_data) > 0,
  nrow(test_data) > 0
)

cat("\nTraining rows:", nrow(train_data), "\n")
cat("Validation rows:", nrow(validation_data), "\n")
cat("Test rows:", nrow(test_data), "\n")
cat("Number of predictors:", length(predictor_cols), "\n")

# Train using training data only
set.seed(2026)

rf_model <- ranger(
  dependent.variable.name = "out01h00m",
  data = as.data.frame(train_data),
  num.trees = 500,
  mtry = floor(sqrt(length(predictor_cols))),
  min.node.size = 5,
  importance = "impurity",
  num.threads = max(1, parallel::detectCores() - 1),
  seed = 2026
)

cat("\n========== RANDOM FOREST MODEL ==========\n")
print(rf_model)

# EXPLORATORY AUDIT ONLY:
# This earlier block evaluates the test split before the standardized Step 19
# comparison. The test split has therefore been inspected during this workflow.
# Do NOT describe it as untouched or as evaluated only once. Treat these early
# test metrics as exploratory; use a fresh holdout/rolling-origin evaluation
# for a future strictly independent final assessment.
# Predict validation and test.
rf_validation_pred <- predict(
  rf_model,
  data = as.data.frame(
    validation_data[, ..predictor_cols]
  )
)$predictions

rf_test_pred <- predict(
  rf_model,
  data = as.data.frame(
    test_data[, ..predictor_cols]
  )
)$predictions

# Calculate metrics
regression_metrics <- function(actual, predicted) {
  error <- actual - predicted
  
  data.table(
    n = length(actual),
    MAE = mean(abs(error)),
    RMSE = sqrt(mean(error^2)),
    Bias = mean(predicted - actual)
  )
}

rf_metrics <- rbindlist(list(
  cbind(
    data.table(model = "Random Forest", split = "validation"),
    regression_metrics(
      validation_data$out01h00m,
      rf_validation_pred
    )
  ),
  cbind(
    data.table(model = "Random Forest", split = "test"),
    regression_metrics(
      test_data$out01h00m,
      rf_test_pred
    )
  )
))

cat("\n========== RANDOM FOREST METRICS ==========\n")
print(rf_metrics)

fwrite(
  rf_metrics,
  "results/random_forest_metrics.csv"
)

# Save variable importance
importance_table <- data.table(
  feature = names(rf_model$variable.importance),
  importance = as.numeric(rf_model$variable.importance)
)

setorder(importance_table, -importance)

fwrite(
  importance_table,
  "results/random_forest_feature_importance.csv"
)

cat("\nSaved RF metrics and feature importance.\n")







# ============================================
# STEP 9: FAIR MODEL COMPARISON
# ============================================

library(data.table)

# Persistence predictions on exactly the same eligible rows
val_baseline <- df[
  split == "validation" & eligible == TRUE,
  .(
    actual = out01h00m,
    predicted = nivel_conselheiro
  )
]

test_baseline <- df[
  split == "test" & eligible == TRUE,
  .(
    actual = out01h00m,
    predicted = nivel_conselheiro
  )
]

# Shared metric function
calculate_metrics <- function(actual, predicted) {
  error <- actual - predicted
  
  data.table(
    n = length(actual),
    MAE = mean(abs(error)),
    RMSE = sqrt(mean(error^2)),
    Bias = mean(predicted - actual)
  )
}

# Compute metrics for both models on identical rows
comparison <- rbindlist(list(
  cbind(
    data.table(model = "Persistence", split = "validation"),
    calculate_metrics(
      val_baseline$actual,
      val_baseline$predicted
    )
  ),
  cbind(
    data.table(model = "Random Forest", split = "validation"),
    calculate_metrics(
      validation_data$out01h00m,
      rf_validation_pred
    )
  ),
  cbind(
    data.table(model = "Persistence", split = "test"),
    calculate_metrics(
      test_baseline$actual,
      test_baseline$predicted
    )
  ),
  cbind(
    data.table(model = "Random Forest", split = "test"),
    calculate_metrics(
      test_data$out01h00m,
      rf_test_pred
    )
  )
))

cat("\n========== FAIR MODEL COMPARISON ==========\n")
print(comparison)

# Calculate percentage MAE improvement relative to persistence.
# Positive = Random Forest improves; negative = Random Forest is worse.
mae_comparison <- dcast(
  comparison,
  split ~ model,
  value.var = "MAE"
)

mae_comparison[, RF_MAE_improvement_pct :=
                 100 * (Persistence - `Random Forest`) / Persistence
]

cat("\n========== MAE IMPROVEMENT OVER PERSISTENCE ==========\n")
print(mae_comparison)

fwrite(
  comparison,
  "results/fair_model_comparison.csv"
)

fwrite(
  mae_comparison,
  "results/mae_improvement_over_persistence.csv"
)

cat("\nSaved fair comparison reports.\n")




# ============================================
# STEP 10: RANDOM FOREST DIAGNOSTICS
# ============================================

library(data.table)

# Use validation predictions for primary diagnosis
val_diag <- data.table(
  actual = validation_data$out01h00m,
  current_level = validation_data$nivel_conselheiro,
  rf_prediction = as.numeric(rf_validation_pred)
)

val_diag[, persistence_error :=
           actual - current_level]

val_diag[, rf_error :=
           actual - rf_prediction]

val_diag[, abs_level_change :=
           abs(actual - current_level)]

val_diag[, abs_persistence_error :=
           abs(persistence_error)]

val_diag[, abs_rf_error :=
           abs(rf_error)]

cat("\n========== VALIDATION TARGET CHANGE ==========\n")

print(val_diag[, .(
  n = .N,
  mean_abs_level_change = mean(abs_level_change),
  median_abs_level_change = median(abs_level_change),
  p90_abs_level_change = quantile(abs_level_change, 0.90),
  p95_abs_level_change = quantile(abs_level_change, 0.95),
  max_abs_level_change = max(abs_level_change)
)])

cat("\n========== VALIDATION ERROR COMPARISON ==========\n")

print(val_diag[, .(
  persistence_MAE = mean(abs_persistence_error),
  RF_MAE = mean(abs_rf_error),
  persistence_RMSE = sqrt(mean(persistence_error^2)),
  RF_RMSE = sqrt(mean(rf_error^2)),
  RF_mean_bias = mean(rf_prediction - actual)
)])

# Feature importance: descriptive only, not causal explanation
importance_top20 <- copy(importance_table[1:min(20, .N)])

cat("\n========== TOP 20 RF FEATURES ==========\n")
print(importance_top20)

fwrite(
  importance_top20,
  "results/rf_top20_features.csv"
)

# Save row-level validation diagnostics
fwrite(
  val_diag,
  "results/rf_validation_diagnostics.csv"
)

cat("\nSaved validation diagnostics and top-20 features.\n")




# ============================================
# STEP 11: RANDOM FOREST FOR LEVEL CHANGE
# ============================================

library(data.table)
library(ranger)

# Target: change from current to next-hour water level
df[, delta_target := out01h00m - nivel_conselheiro]

# Training and validation rows only
train_delta <- df[
  split == "train" & eligible == TRUE,
  c("delta_target", predictor_cols),
  with = FALSE
]

val_delta <- df[
  split == "validation" & eligible == TRUE,
  c("delta_target", "out01h00m",
    "nivel_conselheiro", predictor_cols),
  with = FALSE
]

stopifnot(
  nrow(train_delta) > 0,
  nrow(val_delta) > 0,
  !anyNA(train_delta),
  !anyNA(val_delta)
)

# Fit on training data only
set.seed(2026)

rf_delta_model <- ranger(
  dependent.variable.name = "delta_target",
  data = as.data.frame(train_delta),
  num.trees = 300,
  mtry = max(1L, floor(sqrt(length(predictor_cols)))),
  min.node.size = 5,
  importance = "impurity",
  num.threads = max(1, parallel::detectCores() - 1),
  seed = 2026
)

# Predict the change on validation rows
delta_prediction <- predict(
  rf_delta_model,
  data = as.data.frame(val_delta[, ..predictor_cols])
)$predictions

# Add predicted change to current level
level_prediction <- val_delta$nivel_conselheiro +
  delta_prediction

actual <- val_delta$out01h00m
persistence_prediction <- val_delta$nivel_conselheiro

# Same validation observations for both methods
validation_comparison <- rbindlist(list(
  cbind(
    data.table(model = "Persistence"),
    calculate_metrics(actual, persistence_prediction)
  ),
  cbind(
    data.table(model = "RF level-change"),
    calculate_metrics(actual, level_prediction)
  )
))

cat("\n========== VALIDATION: LEVEL-CHANGE MODEL ==========\n")
print(validation_comparison)

# Percentage MAE improvement over persistence
persistence_mae <- validation_comparison[
  model == "Persistence", MAE
]

delta_mae <- validation_comparison[
  model == "RF level-change", MAE
]

cat(
  "\nMAE improvement over persistence (%): ",
  round(100 * (persistence_mae - delta_mae) /
          persistence_mae, 2),
  "\n",
  sep = ""
)

fwrite(
  validation_comparison,
  "results/rf_level_change_validation.csv"
)

cat("\nSaved: results/rf_level_change_validation.csv\n")




# ============================================
# STEP 12: RAINFALL FEATURE ABLATION
# ============================================
# Standardized main RF rule: 300 trees, mtry=floor(sqrt(p)),
# min.node.size=5, seed=2026. Calculate mtry separately for each feature set.

library(data.table)
library(ranger)

# Compare three predictor sets
feature_sets <- list(
  Level_only = c("nivel_conselheiro"),
  
  Level_plus_local_rain = c(
    "nivel_conselheiro",
    "chuva_conselheiro"
  ),
  
  All_features = predictor_cols
)

ablation_results <- list()

for (model_name in names(feature_sets)) {
  
  features <- feature_sets[[model_name]]
  
  cat("\nFitting:", model_name, "\n")
  cat("Predictor count:", length(features), "\n")
  
  # Training target is water-level change
  train_subset <- train_delta[
    ,
    c("delta_target", features),
    with = FALSE
  ]
  
  # Fit each model using training data only
  set.seed(2026)
  
  model <- ranger(
    dependent.variable.name = "delta_target",
    data = as.data.frame(train_subset),
    num.trees = 300,
    mtry = max(1, floor(sqrt(length(features)))),
    min.node.size = 5,
    importance = "none",
    num.threads = max(1, parallel::detectCores() - 1),
    seed = 2026
  )
  
  # Validation predictions
  predicted_delta <- predict(
    model,
    data = as.data.frame(val_delta[, ..features])
  )$predictions
  
  predicted_level <- val_delta$nivel_conselheiro +
    predicted_delta
  
  # Evaluate on identical validation observations
  metrics <- calculate_metrics(
    val_delta$out01h00m,
    predicted_level
  )
  
  metrics[, model := model_name]
  metrics[, predictors := length(features)]
  
  ablation_results[[model_name]] <- metrics
  
  rm(model, train_subset, predicted_delta, predicted_level)
  gc()
}

# Add persistence baseline as reference
persistence_metrics <- calculate_metrics(
  val_delta$out01h00m,
  val_delta$nivel_conselheiro
)
persistence_metrics[, `:=`(
  model = "Persistence",
  predictors = 1L
)]

ablation_table <- rbindlist(c(
  list(persistence_metrics),
  ablation_results
), use.names = TRUE, fill = TRUE)

setcolorder(
  ablation_table,
  c("model", "predictors", "n", "MAE", "RMSE", "Bias")
)

cat("\n========== VALIDATION FEATURE ABLATION ==========\n")
print(ablation_table)

fwrite(
  ablation_table,
  "results/validation_feature_ablation.csv"
)

cat("\nSaved: results/validation_feature_ablation.csv\n")




# ============================================
# STEP 13: RAINFALL FEATURE OUTAGE ROBUSTNESS
# ============================================

library(data.table)
library(ranger)

# Identify rainfall-grid features only
rain_features <- grep(
  "^precip[0-9]+$",
  predictor_cols,
  value = TRUE
)

stopifnot(length(rain_features) == 504)
stopifnot(all(rain_features %in% names(train_delta)))
stopifnot(all(rain_features %in% names(val_delta)))

# Training-only medians for imputation
train_medians <- vapply(
  train_delta[, ..rain_features],
  median,
  numeric(1),
  na.rm = TRUE
)

# Two models: compact model and full-feature model
feature_sets <- list(
  Level_plus_local_rain = c(
    "nivel_conselheiro",
    "chuva_conselheiro"
  ),
  All_features = predictor_cols
)

models <- list()

for (model_name in names(feature_sets)) {
  
  features <- feature_sets[[model_name]]
  
  cat("\nTraining model:", model_name, "\n")
  
  train_subset <- train_delta[
    ,
    c("delta_target", features),
    with = FALSE
  ]
  
  set.seed(2026)
  
  models[[model_name]] <- ranger(
    dependent.variable.name = "delta_target",
    data = as.data.frame(train_subset),
    num.trees = 300,
    mtry = max(1, floor(sqrt(length(features)))),
    min.node.size = 5,
    importance = "none",
    num.threads = max(
      1,
      parallel::detectCores() - 1
    ),
    seed = 2026
  )
  
  rm(train_subset)
  gc()
}

# Outage levels: fraction of rainfall-grid columns unavailable
outage_rates <- c(0, 0.10, 0.20, 0.40)
mask_seeds <- c(2026, 2027, 2028)

results <- list()
result_index <- 1L

for (model_name in names(feature_sets)) {
  
  features <- feature_sets[[model_name]]
  model <- models[[model_name]]
  
  for (rate in outage_rates) {
    
    for (mask_seed in mask_seeds) {
      
      # Copy validation predictors; do not modify original data
      validation_x <- copy(
        val_delta[, ..features]
      )
      
      masked_features <- character(0)
      
      # Mask rainfall-grid columns only
      if (rate > 0) {
        
        set.seed(mask_seed)
        
        n_mask <- ceiling(
          rate * length(rain_features)
        )
        
        masked_features <- sample(
          rain_features,
          size = n_mask,
          replace = FALSE
        )
        
        # Only columns used by this model
        masked_features <- intersect(
          masked_features,
          features
        )
        
        # Training-only median imputation
        for (feature in masked_features) {
          set(
            validation_x,
            j = feature,
            value = rep(
              train_medians[[feature]],
              nrow(validation_x)
            )
          )
        }
      }
      
      predicted_delta <- predict(
        model,
        data = as.data.frame(validation_x)
      )$predictions
      
      predicted_level <-
        val_delta$nivel_conselheiro +
        predicted_delta
      
      metrics <- calculate_metrics(
        val_delta$out01h00m,
        predicted_level
      )
      
      metrics[, `:=`(
        model = model_name,
        outage_fraction = rate,
        masked_grid_features = length(masked_features),
        mask_seed = mask_seed
      )]
      
      results[[result_index]] <- metrics
      result_index <- result_index + 1L
      
      cat(
        model_name,
        "| outage =", rate,
        "| seed =", mask_seed,
        "| MAE =", round(metrics$MAE, 6),
        "| RMSE =", round(metrics$RMSE, 6),
        "\n"
      )
      
      rm(validation_x, predicted_delta, predicted_level)
    }
  }
}

robustness_results <- rbindlist(
  results,
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  robustness_results,
  "results/rainfall_outage_robustness_raw.csv"
)

# Average across the three masks at each outage level
robustness_summary <- robustness_results[
  ,
  .(
    n = first(n),
    mean_MAE = mean(MAE),
    sd_MAE = sd(MAE),
    mean_RMSE = mean(RMSE),
    sd_RMSE = sd(RMSE),
    mean_Bias = mean(Bias)
  ),
  by = .(
    model,
    outage_fraction,
    masked_grid_features
  )
][order(model, outage_fraction)]

cat("\n========== OUTAGE ROBUSTNESS SUMMARY ==========\n")
print(robustness_summary)

fwrite(
  robustness_summary,
  "results/rainfall_outage_robustness_summary.csv"
)

cat("\nSaved files:\n")
cat("- results/rainfall_outage_robustness_raw.csv\n")
cat("- results/rainfall_outage_robustness_summary.csv\n")






# Check available columns and objects
cat("val_delta columns:\n")
print(names(val_delta))

cat("\nValidation object dimensions:\n")
print(dim(val_delta))

cat("\nDoes df contain datahora?\n")
print(exists("df"))

if (exists("df")) {
  print("datahora" %in% names(df))
  print(names(df)[grepl(
    "datahora|timestamp|time|date",
    names(df),
    ignore.case = TRUE
  )])
}

cat("\nOther relevant objects:\n")
print(intersect(
  c("df", "val_delta", "validation_data", "test_data"),
  ls()
))





library(data.table)

# 1. Confirm required columns
stopifnot(all(c(
  "datahora", "delta_target", "out01h00m",
  "nivel_conselheiro", "eligible", "split"
) %in% names(df)))

# 2. Reconstruct eligible validation rows in chronological order
val_rows <- copy(df[
  eligible == TRUE & split == "validation"
])

setorder(val_rows, datahora)

# 3. Check row counts before restoring timestamps
cat("val_delta rows:", nrow(val_delta), "\n")
cat("Eligible validation rows:", nrow(val_rows), "\n")

stopifnot(nrow(val_delta) == nrow(val_rows))

# 4. Verify values align before attaching timestamps
stopifnot(isTRUE(all.equal(
  val_delta$out01h00m,
  val_rows$out01h00m,
  check.attributes = FALSE
)))

stopifnot(isTRUE(all.equal(
  val_delta$nivel_conselheiro,
  val_rows$nivel_conselheiro,
  check.attributes = FALSE
)))

stopifnot(isTRUE(all.equal(
  val_delta$delta_target,
  val_rows$delta_target,
  check.attributes = FALSE
)))

# 5. Restore timestamps only after verification
val_delta[, datahora := val_rows$datahora]

# 6. Sort and audit the restored timeline
setorder(val_delta, datahora)

time_diffs <- as.numeric(
  diff(val_delta$datahora),
  units = "secs"
) / 60

cat("\n========== VALIDATION TIMELINE ==========\n")
cat("Rows:", nrow(val_delta), "\n")
cat("Start:", as.character(min(val_delta$datahora)), "\n")
cat("End:", as.character(max(val_delta$datahora)), "\n")
cat("Duplicate timestamps:", sum(duplicated(val_delta$datahora)), "\n")
cat("Intervals exactly 30 min:", sum(time_diffs == 30), "\n")
cat("Intervals above 30 min:", sum(time_diffs > 30), "\n")
cat("Maximum interval (min):", max(time_diffs), "\n")

fwrite(
  data.table(
    validation_rows = nrow(val_delta),
    start_time = as.character(min(val_delta$datahora)),
    end_time = as.character(max(val_delta$datahora)),
    duplicate_timestamps = sum(duplicated(val_delta$datahora)),
    intervals_over_30min = sum(time_diffs > 30),
    maximum_gap_minutes = max(time_diffs)
  ),
  "results/validation_timeline_audit.csv"
)




# ============================================
# STEP 14B: CONTIGUOUS-TIME OUTAGE WINDOWS
# ============================================

library(data.table)
library(ranger)

stopifnot(exists("models"))
stopifnot("All_features" %in% names(models))
stopifnot("datahora" %in% names(val_delta))

# Rainfall-grid columns
rain_features <- grep(
  "^precip[0-9]+$",
  predictor_cols,
  value = TRUE
)

stopifnot(length(rain_features) == 504)

# Recalculate medians from training data only
train_medians <- vapply(
  train_delta[, ..rain_features],
  median,
  numeric(1),
  na.rm = TRUE
)

# Sort and prepare validation timeline
val_outage <- copy(val_delta)
setorder(val_outage, datahora)

# Fixed model trained in Step 14B's preceding step
model <- models[["All_features"]]
features <- predictor_cols

# Create candidate outage windows inside validation period.
# Each duration is measured in elapsed clock time.
window_hours <- c(24, 72, 168)

# Choose deterministic start points away from the validation edges
validation_start <- min(val_outage$datahora)
validation_end <- max(val_outage$datahora)

candidate_starts <- validation_start + c(
  7, 21, 35, 49, 63, 77, 91, 105, 119, 133
) * 24 * 60 * 60

candidate_starts <- candidate_starts[
  candidate_starts + max(window_hours) * 3600 <= validation_end
]

# Use a fixed subset of 20% rainfall-grid features for each window.
# This makes duration comparisons more interpretable.
set.seed(2026)
masked_features <- sample(
  rain_features,
  size = ceiling(0.20 * length(rain_features)),
  replace = FALSE
)

outage_results <- list()
idx <- 1L

for (start_time in candidate_starts) {
  
  for (duration in window_hours) {
    
    end_time <- start_time + duration * 3600
    
    window_rows <- which(
      val_outage$datahora >= start_time &
        val_outage$datahora < end_time
    )
    
    if (length(window_rows) < 2) next
    
    # Use only observations that actually exist in the window
    window_data <- val_outage[window_rows]
    
    x_normal <- copy(window_data[, ..features])
    x_outage <- copy(window_data[, ..features])
    
    # Replace selected rainfall features with training medians
    for (feature in masked_features) {
      set(
        x_outage,
        j = feature,
        value = rep(
          train_medians[[feature]],
          nrow(x_outage)
        )
      )
    }
    
    # Predict water-level change, then reconstruct water level
    pred_normal_delta <- predict(
      model,
      data = as.data.frame(x_normal)
    )$predictions
    
    pred_outage_delta <- predict(
      model,
      data = as.data.frame(x_outage)
    )$predictions
    
    pred_normal <- window_data$nivel_conselheiro +
      pred_normal_delta
    
    pred_outage <- window_data$nivel_conselheiro +
      pred_outage_delta
    
    normal_metrics <- calculate_metrics(
      window_data$out01h00m,
      pred_normal
    )
    
    outage_metrics <- calculate_metrics(
      window_data$out01h00m,
      pred_outage
    )
    
    outage_results[[idx]] <- data.table(
      start_time = as.character(start_time),
      end_time = as.character(end_time),
      duration_hours = duration,
      observations = nrow(window_data),
      masked_features = length(masked_features),
      normal_MAE = normal_metrics$MAE,
      outage_MAE = outage_metrics$MAE,
      normal_RMSE = normal_metrics$RMSE,
      outage_RMSE = outage_metrics$RMSE,
      MAE_change_pct = if (normal_metrics$MAE > 0) {
        100 * (outage_metrics$MAE - normal_metrics$MAE) /
          normal_metrics$MAE
      } else {
        NA_real_
      },
      RMSE_change_pct = if (normal_metrics$RMSE > 0) {
        100 * (outage_metrics$RMSE - normal_metrics$RMSE) /
          normal_metrics$RMSE
      } else {
        NA_real_
      }
    )
    
    idx <- idx + 1L
    
    rm(
      x_normal, x_outage,
      pred_normal_delta, pred_outage_delta,
      pred_normal, pred_outage
    )
  }
}

outage_window_table <- rbindlist(
  outage_results,
  use.names = TRUE,
  fill = TRUE
)

cat("\n========== CONTIGUOUS OUTAGE RESULTS ==========\n")
print(outage_window_table)

fwrite(
  outage_window_table,
  "results/contiguous_outage_window_results.csv"
)

cat("\nSaved: results/contiguous_outage_window_results.csv\n")



library(data.table)

outage_window_table <- fread(
  "results/contiguous_outage_window_results.csv"
)

outage_window_table[, start_datetime :=
                      as.POSIXct(
                        as.numeric(start_time),
                        origin = "1970-01-01",
                        tz = "UTC"
                      )
]

outage_window_table[, end_datetime :=
                      as.POSIXct(
                        as.numeric(end_time),
                        origin = "1970-01-01",
                        tz = "UTC"
                      )
]

print(
  outage_window_table[
    ,
    .(
      start_datetime,
      end_datetime,
      duration_hours,
      observations,
      normal_MAE,
      outage_MAE,
      MAE_change_pct,
      normal_RMSE,
      outage_RMSE,
      RMSE_change_pct
    )
  ]
)

fwrite(
  outage_window_table,
  "results/contiguous_outage_window_results_readable.csv"
)




library(data.table)

# Load the saved outage results
outage_window_table <- fread(
  "results/contiguous_outage_window_results.csv"
)

# Restore timestamps from Unix seconds
outage_window_table[, start_datetime :=
                      as.POSIXct(
                        as.numeric(start_time),
                        origin = "1970-01-01",
                        tz = "UTC"
                      )
]

outage_window_table[, end_datetime :=
                      as.POSIXct(
                        as.numeric(end_time),
                        origin = "1970-01-01",
                        tz = "UTC"
                      )
]

# Prepare the validation observations
stopifnot(exists("val_delta"))
stopifnot("datahora" %in% names(val_delta))

val_timeline <- copy(val_delta)
setorder(val_timeline, datahora)

coverage_results <- vector(
  "list",
  nrow(outage_window_table)
)

for (i in seq_len(nrow(outage_window_table))) {
  
  start_time <- outage_window_table$start_datetime[i]
  end_time   <- outage_window_table$end_datetime[i]
  duration   <- outage_window_table$duration_hours[i]
  
  # Only observations that actually exist in this window
  window_data <- val_timeline[
    datahora >= start_time & datahora < end_time
  ]
  
  timestamps <- sort(unique(window_data$datahora))
  
  # A complete 30-minute series has 2 observations per hour
  expected_observations <- duration * 2
  
  if (length(timestamps) >= 2) {
    gaps_minutes <- as.numeric(
      diff(timestamps),
      units = "mins"
    )
    
    # Number of missing 30-minute slots across observed gaps
    missing_slots <- sum(
      pmax(round(gaps_minutes / 30) - 1, 0)
    )
    
    intervals_30min <- sum(
      abs(gaps_minutes - 30) < 1e-8
    )
    
    max_gap_minutes <- max(gaps_minutes)
  } else {
    missing_slots <- NA_integer_
    intervals_30min <- 0L
    max_gap_minutes <- NA_real_
  }
  
  coverage_results[[i]] <- data.table(
    window_id = i,
    start_datetime = as.character(start_time),
    duration_hours = duration,
    expected_observations = expected_observations,
    actual_observations = nrow(window_data),
    coverage_pct = 100 * nrow(window_data) /
      expected_observations,
    missing_observations_vs_expected =
      expected_observations - nrow(window_data),
    missing_30min_slots_between_observations =
      missing_slots,
    exact_30min_intervals = intervals_30min,
    max_gap_minutes = max_gap_minutes
  )
}

coverage_audit <- rbindlist(coverage_results)

cat("\n========== COVERAGE AUDIT ==========\n")
print(coverage_audit)

cat("\n========== SUMMARY BY DURATION ==========\n")

coverage_summary <- coverage_audit[
  ,
  .(
    windows = .N,
    mean_coverage_pct = mean(coverage_pct),
    min_coverage_pct = min(coverage_pct),
    mean_observations = mean(actual_observations),
    windows_below_90pct = sum(coverage_pct < 90),
    windows_below_75pct = sum(coverage_pct < 75)
  ),
  by = duration_hours
]

print(coverage_summary[order(duration_hours)])

fwrite(
  coverage_audit,
  "results/contiguous_outage_coverage_audit.csv"
)

fwrite(
  coverage_summary,
  "results/contiguous_outage_coverage_summary.csv"
)

cat("\nSaved both coverage audit files in results/.\n")






library(data.table)

coverage_audit <- fread(
  "results/contiguous_outage_coverage_audit.csv"
)

outage_metrics <- fread(
  "results/contiguous_outage_window_results.csv"
)

# Join using the existing window order/ID
stopifnot(nrow(coverage_audit) == nrow(outage_metrics))

outage_metrics[, window_id := seq_len(.N)]
coverage_audit[, window_id := seq_len(.N)]

comparison <- merge(
  outage_metrics,
  coverage_audit[
    ,
    .(
      window_id,
      coverage_pct,
      max_gap_minutes,
      actual_observations
    )
  ],
  by = "window_id"
)

# Primary descriptive subset: at least 90% coverage
well_covered <- comparison[coverage_pct >= 90]

cat("\n===== WELL-COVERED WINDOWS =====\n")
print(
  well_covered[
    ,
    .(
      window_id,
      duration_hours,
      coverage_pct,
      actual_observations,
      max_gap_minutes,
      MAE_change_pct,
      RMSE_change_pct
    )
  ][order(duration_hours, window_id)]
)

cat("\n===== SUMMARY BY DURATION =====\n")

if (nrow(well_covered) > 0) {
  well_covered_summary <- well_covered[
    ,
    .(
      windows = .N,
      median_MAE_change_pct = median(MAE_change_pct),
      mean_MAE_change_pct = mean(MAE_change_pct),
      median_RMSE_change_pct = median(RMSE_change_pct),
      mean_RMSE_change_pct = mean(RMSE_change_pct),
      windows_with_MAE_increase = sum(MAE_change_pct > 0),
      windows_with_RMSE_increase = sum(RMSE_change_pct > 0)
    ),
    by = duration_hours
  ]
  
  print(well_covered_summary[order(duration_hours)])
  
  fwrite(
    well_covered,
    "results/contiguous_outage_well_covered_windows.csv"
  )
  
  fwrite(
    well_covered_summary,
    "results/contiguous_outage_well_covered_summary.csv"
  )
} else {
  cat("No windows meet the coverage criterion.\n")
}







library(data.table)

cat("\n===== STEP 15A: OUTAGE EXPERIMENT AUDIT =====\n")

# 1. Confirm output files exist
files_to_check <- c(
  "results/rainfall_outage_robustness_raw.csv",
  "results/rainfall_outage_robustness_summary.csv",
  "results/contiguous_outage_window_results.csv",
  "results/contiguous_outage_coverage_audit.csv",
  "results/contiguous_outage_well_covered_summary.csv"
)

file_status <- data.table(
  file = files_to_check,
  exists = file.exists(files_to_check)
)
print(file_status)

# 2. Inspect the random masking results
if (file.exists(files_to_check[1])) {
  mask_raw <- fread(files_to_check[1])
  cat("\n--- Random masking: columns and dimensions ---\n")
  print(dim(mask_raw))
  print(names(mask_raw))
  print(head(mask_raw, 8))
}

# 3. Inspect contiguous outage results
if (file.exists(files_to_check[3])) {
  window_raw <- fread(files_to_check[3])
  cat("\n--- Contiguous outage: columns and dimensions ---\n")
  print(dim(window_raw))
  print(names(window_raw))
  print(head(window_raw, 5))
}

# 4. Check whether important model objects are still in memory
objects_to_check <- c(
  "models",
  "predictor_cols",
  "train_data",
  "validation_data",
  "test_data",
  "train_medians",
  "val_delta"
)

object_status <- data.table(
  object = objects_to_check,
  available = vapply(
    objects_to_check,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = FALSE
  )
)
cat("\n--- R session objects ---\n")
print(object_status)

# 5. If predictor columns remain available, inspect feature groups
if (exists("predictor_cols", envir = .GlobalEnv, inherits = FALSE)) {
  predictors <- get("predictor_cols", envir = .GlobalEnv)
  
  cat("\n--- Predictor checks ---\n")
  cat("Total predictors:", length(predictors), "\n")
  cat("Rainfall-grid predictors:",
      sum(grepl("^precip[0-9]+$", predictors)), "\n")
  cat("Current water level included:",
      "nivel_conselheiro" %in% predictors, "\n")
  cat("Local rainfall included:",
      "chuva_conselheiro" %in% predictors, "\n")
}

cat("\n===== END STEP 15A =====\n")




library(data.table)

cat("\n===== STEP 15B: STRUCTURAL VALIDATION =====\n")

# A. Validate random masking experiment
mask_raw <- fread("results/rainfall_outage_robustness_raw.csv")

cat("\n--- A1. Expected experiment design ---\n")
cat("Rows:", nrow(mask_raw), "\n")
cat("Models:", paste(sort(unique(mask_raw$model)), collapse = ", "), "\n")
cat("Mask rates:", paste(sort(unique(mask_raw$outage_fraction)), collapse = ", "), "\n")
cat("Mask seeds:", paste(sort(unique(mask_raw$mask_seed)), collapse = ", "), "\n")

# Expected grid features masked: ceiling(rate * 504)
mask_raw[, expected_masked := ceiling(outage_fraction * 504)]

mask_raw[, mask_count_matches := (
  masked_grid_features == expected_masked
)]

cat("\nMask-count mismatches:",
    sum(!mask_raw$mask_count_matches), "\n")

print(
  mask_raw[
    mask_count_matches == FALSE,
    .(model, outage_fraction, mask_seed,
      masked_grid_features, expected_masked)
  ]
)

# B. Check the grid-free model is unchanged across masking settings
simple_model <- mask_raw[model == "Level_plus_local_rain"]

simple_model_check <- simple_model[
  , .(
    rows = .N,
    unique_MAE = uniqueN(MAE),
    unique_RMSE = uniqueN(RMSE),
    unique_Bias = uniqueN(Bias),
    unique_masked_feature_counts = uniqueN(masked_grid_features)
  ),
  by = outage_fraction
]

cat("\n--- B. Grid-free model invariance ---\n")
print(simple_model_check)

# C. Check full-feature model's no-mask results across seeds
full_model <- mask_raw[model == "All_features"]

full_zero <- full_model[outage_fraction == 0]

cat("\n--- C. Full model, zero masking ---\n")
print(
  full_zero[
    , .(
      rows = .N,
      unique_MAE = uniqueN(MAE),
      unique_RMSE = uniqueN(RMSE),
      unique_Bias = uniqueN(Bias),
      unique_masked_feature_counts = uniqueN(masked_grid_features)
    )
  ]
)

# D. Validate contiguous-window percentage calculations
window_raw <- fread("results/contiguous_outage_window_results.csv")

window_raw[, recomputed_MAE_change :=
             (outage_MAE - normal_MAE) / normal_MAE * 100]

window_raw[, recomputed_RMSE_change :=
             (outage_RMSE - normal_RMSE) / normal_RMSE * 100]

window_raw[, MAE_formula_error :=
             abs(MAE_change_pct - recomputed_MAE_change)]

window_raw[, RMSE_formula_error :=
             abs(RMSE_change_pct - recomputed_RMSE_change)]

cat("\n--- D. Percentage calculation checks ---\n")
cat("Maximum MAE percentage discrepancy:",
    max(window_raw$MAE_formula_error, na.rm = TRUE), "\n")
cat("Maximum RMSE percentage discrepancy:",
    max(window_raw$RMSE_formula_error, na.rm = TRUE), "\n")

cat("\n--- Window counts and masked-feature counts ---\n")
print(
  window_raw[
    , .(
      windows = .N,
      min_observations = min(observations),
      max_observations = max(observations),
      unique_durations = uniqueN(duration_hours),
      unique_masked_feature_counts = uniqueN(masked_features)
    )
  ]
)

# E. Compare window observations with coverage audit
coverage_audit <- fread("results/contiguous_outage_coverage_audit.csv")

cat("\n--- E. Coverage alignment checks ---\n")
cat("Same number of windows:",
    nrow(window_raw) == nrow(coverage_audit), "\n")

if (nrow(window_raw) == nrow(coverage_audit)) {
  cat("Observation-count mismatches:",
      sum(window_raw$observations !=
            coverage_audit$actual_observations), "\n")
}

cat("\n===== END STEP 15B =====\n")






library(data.table)

cat("\n===== STEP 15C: MODEL AND SCRIPT AUDIT =====\n")

# 1. Inspect model names and classes
cat("\n--- Model objects ---\n")
print(names(models))

for (nm in names(models)) {
  cat("\nModel:", nm, "\n")
  cat("Class:", paste(class(models[[nm]]), collapse = ", "), "\n")
  
  # ranger models usually store their fitted predictor names here
  if (!is.null(models[[nm]]$forest$independent.variable.names)) {
    fitted_features <- models[[nm]]$forest$independent.variable.names
    
    cat("Fitted predictor count:", length(fitted_features), "\n")
    cat("Rainfall-grid count:",
        sum(grepl("^precip[0-9]+$", fitted_features)), "\n")
    cat("Includes current level:",
        "nivel_conselheiro" %in% fitted_features, "\n")
    cat("Includes local rainfall:",
        "chuva_conselheiro" %in% fitted_features, "\n")
  } else {
    cat("Fitted predictor names not found in the expected ranger field.\n")
  }
}

# 2. Correctly validate the recorded masking counts by model
mask_raw <- fread("results/rainfall_outage_robustness_raw.csv")

mask_raw[, expected_masked := fifelse(
  model == "All_features",
  ceiling(outage_fraction * 504),
  0L
)]

mask_raw[, mask_count_matches := (
  masked_grid_features == expected_masked
)]

cat("\n--- Mask counts checked against model design ---\n")
print(
  mask_raw[
    , .(
      rows = .N,
      mismatches = sum(!mask_count_matches)
    ),
    by = model
  ]
)

# 3. List available R scripts
cat("\n--- R scripts in project folders ---\n")

script_files <- list.files(
  path = ".",
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)

print(script_files)

# 4. Search scripts for masking and prediction logic
if (length(script_files) > 0) {
  patterns <- paste(
    "outage",
    "mask",
    "median",
    "predict\\(",
    "precip",
    "All_features",
    "Level_plus_local_rain",
    sep = "|"
  )
  
  for (f in script_files) {
    lines <- tryCatch(
      readLines(f, warn = FALSE),
      error = function(e) character()
    )
    
    hits <- grep(patterns, lines, ignore.case = TRUE)
    
    if (length(hits) > 0) {
      cat("\n--- Relevant lines from:", f, "---\n")
      
      # Print matching lines with nearby context
      selected <- sort(unique(unlist(
        lapply(hits, function(i) {
          seq(max(1, i - 2), min(length(lines), i + 2))
        })
      )))
      
      cat(
        paste(
          sprintf("%4d: %s", selected, lines[selected]),
          collapse = "\n"
        ),
        "\n"
      )
    }
  }
}

cat("\n===== END STEP 15C =====\n")




library(data.table)

cat("\n===== STEP 16: TEMPORAL LEAKAGE AUDIT =====\n")

# 1. Confirm the data object and required columns
cat("\n--- Available data objects ---\n")
print(
  Filter(
    function(x) is.data.frame(get(x, envir = .GlobalEnv)) ||
      data.table::is.data.table(get(x, envir = .GlobalEnv)),
    ls(envir = .GlobalEnv)
  )
)

if (!exists("df", envir = .GlobalEnv, inherits = FALSE)) {
  stop("Object 'df' not found. Tell me which object contains the original CSV data.")
}

cat("\n--- Dataset dimensions ---\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

required_cols <- c(
  "datahora", "nivel_conselheiro",
  "chuva_conselheiro", "out01h00m",
  "precip0", "precip503"
)

cat("\n--- Required columns present ---\n")
print(data.table(
  column = required_cols,
  present = required_cols %in% names(df)
))

stopifnot(all(required_cols %in% names(df)))

# 2. Inspect predictor values at their recorded timestamps
cat("\n--- Sample rows ---\n")
print(
  df[
    seq_len(min(10L, nrow(df))),
    .(
      datahora,
      nivel_conselheiro,
      chuva_conselheiro,
      precip0,
      precip503,
      out01h00m
    )
  ]
)

# 3. Check whether target exactly matches level one hour later
# Use the existing timestamp-alignment report if available.
alignment_path <- "results/target_alignment_report.csv"

cat("\n--- Existing target alignment report ---\n")
if (file.exists(alignment_path)) {
  print(fread(alignment_path))
} else {
  cat("Alignment report not found at:", alignment_path, "\n")
}

# 4. Check timestamps and predictor missingness
cat("\n--- Timestamp class and missingness ---\n")
print(class(df$datahora))
cat("Missing timestamps:", sum(is.na(df$datahora)), "\n")

predictor_cols <- c(
  grep("^precip[0-9]+$", names(df), value = TRUE),
  "chuva_conselheiro",
  "nivel_conselheiro"
)

cat("Number of candidate predictors:", length(predictor_cols), "\n")
cat("Rows with any missing candidate predictor:",
    sum(!complete.cases(df[, ..predictor_cols])), "\n")

# 5. Check saved project documentation and scripts
cat("\n--- Project files that may document data provenance ---\n")
all_files <- list.files(
  ".",
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)

print(
  all_files[
    grepl(
      "readme|metadata|description|source|data|\\.txt$|\\.md$|\\.pdf$|\\.R$",
      basename(all_files),
      ignore.case = TRUE
    )
  ]
)

cat("\n===== END STEP 16 =====\n")






cat("\n===== STEP 16B: DATA PROVENANCE =====\n")

# 1. Inspect the existing data-audit script
script_path <- "01_data_audit.R"

if (file.exists(script_path)) {
  script_lines <- readLines(script_path, warn = FALSE)
  
  cat("\n--- 01_data_audit.R: first 180 lines ---\n")
  cat(
    paste(
      sprintf(
        "%3d: %s",
        seq_len(min(180L, length(script_lines))),
        head(script_lines, 180)
      ),
      collapse = "\n"
    ),
    "\n"
  )
}

# 2. List files in the original dataset directory
dataset_dir <- "../dataset"

cat("\n--- Candidate dataset directory ---\n")
cat("Path:", normalizePath(dataset_dir, mustWork = FALSE), "\n")
cat("Exists:", dir.exists(dataset_dir), "\n")

if (dir.exists(dataset_dir)) {
  print(
    data.table::as.data.table(
      file.info(list.files(
        dataset_dir,
        full.names = TRUE,
        recursive = FALSE,
        all.files = TRUE
      )),
      keep.rownames = "full_path"
    )[, .(full_path, size, isdir, mtime)]
  )
}

# 3. Search project and nearby folders for documentation
search_roots <- c(".", "..")

for (root in search_roots) {
  if (dir.exists(root)) {
    candidates <- list.files(
      root,
      recursive = TRUE,
      full.names = TRUE,
      all.files = FALSE
    )
    
    docs <- candidates[
      grepl(
        "readme|metadata|documentation|dataset|source|license|citation|\\.pdf$|\\.md$|\\.txt$|\\.html$",
        basename(candidates),
        ignore.case = TRUE
      ) &
        !file.info(candidates)$isdir
    ]
    
    cat("\n--- Documentation candidates under", root, "---\n")
    print(head(docs, 100))
  }
}

cat("\n===== END STEP 16B =====\n")




cat("\n===== STEP 16C: ZIP CONTENTS AND HEADER CHECK =====\n")

zip_path <- "../dataset.zip"

cat("ZIP exists:", file.exists(zip_path), "\n")

if (file.exists(zip_path)) {
  zip_contents <- utils::unzip(zip_path, list = TRUE)
  
  cat("\n--- ZIP file contents (first 100 entries) ---\n")
  print(head(zip_contents, 100))
  
  cat("\n--- Files that may contain source/metadata information ---\n")
  print(
    zip_contents[
      grepl(
        "readme|metadata|source|citation|license|documentation|\\.pdf$|\\.md$|\\.txt$|\\.html$",
        zip_contents$Name,
        ignore.case = TRUE
      ),
      ,
      drop = FALSE
    ]
  )
}

cat("\n--- CSV header and sample timestamps ---\n")

csv_path <- file.path(
  "../dataset",
  "bacia_rio_bengalas_2019_2023_out01h00m.csv"
)

if (file.exists(csv_path)) {
  header <- data.table::fread(
    csv_path,
    nrows = 5,
    showProgress = FALSE
  )
  
  print(names(header))
  print(head(header[, .(datahora, chuva_conselheiro,
                        nivel_conselheiro, out01h00m)], 5))
}

cat("\n===== END STEP 16C =====\n")



cat("\n===== STEP 16D: EXTRA FEATURE AUDIT =====\n")

grid_cols <- grep("^precip[0-9]+$", names(train_data), value = TRUE)

cat("Rainfall grid columns:", length(grid_cols), "\n")
cat("Local rainfall present:", "chuva_conselheiro" %in% names(train_data), "\n")
cat("Current water level present:", "nivel_conselheiro" %in% names(train_data), "\n")
cat("Target present:", "out01h00m" %in% names(train_data), "\n")

# Compare local rainfall with the 504 grid cells
if ("chuva_conselheiro" %in% names(train_data) && length(grid_cols) > 0) {
  
  local_rain <- train_data$chuva_conselheiro
  
  correlations <- vapply(
    grid_cols,
    function(col) {
      suppressWarnings(
        cor(local_rain, train_data[[col]], use = "complete.obs")
      )
    },
    numeric(1)
  )
  
  correlations <- sort(correlations, decreasing = TRUE)
  
  cat("\nTop 10 grid cells correlated with chuva_conselheiro:\n")
  print(head(correlations, 10))
  
  cat("\nLocal rainfall summary:\n")
  print(summary(local_rain))
  
  cat("\nProportion of zero local-rain observations:\n")
  print(mean(local_rain == 0, na.rm = TRUE))
}

cat("\n===== END STEP 16D =====\n")




cat("\n===== DEBUG: train_delta / val_delta =====\n")

cat("\ntrain_delta class:\n")
print(class(train_delta))

cat("\ntrain_delta dimensions:\n")
print(dim(train_delta))

cat("\ntrain_delta column names:\n")
print(names(train_delta))

cat("\nIs delta_target present?\n")
print("delta_target" %in% names(train_delta))

cat("\nval_delta dimensions:\n")
print(dim(val_delta))

cat("\nval_delta column names (first 15):\n")
print(head(names(val_delta), 15))

cat("\n===== END DEBUG =====\n")



cat("\n===== STEP 16E: AUTHORS-ALIGNED MODEL (FIXED) =====\n")

# Authors-aligned predictors: 504 grid cells + current water level
authors_predictors <- c(grid_cols, "nivel_conselheiro")

# Convert explicitly to data.frame before selecting columns
train_authors <- as.data.frame(train_delta)
val_authors <- as.data.frame(val_delta)

stopifnot("delta_target" %in% names(train_authors))
stopifnot(all(authors_predictors %in% names(train_authors)))
stopifnot(all(authors_predictors %in% names(val_authors)))
stopifnot(all(c("out01h00m", "nivel_conselheiro") %in%
                names(val_authors)))

# Fit on training data only.
# Use the same RF hyperparameter rule as the other main delta-target models.
authors_mtry <- max(1L, floor(sqrt(length(authors_predictors))))

model_authors_aligned <- ranger::ranger(
  dependent.variable.name = "delta_target",
  data = train_authors[, c("delta_target", authors_predictors),
                       drop = FALSE],
  num.trees = 300,
  mtry = authors_mtry,
  min.node.size = 5,
  seed = 2026,
  importance = "none",
  num.threads = max(1, parallel::detectCores() - 1)
)

# Predict change, then reconstruct future water level
pred_delta_authors <- predict(
  model_authors_aligned,
  data = val_authors[, authors_predictors, drop = FALSE]
)$predictions

pred_level_authors <- val_authors$nivel_conselheiro +
  pred_delta_authors

actual_level <- val_authors$out01h00m
persistence <- val_authors$nivel_conselheiro

metric_row <- function(name, pred, actual) {
  err <- pred - actual
  data.frame(
    model = name,
    n = length(actual),
    MAE = mean(abs(err)),
    RMSE = sqrt(mean(err^2)),
    Bias = mean(err)
  )
}

# Existing augmented model, evaluated on the same validation rows
pred_delta_augmented <- predict(
  models[["All_features"]],
  data = val_authors[, predictor_cols, drop = FALSE]
)$predictions

pred_level_augmented <- val_authors$nivel_conselheiro +
  pred_delta_augmented

comparison <- rbind(
  metric_row("Persistence", persistence, actual_level),
  metric_row("Authors_aligned_505", pred_level_authors, actual_level),
  metric_row("Augmented_506", pred_level_augmented, actual_level)
)

print(comparison)

write.csv(
  comparison,
  "results/validation_authors_aligned_comparison.csv",
  row.names = FALSE
)

cat("\nSaved: results/validation_authors_aligned_comparison.csv\n")
cat("\n===== END STEP 16E =====\n")






cat("\n===== STEP 16F: IMPROVEMENT OVER PERSISTENCE =====\n")

base <- comparison[comparison$model == "Persistence", ]
comparison$MAE_change_pct <- (
  comparison$MAE - base$MAE
) / base$MAE * 100

comparison$RMSE_change_pct <- (
  comparison$RMSE - base$RMSE
) / base$RMSE * 100

print(comparison)

write.csv(
  comparison,
  "results/validation_authors_aligned_comparison.csv",
  row.names = FALSE
)

cat("\nSaved updated comparison.\n")
cat("\n===== END STEP 16F =====\n")





cat("\n===== STEP 17A: SEARCH PROJECT DOCUMENTATION =====\n")

project_root <- getwd()
cat("Current project:", project_root, "\n")

files_found <- list.files(
  path = project_root,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  pattern = "README|readme|metadata|data.?dictionary|ML4FF|main\\.py|data_structure|\\.md$|\\.txt$|\\.py$"
)

print(files_found)

cat("\n===== END STEP 17A =====\n")



cat("\n===== STEP 17B: FEATURE AND TARGET TIMING PROFILE =====\n")

library(data.table)

# Use the existing one-hour dataset
timing_profile <- data.table(
  datahora = df$datahora,
  nivel_conselheiro = df$nivel_conselheiro,
  chuva_conselheiro = df$chuva_conselheiro,
  out01h00m = df$out01h00m
)

grid_cols <- grep("^precip[0-9]+$", names(df), value = TRUE)

cat("\nNumber of rainfall grid features:", length(grid_cols), "\n")

cat("\nTimestamp range:\n")
print(range(timing_profile$datahora, na.rm = TRUE))

cat("\nSummary of key variables:\n")
print(summary(timing_profile[, .(
  nivel_conselheiro,
  chuva_conselheiro,
  out01h00m
)]))

cat("\nFirst 10 observations:\n")
print(head(timing_profile, 10))

cat("\nLast 10 observations:\n")
print(tail(timing_profile, 10))

cat("\n===== END STEP 17B =====\n")







# ============================================================
# STEP 18: PAIRED RAINFALL-GRID MASKING COMPARISON
# Authors-aligned 505-feature model vs augmented 506-feature
# model. Validation data only; no retraining.
# ============================================================

library(data.table)
library(ranger)

stopifnot(
  exists("val_delta"),
  exists("train_delta"),
  exists("grid_cols"),
  exists("models"),
  exists("model_authors_aligned")
)

# Define exact predictors used by each fitted model
authors_predictors <- c(grid_cols, "nivel_conselheiro")
augmented_predictors <- predictor_cols

model_objects <- list(
  Authors_aligned_505 = model_authors_aligned,
  Augmented_506 = models[["All_features"]]
)

predictor_sets <- list(
  Authors_aligned_505 = authors_predictors,
  Augmented_506 = augmented_predictors
)

# Confirm that all required features exist
stopifnot(
  length(grid_cols) == 504,
  all(authors_predictors %in% names(val_delta)),
  all(augmented_predictors %in% names(val_delta)),
  all(authors_predictors %in% names(train_delta)),
  all(augmented_predictors %in% names(train_delta))
)

# Training-only medians for rainfall-grid replacement
train_medians_local <- vapply(
  grid_cols,
  function(x) median(train_delta[[x]], na.rm = TRUE),
  numeric(1)
)

# Validation data; preserve identical observations across models
val_base <- as.data.frame(val_delta)
n_val <- nrow(val_base)

stopifnot(
  all(is.finite(val_base$nivel_conselheiro)),
  all(is.finite(val_base$delta_target)),
  n_val > 0
)

mask_rates <- c(0, 0.10, 0.20, 0.40)
mask_seeds <- c(2026, 2027, 2028)

results_step18 <- list()
k <- 1L

for (seed in mask_seeds) {
  for (rate in mask_rates) {
    
    # Generate ONE mask per seed/rate and reuse it for both models
    set.seed(seed)
    
    n_mask <- round(rate * length(grid_cols))
    
    masked_cols <- if (n_mask > 0) {
      sample(grid_cols, size = n_mask, replace = FALSE)
    } else {
      character(0)
    }
    
    val_masked <- val_base
    
    if (length(masked_cols) > 0) {
      for (col in masked_cols) {
        val_masked[[col]] <- train_medians_local[[col]]
      }
    }
    
    # Evaluate both models on precisely the same validation rows
    for (model_name in names(model_objects)) {
      
      fitted_model <- model_objects[[model_name]]
      features <- predictor_sets[[model_name]]
      
      x_val <- val_masked[, features, drop = FALSE]
      
      pred_delta <- predict(
        fitted_model,
        data = x_val
      )$predictions
      
      # Reconstruct predicted future water level
      pred_level <- val_masked$nivel_conselheiro + pred_delta
      actual_level <- val_masked$out01h00m
      
      errors <- pred_level - actual_level
      
      results_step18[[k]] <- data.table(
        model = model_name,
        mask_seed = seed,
        outage_fraction = rate,
        masked_grid_features = n_mask,
        n = length(errors),
        MAE = mean(abs(errors)),
        RMSE = sqrt(mean(errors^2)),
        Bias = mean(errors)
      )
      
      k <- k + 1L
    }
  }
}

step18_results <- rbindlist(results_step18)

# Compare each model against its own zero-mask result
step18_results[, `:=`(
  MAE_change_pct = NA_real_,
  RMSE_change_pct = NA_real_
)]

for (model_name in unique(step18_results$model)) {
  baseline <- step18_results[
    model == model_name & outage_fraction == 0,
    .(mask_seed, base_MAE = MAE, base_RMSE = RMSE)
  ]
  
  idx <- which(step18_results$model == model_name)
  temp <- merge(
    step18_results[idx],
    baseline,
    by = "mask_seed",
    all.x = TRUE,
    sort = FALSE
  )
  
  step18_results[
    idx,
    MAE_change_pct :=
      100 * (temp$MAE - temp$base_MAE) / temp$base_MAE
  ]
  
  step18_results[
    idx,
    RMSE_change_pct :=
      100 * (temp$RMSE - temp$base_RMSE) / temp$base_RMSE
  ]
}

# Save raw results
dir.create("results", showWarnings = FALSE)

fwrite(
  step18_results,
  "results/step18_paired_masking_comparison.csv"
)

# Summarize across the three masking seeds
step18_summary <- step18_results[
  ,
  .(
    mean_MAE = mean(MAE),
    sd_MAE = sd(MAE),
    mean_RMSE = mean(RMSE),
    sd_RMSE = sd(RMSE),
    mean_Bias = mean(Bias),
    mean_MAE_change_pct = mean(MAE_change_pct),
    mean_RMSE_change_pct = mean(RMSE_change_pct)
  ),
  by = .(model, outage_fraction, masked_grid_features)
][order(model, outage_fraction)]

fwrite(
  step18_summary,
  "results/step18_paired_masking_summary.csv"
)

cat("\n===== STEP 18: PAIRED MASKING RESULTS =====\n")
print(step18_results[order(outage_fraction, model, mask_seed)])
cat("\n===== SUMMARY =====\n")
print(step18_summary)

cat("\nSaved files:\n")
cat("results/step18_paired_masking_comparison.csv\n")
cat("results/step18_paired_masking_summary.csv\n")





cat("===== CHECK TEST DATA OBJECTS =====\n")

objects_to_check <- c(
  "test_delta",
  "test_data",
  "test_data_aligned",
  "df",
  "eligible",
  "train_delta",
  "val_delta"
)

for (obj in objects_to_check) {
  cat(
    obj, ":",
    if (exists(obj, envir = .GlobalEnv, inherits = FALSE)) {
      paste("EXISTS | class =", paste(class(get(obj)), collapse = ", "),
            "| rows =", nrow(get(obj)))
    } else {
      "NOT FOUND"
    },
    "\n"
  )
}

if (exists("test_data", envir = .GlobalEnv, inherits = FALSE)) {
  cat("\nColumns in test_data:\n")
  print(names(test_data))
  cat("\nDimensions:\n")
  print(dim(test_data))
}





# ============================================================
# STEP 19: STANDARDIZED TEST COMPARISON
# IMPORTANT: the test split was inspected earlier in this script. These are
# standardized rerun results, NOT an untouched-test estimate.
# ============================================================

library(data.table)
library(ranger)

# Reconstruct target change from existing test_data
test_final <- as.data.frame(test_data)
test_final$delta_target <-
  test_final$out01h00m - test_final$nivel_conselheiro

train_final <- as.data.frame(train_delta)

grid_cols <- grep("^precip[0-9]+$", names(test_final), value = TRUE)

feature_sets <- list(
  Authors_aligned_505 = c(grid_cols, "nivel_conselheiro"),
  Augmented_506 = c(
    grid_cols, "nivel_conselheiro", "chuva_conselheiro"
  ),
  Level_plus_local_rain = c(
    "nivel_conselheiro", "chuva_conselheiro"
  )
)

# Save the exact final-protocol configuration for reproducibility.
final_model_config <- data.table(
  model = names(feature_sets),
  predictors = vapply(feature_sets, length, integer(1)),
  num_trees = 300L,
  mtry = vapply(feature_sets, function(x) {
    as.integer(max(1L, floor(sqrt(length(x)))))
  }, integer(1)),
  min_node_size = 5L,
  seed = 2026L,
  target = "delta_target",
  prediction_reconstruction = "nivel_conselheiro + predicted_delta"
)
fwrite(final_model_config, "results/step19_model_configuration.csv")

stopifnot(
  length(grid_cols) == 504,
  nrow(test_final) == 10459,
  all(is.finite(test_final$delta_target)),
  all(is.finite(test_final$nivel_conselheiro)),
  all(is.finite(train_final$delta_target))
)

# Train fixed configurations on training data only
final_models <- list()
final_predictions <- list()

for (model_name in names(feature_sets)) {
  
  predictors <- feature_sets[[model_name]]
  
  stopifnot(all(predictors %in% names(train_final)))
  stopifnot(all(predictors %in% names(test_final)))
  
  # Explicitly apply the same RF configuration rule to every predictor set.
  # mtry is calculated per model because predictor counts differ.
  mtry_value <- max(1L, floor(sqrt(length(predictors))))

  fit <- ranger(
    dependent.variable.name = "delta_target",
    data = train_final[, c("delta_target", predictors), drop = FALSE],
    num.trees = 300,
    mtry = mtry_value,
    min.node.size = 5,
    seed = 2026,
    importance = "none",
    num.threads = max(1, parallel::detectCores() - 1)
  )
  
  pred_delta <- predict(
    fit,
    data = test_final[, predictors, drop = FALSE]
  )$predictions
  
  final_models[[model_name]] <- fit
  final_predictions[[model_name]] <-
    test_final$nivel_conselheiro + pred_delta
}

# Persistence baseline
final_predictions[["Persistence"]] <- test_final$nivel_conselheiro

# Calculate metrics
final_test_results <- rbindlist(lapply(
  names(final_predictions),
  function(model_name) {
    
    pred <- final_predictions[[model_name]]
    actual <- test_final$out01h00m
    err <- pred - actual
    
    data.table(
      model = model_name,
      n = length(actual),
      MAE = mean(abs(err)),
      RMSE = sqrt(mean(err^2)),
      Bias = mean(err)
    )
  }
))

base <- final_test_results[
  model == "Persistence",
  .(base_MAE = MAE, base_RMSE = RMSE)
]

final_test_results[, MAE_change_vs_persistence_pct :=
                     100 * (MAE - base$base_MAE) / base$base_MAE]

final_test_results[, RMSE_change_vs_persistence_pct :=
                     100 * (RMSE - base$base_RMSE) / base$base_RMSE]

setorder(final_test_results, MAE)

dir.create("results", showWarnings = FALSE)

fwrite(
  final_test_results,
  "results/step19_final_test_metrics.csv"
)

cat("\n===== STEP 19: FINAL TEST METRICS =====\n")
print(final_test_results)

cat("\nSaved: results/step19_final_test_metrics.csv\n")






# ============================================================
# STEP 20: PAIRED TEST ERROR COMPARISON
# ============================================================

stopifnot(
  exists("final_predictions"),
  exists("test_final")
)

actual <- test_final$out01h00m
persistence_pred <- final_predictions[["Persistence"]]

comparison_models <- c(
  "Level_plus_local_rain",
  "Augmented_506",
  "Authors_aligned_505"
)

step20_results <- rbindlist(lapply(
  comparison_models,
  function(model_name) {
    
    model_pred <- final_predictions[[model_name]]
    
    # Absolute errors on identical test observations
    error_persistence <- abs(persistence_pred - actual)
    error_model <- abs(model_pred - actual)
    
    # Positive difference means the RF has greater absolute error
    difference <- error_model - error_persistence
    
    data.table(
      model = model_name,
      n = length(actual),
      mean_absolute_error_difference = mean(difference),
      median_absolute_error_difference = median(difference),
      proportion_observations_RF_better =
        mean(error_model < error_persistence),
      proportion_observations_tied =
        mean(error_model == error_persistence),
      proportion_observations_RF_worse =
        mean(error_model > error_persistence)
    )
  }
))

print(step20_results)

fwrite(
  step20_results,
  "results/step20_paired_test_error_comparison.csv"
)

cat("\nSaved: results/step20_paired_test_error_comparison.csv\n")







# ============================================================
# STEP 21: RESULTS FILE AUDIT
# ============================================================

result_files <- list.files(
  "results",
  pattern = "\\.csv$",
  full.names = TRUE
)

results_inventory <- data.frame(
  file = basename(result_files),
  rows = sapply(result_files, function(f) {
    tryCatch(nrow(data.table::fread(f)), error = function(e) NA_integer_)
  }),
  columns = sapply(result_files, function(f) {
    tryCatch(ncol(data.table::fread(f)), error = function(e) NA_integer_)
  })
)

print(results_inventory, row.names = FALSE)

write.csv(
  results_inventory,
  "results/step21_results_inventory.csv",
  row.names = FALSE
)

cat("\nSaved: results/step21_results_inventory.csv\n")







# ============================================================
# STEP 22: AUDIT CORE RESULT TABLES
# ============================================================

library(data.table)

files_to_check <- c(
  "persistence_baseline_metrics.csv",
  "rf_level_change_validation.csv",
  "validation_feature_ablation.csv",
  "validation_authors_aligned_comparison.csv",
  "step18_paired_masking_summary.csv",
  "contiguous_outage_well_covered_summary.csv",
  "step19_final_test_metrics.csv",
  "step20_paired_test_error_comparison.csv"
)

for (f in files_to_check) {
  path <- file.path("results", f)
  
  cat("\n\n========================================\n")
  cat("FILE:", f, "\n")
  cat("========================================\n")
  
  if (file.exists(path)) {
    print(fread(path))
  } else {
    cat("FILE NOT FOUND\n")
  }
}







# ============================================================
# STEP 23: VERIFY ALIGNED PERSISTENCE METRICS
# ============================================================

library(data.table)

stopifnot(
  exists("val_delta"),
  exists("test_data"),
  all(c("out01h00m", "nivel_conselheiro") %in%
        names(test_data))
)

# Validation: same eligible rows used in model comparison
val_actual <- val_delta$delta_target +
  val_delta$nivel_conselheiro

val_pred_persistence <- val_delta$nivel_conselheiro

val_error <- val_pred_persistence - val_actual

# Test: same eligible rows used in Step 19
test_actual <- test_data$out01h00m
test_pred_persistence <- test_data$nivel_conselheiro

test_error <- test_pred_persistence - test_actual

aligned_baseline <- data.table(
  split = c("validation_aligned", "test_aligned"),
  n = c(length(val_error), length(test_error)),
  MAE = c(
    mean(abs(val_error)),
    mean(abs(test_error))
  ),
  RMSE = c(
    sqrt(mean(val_error^2)),
    sqrt(mean(test_error^2))
  ),
  Bias = c(
    mean(val_error),
    mean(test_error)
  )
)

print(aligned_baseline)

fwrite(
  aligned_baseline,
  "results/step23_aligned_persistence_verification.csv"
)

cat("\nSaved: results/step23_aligned_persistence_verification.csv\n")






# ============================================================
# STEP 24: RANDOM-MASKING ROBUSTNESS FIGURE
# ============================================================

library(data.table)
library(ggplot2)

results <- fread(
  "results/step18_paired_masking_summary.csv"
)

# Convert outage fraction to percentage
results[, masked_percent := 100 * outage_fraction]

# Prepare long-format data for MAE and RMSE
mae_data <- results[, .(
  model,
  masked_percent,
  change_percent = mean_MAE_change_pct,
  metric = "MAE"
)]

rmse_data <- results[, .(
  model,
  masked_percent,
  change_percent = mean_RMSE_change_pct,
  metric = "RMSE"
)]

plot_data <- rbind(mae_data, rmse_data)

plot_data[, model := factor(
  model,
  levels = c("Authors_aligned_505", "Augmented_506"),
  labels = c("Authors-aligned (505 features)",
             "Augmented (506 features)")
)]

p <- ggplot(
  plot_data,
  aes(
    x = masked_percent,
    y = change_percent,
    colour = model,
    group = model
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~metric, scales = "free_y") +
  scale_x_continuous(
    breaks = c(0, 10, 20, 40)
  ) +
  labs(
    x = "Rainfall-grid features masked (%)",
    y = "Error change from unmasked baseline (%)",
    colour = "Model",
    title = "Random Masking Robustness of Water-Level Forecasts"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

print(p)

ggsave(
  "results/fig_random_masking_robustness.png",
  plot = p,
  width = 7.2,
  height = 4.4,
  dpi = 300
)

ggsave(
  "results/fig_random_masking_robustness.pdf",
  plot = p,
  width = 7.2,
  height = 4.4
)

cat("\nSaved PNG and PDF in the results folder.\n")







# ============================================================
# STEP 25: FINAL VALIDATION + TEST COMPARISON TABLE
# ============================================================

library(data.table)

# Validation results from existing, aligned evaluations
validation <- data.table(
  model = c(
    "Persistence",
    "Level_only",
    "Level_plus_local_rain",
    "Authors_aligned_505",
    "Augmented_506"
  ),
  validation_MAE = c(
    0.017988154,
    0.01906206,
    0.01674304,
    0.02052632,
    0.02031422
  ),
  validation_RMSE = c(
    0.06269181,
    0.06280791,
    0.05703609,
    0.05654045,
    0.05626536
  ),
  validation_Bias = c(
    0.000450898,
    0.000346153,
    -0.000068736,
    0.006812026,
    0.006415564
  )
)

# Verified final test results
test <- fread("results/step19_final_test_metrics.csv")

test <- test[, .(
  model,
  test_n = n,
  test_MAE = MAE,
  test_RMSE = RMSE,
  test_Bias = Bias
)]

# Combine validation and test metrics
final_comparison <- merge(
  validation,
  test,
  by = "model",
  all = TRUE,
  sort = FALSE
)

# Add test MAE change relative to persistence
persistence_test_MAE <- final_comparison[
  model == "Persistence",
  test_MAE
]

final_comparison[, test_MAE_change_pct :=
                   100 * (test_MAE - persistence_test_MAE) /
                   persistence_test_MAE
]

setorder(final_comparison, test_MAE)

print(final_comparison)

fwrite(
  final_comparison,
  "results/step25_final_validation_test_comparison.csv"
)

cat("\nSaved: results/step25_final_validation_test_comparison.csv\n")