# ============================================================
# Heart Disease Severity Analysis
# ============================================================
# Reproducible analysis script
# ============================================================

# 1. Packages --------------------------------------------------
required_packages <- c(
  "tidyverse",
  "MASS",
  "broom",
  "car",
  "ordinal"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# 2. Paths -----------------------------------------------------
data_path <- file.path("data", "heart_disease.csv")
figure_dir <- "figures"
result_dir <- "results"

dir.create(figure_dir, showWarnings = FALSE)
dir.create(result_dir, showWarnings = FALSE)

# 3. Import and variable names --------------------------------
variable_names <- c(
  "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
  "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target"
)

df <- read.csv(
  data_path,
  header = FALSE,
  na.strings = "?",
  stringsAsFactors = FALSE
)

if (ncol(df) != length(variable_names)) {
  stop("Unexpected number of columns in the dataset.")
}

names(df) <- variable_names

# Convert all columns to numeric
df <- df %>%
  mutate(across(everything(), as.numeric))

# Keep complete cases for the main analysis
df_clean <- df %>%
  drop_na()

# Treat the target as an ordered outcome
df_clean <- df_clean %>%
  mutate(
    target_ordered = ordered(target, levels = sort(unique(target)))
  )

write.csv(
  df_clean,
  file.path(result_dir, "cleaned_analysis_data.csv"),
  row.names = FALSE
)

# 4. Descriptive statistics ------------------------------------
descriptive_stats <- df_clean %>%
  summarise(
    across(
      c(age, trestbps, chol, thalach, oldpeak),
      list(
        mean = ~mean(.x),
        median = ~median(.x),
        sd = ~sd(.x),
        min = ~min(.x),
        max = ~max(.x)
      ),
      .names = "{.col}_{.fn}"
    )
  )

write.csv(
  descriptive_stats,
  file.path(result_dir, "descriptive_statistics.csv"),
  row.names = FALSE
)

# 5. Target distribution ---------------------------------------
target_distribution <- df_clean %>%
  count(target) %>%
  mutate(percentage = 100 * n / sum(n))

write.csv(
  target_distribution,
  file.path(result_dir, "target_distribution.csv"),
  row.names = FALSE
)

p_target <- ggplot(target_distribution, aes(x = factor(target), y = n)) +
  geom_col() +
  labs(
    title = "Distribution of Heart Disease Severity",
    x = "Severity category",
    y = "Number of observations"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "target_distribution.png"),
  p_target, width = 7, height = 5, dpi = 300
)

# 6. Box plots -------------------------------------------------
box_variables <- c("age", "trestbps", "chol", "thalach", "oldpeak")

for (v in box_variables) {
  p <- ggplot(df_clean, aes(x = factor(target), y = .data[[v]])) +
    geom_boxplot() +
    labs(
      title = paste("Distribution of", v, "by Heart Disease Severity"),
      x = "Severity category",
      y = v
    ) +
    theme_minimal()

  ggsave(
    file.path(figure_dir, paste0("boxplot_", v, ".png")),
    p, width = 7, height = 5, dpi = 300
  )
}

# 7. Correlation matrix ----------------------------------------
cor_vars <- df_clean %>%
  select(age, trestbps, chol, thalach, oldpeak, target)

correlation_matrix <- cor(cor_vars, use = "complete.obs")
write.csv(
  correlation_matrix,
  file.path(result_dir, "correlation_matrix.csv")
)

# 8. OLS baseline model ----------------------------------------
ols_model <- lm(
  target ~ age + trestbps + chol + thalach + oldpeak,
  data = df_clean
)

ols_table <- broom::tidy(ols_model, conf.int = TRUE)
write.csv(
  ols_table,
  file.path(result_dir, "ols_coefficients.csv"),
  row.names = FALSE
)

capture.output(
  summary(ols_model),
  file = file.path(result_dir, "ols_summary.txt")
)

# Multicollinearity diagnostic
vif_values <- car::vif(ols_model)
write.csv(
  data.frame(variable = names(vif_values), VIF = as.numeric(vif_values)),
  file.path(result_dir, "vif_diagnostics.csv"),
  row.names = FALSE
)

# 9. Ordered logistic regression -------------------------------
ordinal_model <- MASS::polr(
  target_ordered ~ age + trestbps + chol + thalach + oldpeak,
  data = df_clean,
  Hess = TRUE,
  method = "logistic"
)

ordinal_coefficients <- broom::tidy(ordinal_model, conf.int = TRUE)
write.csv(
  ordinal_coefficients,
  file.path(result_dir, "ordered_logit_coefficients.csv"),
  row.names = FALSE
)

capture.output(
  summary(ordinal_model),
  file = file.path(result_dir, "ordered_logit_summary.txt")
)

# 10. Model comparison -----------------------------------------
model_comparison <- data.frame(
  model = c("OLS baseline", "Ordered logistic regression"),
  AIC = c(AIC(ols_model), AIC(ordinal_model)),
  logLik = c(as.numeric(logLik(ols_model)), as.numeric(logLik(ordinal_model)))
)

write.csv(
  model_comparison,
  file.path(result_dir, "model_comparison.csv"),
  row.names = FALSE
)

# 11. Predicted probabilities ---------------------------------
predicted_probabilities <- as.data.frame(
  predict(ordinal_model, type = "probs")
)

predicted_probabilities$row_id <- seq_len(nrow(predicted_probabilities))

write.csv(
  predicted_probabilities,
  file.path(result_dir, "predicted_probabilities.csv"),
  row.names = FALSE
)

# 12. Model diagnostics ----------------------------------------
png(file.path(figure_dir, "ols_diagnostic_plots.png"),
    width = 1600, height = 1200, res = 200)
par(mfrow = c(2, 2))
plot(ols_model)
dev.off()

# 13. Session information --------------------------------------
capture.output(
  sessionInfo(),
  file = file.path(result_dir, "session_info.txt")
)

message("Analysis completed. Check the figures/ and results/ folders.")
