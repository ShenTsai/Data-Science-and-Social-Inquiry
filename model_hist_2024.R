.libPaths(c("R_lib", .libPaths()))
library(readr)
library(dplyr)
library(ggplot2)
library(randomForest)

# Keep every complete original record; this is the source data for later work.
predictor_names <- c("sales", "temperature", "humidity", "distance_to_store", "indoor", "weekend", "club_activity_day", "building_type")
clean <- read_csv("historical_sales_2024.csv", show_col_types = FALSE) |>
  filter(if_all(all_of(predictor_names), ~ !is.na(.))) |>
  mutate(building_type = relevel(factor(building_type), ref = "classroom"))
saveRDS(clean, "hist_2024_cleaned.rds")

# 1. Top/bottom sales-tail profile (cut points are calculated on machine-days).
cut_points <- quantile(clean$sales, c(.20, .80), names = FALSE)
tail_data <- clean |>
  filter(sales <= cut_points[1] | sales >= cut_points[2]) |>
  mutate(sales_group = if_else(sales <= cut_points[1], "Low: bottom 20%", "High: top 20%"))
numeric_profile <- tail_data |>
  group_by(sales_group) |>
  summarise(across(c(sales, temperature, humidity, distance_to_store, indoor, weekend, club_activity_day), mean), n = n(), .groups = "drop")
building_profile <- tail_data |>
  count(sales_group, building_type) |>
  group_by(sales_group) |>
  mutate(share = n / sum(n)) |>
  ungroup()
write_csv(numeric_profile, "hist_2024_tail_numeric_profile.csv")
write_csv(building_profile, "hist_2024_tail_building_profile.csv")
write_csv(tibble(bottom_20_cutoff = cut_points[1], top_20_cutoff = cut_points[2]), "hist_2024_tail_cutoffs.csv")

profile_long <- numeric_profile |>
  select(-n, -sales) |>
  tidyr::pivot_longer(-sales_group, names_to = "variable", values_to = "mean") |>
  group_by(variable) |>
  mutate(z = (mean - mean(mean)) / sd(mean)) |>
  ungroup()
ggsave("hist_2024_sales_tail_profile.png", ggplot(profile_long, aes(variable, z, fill = sales_group)) +
  geom_col(position = "dodge") + coord_flip() +
  labs(x = NULL, y = "Standardized mean within comparison", fill = NULL,
       title = "Characteristics of low- and high-sales machine-days") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom"), width = 8, height = 5, dpi = 180)
ggsave("hist_2024_sales_tail_building_mix.png", ggplot(building_profile, aes(building_type, share, fill = sales_group)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::percent) +
  labs(x = "Building type", y = "Share of sales-tail group", fill = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom"), width = 8, height = 5, dpi = 180)

# 2--5. Identical predictors and 5-fold random cross-validation for both models.
ols_formula <- sales ~ temperature + humidity + distance_to_store + indoor + weekend + club_activity_day + building_type
set.seed(20260927)
clean$fold <- sample(rep(1:5, length.out = nrow(clean)))
ols_oof <- rf_oof <- rep(NA_real_, nrow(clean))
checkpoint_file <- "hist_2024_cv_checkpoint.rds"
if (file.exists(checkpoint_file)) {
  checkpoint <- readRDS(checkpoint_file)
  ols_oof <- checkpoint$ols_oof; rf_oof <- checkpoint$rf_oof
}
for (k in 1:5) {
  if (all(is.finite(ols_oof[clean$fold == k])) && all(is.finite(rf_oof[clean$fold == k]))) next
  train <- clean[clean$fold != k, ]; test <- clean[clean$fold == k, ]
  ols_oof[clean$fold == k] <- predict(lm(ols_formula, data = train), newdata = test)
  # A reproducible 20,000-row sample per fold makes RF training tractable at 1.1m rows;
  # every held-out row is still used to calculate the reported accuracy.
  set.seed(20260927 + k)
  rf_train <- train[sample.int(nrow(train), min(20000, nrow(train))), ]
  rf_fit <- randomForest(ols_formula, data = rf_train, ntree = 40, maxnodes = 500, mtry = 3, nodesize = 10)
  rf_oof[clean$fold == k] <- predict(rf_fit, newdata = test)
  saveRDS(list(ols_oof = ols_oof, rf_oof = rf_oof), checkpoint_file)
  message("Completed fold ", k)
}
residuals_oof <- clean |>
  transmute(sales, fold, ols_prediction = ols_oof, rf_prediction = rf_oof,
            ols_residual = sales - ols_oof, rf_residual = sales - rf_oof)
metrics <- tibble(model = c("OLS", "Random Forest"),
  mse = c(mean(residuals_oof$ols_residual^2), mean(residuals_oof$rf_residual^2)),
  rmse = sqrt(mse),
  mae = c(mean(abs(residuals_oof$ols_residual)), mean(abs(residuals_oof$rf_residual))))
write_csv(metrics, "hist_2024_cv_metrics.csv")
write_csv(residuals_oof, "hist_2024_oof_predictions.csv")

ggsave("hist_2024_ols_oof_residuals.png", ggplot(residuals_oof, aes(ols_residual)) +
  geom_histogram(bins = 80, fill = "#3B82F6", colour = "white") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "OLS out-of-fold residual (observed − held-out prediction)", y = "Machine-days",
       title = "Distribution of OLS held-out residuals") + theme_minimal(base_size = 12), width = 8, height = 5, dpi = 180)
ggsave("hist_2024_cv_accuracy.png", ggplot(metrics, aes(model, mse, fill = model)) +
  geom_col(width = .6, show.legend = FALSE) +
  labs(x = NULL, y = "5-fold cross-validated MSE", title = "Held-out predictive accuracy") +
  theme_minimal(base_size = 12), width = 6, height = 5, dpi = 180)

print(tibble(n_clean = nrow(clean), bottom_20_cutoff = cut_points[1], top_20_cutoff = cut_points[2]))
print(numeric_profile); print(building_profile); print(summary(lm(ols_formula, data = clean)))
print(metrics)
