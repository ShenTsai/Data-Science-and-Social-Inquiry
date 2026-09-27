library(readr)
library(dplyr)
library(ggplot2)

# Refit the prior best model (OLS) on all cleaned 2024 machine-days.
train <- readRDS("hist_2024_cleaned.rds") |>
  mutate(building_type = relevel(factor(building_type), ref = "classroom"))
ols_formula <- sales ~ temperature + humidity + distance_to_store + indoor + weekend + club_activity_day + building_type
ols_fit <- lm(ols_formula, data = train)

candidate_daily <- read_csv("candidate_site_inputs_2025.csv", show_col_types = FALSE) |>
  filter(if_all(c("location_id", "building_type", "distance_to_store", "temperature", "humidity", "indoor", "weekend", "club_activity_day"), ~ !is.na(.))) |>
  mutate(building_type = factor(building_type, levels = levels(train$building_type)))
candidate_daily$predicted_sales <- predict(ols_fit, newdata = candidate_daily)

# One annual forecast and one set of static/location-average characteristics per candidate.
annual <- candidate_daily |>
  group_by(location_id, building_type) |>
  summarise(predicted_annual_sales = sum(predicted_sales),
    distance_to_store = first(distance_to_store), indoor = first(indoor),
    mean_temperature = mean(temperature), mean_humidity = mean(humidity),
    weekend_share = mean(weekend), club_activity_share = mean(club_activity_day),
    .groups = "drop") |>
  arrange(desc(predicted_annual_sales), location_id) |>
  mutate(predicted_rank = row_number())
write_csv(annual, "candidate_annual_sales_rankings.csv")
write_csv(slice_head(annual, n = 3), "candidate_top_3.csv")

top_bottom_summary <- bind_rows(
  annual |> slice_head(n = 100) |> mutate(group = "Top 100"),
  annual |> slice_tail(n = 100) |> mutate(group = "Bottom 100")
) |>
  group_by(group) |>
  summarise(n = n(), mean_predicted_annual_sales = mean(predicted_annual_sales),
    median_predicted_annual_sales = median(predicted_annual_sales), min_predicted_annual_sales = min(predicted_annual_sales),
    max_predicted_annual_sales = max(predicted_annual_sales), mean_distance_to_store = mean(distance_to_store),
    indoor_share = mean(indoor), mean_temperature = mean(mean_temperature), mean_humidity = mean(mean_humidity),
    club_activity_share = mean(club_activity_share), .groups = "drop")
top_bottom_types <- bind_rows(
  annual |> slice_head(n = 100) |> mutate(group = "Top 100"),
  annual |> slice_tail(n = 100) |> mutate(group = "Bottom 100")
) |>
  count(group, building_type) |>
  group_by(group) |> mutate(share = n / sum(n)) |> ungroup()
write_csv(top_bottom_summary, "candidate_top_bottom_100_summary.csv")
write_csv(top_bottom_types, "candidate_top_bottom_100_building_mix.csv")

# Decision comparison: farthest 300 vs. 300 with the largest annual model forecasts.
distance_300 <- annual |> arrange(desc(distance_to_store), location_id) |> slice_head(n = 300) |> mutate(selection = "Farthest 300")
model_300 <- annual |> arrange(desc(predicted_annual_sales), location_id) |> slice_head(n = 300) |> mutate(selection = "Top-forecast 300")
selection_comparison <- bind_rows(distance_300, model_300) |>
  group_by(selection) |>
  summarise(n = n(), total_predicted_annual_sales = sum(predicted_annual_sales),
    mean_predicted_annual_sales = mean(predicted_annual_sales), mean_distance_to_store = mean(distance_to_store),
    indoor_share = mean(indoor), overlap_with_other_set = sum(location_id %in% if (first(selection) == "Farthest 300") model_300$location_id else distance_300$location_id),
    .groups = "drop")
difference <- selection_comparison$total_predicted_annual_sales[selection_comparison$selection == "Top-forecast 300"] -
  selection_comparison$total_predicted_annual_sales[selection_comparison$selection == "Farthest 300"]
selection_comparison <- selection_comparison |> mutate(advantage_vs_farthest = if_else(selection == "Top-forecast 300", difference, 0))
write_csv(selection_comparison, "candidate_selection_comparison.csv")

ggsave("candidate_annual_sales_ranking.png", ggplot(annual, aes(predicted_rank, predicted_annual_sales, colour = building_type)) +
  geom_point(alpha = .6, size = 1.5) +
  labs(x = "Rank (1 = highest forecast)", y = "Predicted annual sales", colour = "Building type") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom"), width = 8, height = 5, dpi = 180)
ggsave("candidate_selection_comparison.png", ggplot(selection_comparison, aes(selection, total_predicted_annual_sales, fill = selection)) +
  geom_col(show.legend = FALSE) + labs(x = NULL, y = "Total predicted annual sales") + theme_minimal(base_size = 12), width = 6, height = 5, dpi = 180)

print(slice_head(annual, n = 3)); print(top_bottom_summary); print(top_bottom_types); print(selection_comparison)
