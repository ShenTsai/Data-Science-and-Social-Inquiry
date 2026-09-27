library(readr)
library(dplyr)
library(ggplot2)

# Weather is common within a date, so inference uses daily building-type means.
raw <- read_csv("historical_sales_2024.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date), building_type = factor(building_type))

daily <- raw |>
  group_by(date, building_type) |>
  summarise(mean_sales = mean(sales), temperature = first(temperature),
    humidity = first(humidity), weekend = first(weekend),
    club_activity_rate = mean(club_activity_day),
    mean_activity_scale = mean(activity_scale), indoor_share = mean(indoor), .groups = "drop")

# Interactions test whether weather/weekend relationships vary by building type.
fit <- lm(mean_sales ~ building_type * (temperature + humidity + weekend) +
            club_activity_rate + mean_activity_scale, data = daily)

contrast_rows <- function(variable, low, high) {
  base <- daily |> summarise(temperature = mean(temperature), humidity = mean(humidity), weekend = 0,
    club_activity_rate = mean(club_activity_rate), mean_activity_scale = mean(mean_activity_scale), indoor_share = mean(indoor_share))
  bind_rows(lapply(levels(daily$building_type), function(bt) {
    a <- base |> mutate(building_type = factor(bt, levels = levels(daily$building_type))); b <- a
    a[[variable]] <- low; b[[variable]] <- high
    xa <- model.matrix(delete.response(terms(fit)), a); xb <- model.matrix(delete.response(terms(fit)), b)
    delta <- drop((xb - xa) %*% coef(fit)); se <- sqrt(drop((xb - xa) %*% vcov(fit) %*% t(xb - xa)))
    tibble(building_type = bt, comparison = paste0(variable, ": ", round(low, 1), " to ", round(high, 1)),
      estimated_change = delta, se = se, p_value = 2 * pt(abs(delta / se), df.residual(fit), lower.tail = FALSE))
  }))
}

q_temp <- quantile(daily$temperature, c(.1, .9)); q_hum <- quantile(daily$humidity, c(.1, .9))

weather_effects <- bind_rows(contrast_rows("temperature", q_temp[1], q_temp[2]), contrast_rows("humidity", q_hum[1], q_hum[2]))

weekend_effects <- contrast_rows("weekend", 0, 1) |>
  transmute(building_type, weekday_to_weekend_change = estimated_change, se, p_value, significant_at_05 = p_value < .05)

write_csv(weather_effects, "hist_2024_weather_effects.csv")
write_csv(weekend_effects, "hist_2024_weekend_effects.csv")
write_csv(broom::tidy(fit), "hist_2024_model_coefficients.csv")

prediction_data <- function(variable, values) {
  x <- expand.grid(building_type = levels(daily$building_type), value = values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) |>
    as_tibble() |> mutate(building_type = factor(building_type, levels = levels(daily$building_type)),
      temperature = mean(daily$temperature), humidity = mean(daily$humidity), weekend = 0,
      club_activity_rate = mean(daily$club_activity_rate), mean_activity_scale = mean(daily$mean_activity_scale), indoor_share = mean(daily$indoor_share))
  x[[variable]] <- x$value
  bind_cols(x, as_tibble(predict(fit, x, interval = "confidence")))
}

temp_pred <- prediction_data("temperature", seq(min(daily$temperature), max(daily$temperature), length.out = 100))

humid_pred <- prediction_data("humidity", seq(min(daily$humidity), max(daily$humidity), length.out = 100))

weather_plot <- function(pred, x, label) ggplot(pred, aes(x = .data[[x]], y = fit, colour = building_type, fill = building_type)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .14, colour = NA) + geom_line(linewidth = .9) +
  labs(x = label, y = "Adjusted mean daily sales per machine", colour = "Building type", fill = "Building type") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("hist_2024_temperature.png", weather_plot(temp_pred, "temperature", "Temperature (°C)"), width = 8, height = 5, dpi = 180)
ggsave("hist_2024_humidity.png", weather_plot(humid_pred, "humidity", "Humidity (%)"), width = 8, height = 5, dpi = 180)

week_grid <- expand.grid(building_type = levels(daily$building_type), weekend = c(0, 1), KEEP.OUT.ATTRS = FALSE) |>
  as_tibble() |> mutate(building_type = factor(building_type, levels = levels(daily$building_type)),
    temperature = mean(daily$temperature), humidity = mean(daily$humidity), club_activity_rate = mean(daily$club_activity_rate),
    mean_activity_scale = mean(daily$mean_activity_scale), indoor_share = mean(daily$indoor_share), day = if_else(weekend == 1, "Weekend", "Weekday"))

week_grid <- bind_cols(week_grid, as_tibble(predict(fit, week_grid, interval = "confidence")))

ggsave("hist_2024_weekend.png", ggplot(week_grid, aes(day, fit, colour = building_type, group = building_type)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .08, position = position_dodge(.25)) + geom_point(size = 3, position = position_dodge(.25)) +
  geom_line(position = position_dodge(.25)) + labs(x = NULL, y = "Adjusted mean daily sales per machine", colour = "Building type") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom"), width = 8, height = 5, dpi = 180)

print(weather_effects); print(weekend_effects); print(anova(fit))
