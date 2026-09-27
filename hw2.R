library(readr)
library(ggplot2)
library(dplyr)

cand <- read_csv("candidate_site_inputs_2025.csv")
hist_2024 <- read_csv("historical_sales_2024.csv")

ggplot(data = hist_2024, aes(x = factor(building_type), y = sales)) + geom_boxplot()

lm(data = hist_2024, sales ~ temperature + humidity + distance_to_store + indoor + weekend + club_activity_day)

