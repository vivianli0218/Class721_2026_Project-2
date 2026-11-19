library(ggplot2)



# load the cleaning function and create the combined dataset
source("process_aq.R")

# calculate the minimum and maximum daily CO concentration in 2018
min(aq_2018_clean$co, na.rm = TRUE)
max(aq_2018_clean$co, na.rm = TRUE)

# summarize the daily concentration ranges for 2018
table_2018 <- data.frame(
  Pollutant = c(
    "CO (ppm)",
    "PM2.5 (ug/m3 LC)",
    "O3 (ppm)"
  ),
  Minimum = c(
    min(aq_2018_clean$co, na.rm = TRUE),
    min(aq_2018_clean$pm25, na.rm = TRUE),
    min(aq_2018_clean$o3, na.rm = TRUE)
  ),
  Maximum = c(
    max(aq_2018_clean$co, na.rm = TRUE),
    max(aq_2018_clean$pm25, na.rm = TRUE),
    max(aq_2018_clean$o3, na.rm = TRUE)
  )
)

table_2018

# display the table 
knitr::kable(
  table_2018,
  digits = 3,
  format.args = list(nsmall = 3),
  row.names = FALSE,
  caption = "Table 1. Minimum and maximum daily pollutant concentrations in 2018."
)

# table for 2019
table_2019 <- data.frame(
  Pollutant = c(
    "CO (ppm)",
    "PM2.5 (ug/m3 LC)",
    "O3 (ppm)"
  ),
  Minimum = c(
    min(aq_2019_clean$co, na.rm = TRUE),
    min(aq_2019_clean$pm25, na.rm = TRUE),
    min(aq_2019_clean$o3, na.rm = TRUE)
  ),
  Maximum = c(
    max(aq_2019_clean$co, na.rm = TRUE),
    max(aq_2019_clean$pm25, na.rm = TRUE),
    max(aq_2019_clean$o3, na.rm = TRUE)
  )
)

table_2019

knitr::kable(
  table_2019,
  digits = 3,
  format.args = list(nsmall = 3),
  row.names = FALSE,
  caption = "Table 2. Minimum and maximum daily pollutant concentrations in 2019."
)

# table for 2020
table_2020 <- data.frame(
  Pollutant = c(
    "CO (ppm)",
    "PM2.5 (ug/m3 LC)",
    "O3 (ppm)"
  ),
  Minimum = c(
    min(aq_2020_clean$co, na.rm = TRUE),
    min(aq_2020_clean$pm25, na.rm = TRUE),
    min(aq_2020_clean$o3, na.rm = TRUE)
  ),
  Maximum = c(
    max(aq_2020_clean$co, na.rm = TRUE),
    max(aq_2020_clean$pm25, na.rm = TRUE),
    max(aq_2020_clean$o3, na.rm = TRUE)
  )
)

table_2020


knitr::kable(
  table_2020,
  digits = 3,
  format.args = list(nsmall = 3),
  row.names = FALSE,
  caption = "Table 3. Minimum and maximum daily pollutant concentrations from January through April 2020."
)


## Plots
# extract the calendar month from each date
aq_all <- aq_all |>
  dplyr::mutate(
    month = as.integer(format(date, "%m"))
  )

# check the number of daily rows in each month
table(aq_all$month)

# pool daily CO values by calendar month across years
co_monthly <- aq_all |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    mean_co = mean(co, na.rm = TRUE),
    sd_co = sd(co, na.rm = TRUE),
    n_co = sum(!is.na(co)),
    .groups = "drop"
  )

co_monthly

# calculate standard errors and 95% confidence intervals
co_monthly <- co_monthly |>
  dplyr::mutate(
    se_co = sd_co / sqrt(n_co),
    lower_co = mean_co - 1.96 * se_co,
    upper_co = mean_co + 1.96 * se_co
  )

co_monthly

# pool daily ozone values by calendar month
o3_monthly <- aq_all |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    mean_o3 = mean(o3, na.rm = TRUE),
    sd_o3 = sd(o3, na.rm = TRUE),
    n_o3 = sum(!is.na(o3)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    se_o3 = sd_o3 / sqrt(n_o3),
    lower_o3 = mean_o3 - 1.96 * se_o3,
    upper_o3 = mean_o3 + 1.96 * se_o3
  )

# prepare CO results for plotting
co_plot <- co_monthly |>
  dplyr::transmute(
    month,
    pollutant = "CO",
    mean = mean_co,
    lower = lower_co,
    upper = upper_co
  )

# prepare ozone results for plotting
o3_plot <- o3_monthly |>
  dplyr::transmute(
    month,
    pollutant = "O3",
    mean = mean_o3,
    lower = lower_o3,
    upper = upper_o3
  )

# Combine both pollutants
plot1_data <- dplyr::bind_rows(co_plot, o3_plot)

head(plot1_data)
dim(plot1_data)


plot1 <- ggplot(
  plot1_data,
  aes(x = month, y = mean, color = pollutant)
) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2
  ) +
  facet_wrap(
    ~ pollutant,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly CO and ozone concentrations",
    subtitle = "Pooled daily means with 95% confidence intervals",
    x = "Month",
    y = "Concentration (ppm)",
    color = "Pollutant"
  ) +
  theme_minimal()

plot1


# add calendar year
aq_all <- aq_all |>
  dplyr::mutate(
    year = as.integer(format(date, "%Y"))
  )

# calculate monthly PM2.5 means within each year
pm25_monthly <- aq_all |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    n_pm25 = sum(!is.na(pm25)),
    .groups = "drop"
  )

pm25_monthly

plot2 <- ggplot(
  pm25_monthly,
  aes(
    x = month,
    y = mean_pm25,
    color = factor(year),
    group = year
  )
) +
  geom_point(size = 2.5) +
  geom_line() +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly PM2.5 concentrations by year",
    x = "Month",
    y = "Mean PM2.5 concentration (ug/m3 LC)",
    color = "Year"
  ) +
  theme_minimal()

plot2
