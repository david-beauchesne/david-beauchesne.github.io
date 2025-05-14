# Install necessary packages if not already installed
install.packages(c("tidyverse", "lubridate", "forecast", "segmented"))

# Load libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(segmented)

# Example data loading
# Assume `data` is a dataframe with columns: date, public_health_unit, cases, deaths
# data <- read.csv("path_to_your_data.csv")

# Example data
set.seed(123)
data <- data.frame(
  date = seq(as.Date("2020-01-01"), by = "day", length.out = 1000),
  public_health_unit = rep(c("Unit1", "Unit2"), each = 500),
  cases = c(rpois(500, lambda = 10), rpois(500, lambda = 20)),
  deaths = c(rpois(500, lambda = 1), rpois(500, lambda = 2))
)


data <- dat |>
  dplyr::select(date, public_health_unit = sub_region_1, name, outcomes_per_100000) |>
  tidyr::pivot_wider(id_cols = c("date", "public_health_unit"), names_from = name, values_from = outcomes_per_100000)



# Convert date to Date type
data$date <- as.Date(data$date)

# Filter for a specific public health unit (example: "Unit1")
unit_data <- data %>% filter(public_health_unit == "4831")

# Plot cases and deaths over time
ggplot(unit_data, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue") +
  geom_line(aes(y = deaths), color = "red") +
  labs(
    title = "COVID-19 Cases and Deaths Over Time",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()

# Define the intervention dates
intervention_dates <- c(as.Date("2021-12-31"), as.Date("2022-12-31"), as.Date("2023-12-31"))
intervention_dates <- man$New

# Create binary variables for each intervention
unit_data <- unit_data %>%
  mutate(
    intervention1 = ifelse(date >= intervention_dates[1], 1, 0),
    intervention2 = ifelse(date >= intervention_dates[2], 1, 0),
    intervention3 = ifelse(date >= intervention_dates[3], 1, 0)
  )

# Create a time variable
unit_data <- unit_data %>%
  mutate(time = 1:nrow(unit_data))

# Fit a segmented regression model with multiple interventions
fit_cases <- lm(cases ~ time + intervention1 + I(time * intervention1) +
  intervention2 + I(time * intervention2) +
  intervention3 + I(time * intervention3), data = unit_data)

fit_deaths <- lm(deaths ~ time + intervention1 + I(time * intervention1) +
  intervention2 + I(time * intervention2) +
  intervention3 + I(time * intervention3), data = unit_data)

# Summarize the models
summary(fit_cases)
summary(fit_deaths)

# Plot the fitted values
unit_data <- unit_data %>%
  mutate(
    fitted_cases = predict(fit_cases, newdata = unit_data),
    fitted_deaths = predict(fit_deaths, newdata = unit_data)
  )

ggplot(unit_data, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue") +
  geom_line(aes(y = fitted_cases), color = "blue", linetype = "dashed") +
  geom_line(aes(y = deaths), color = "red") +
  geom_line(aes(y = fitted_deaths), color = "red", linetype = "dashed") +
  labs(
    title = "Interrupted Time Series Analysis of COVID-19 Cases and Deaths",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()


# Convert date to Date type
data$date <- as.Date(data$date)


# Filter for a specific public health unit (example: "Unit1")
unit_data <- data %>% filter(public_health_unit == "Unit1")

# Plot cases and deaths over time
ggplot(unit_data, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue") +
  geom_line(aes(y = deaths), color = "red") +
  labs(
    title = "COVID-19 Cases and Deaths Over Time",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()

# Define the intervention date
intervention_date <- as.Date("2020-09-01")

# Create a binary variable for intervention
unit_data <- unit_data %>%
  mutate(intervention = ifelse(date >= intervention_date, 1, 0))

# Create a time variable
unit_data <- unit_data %>%
  mutate(time = 1:nrow(unit_data))

# Fit a segmented regression model
fit_cases <- lm(cases ~ time + intervention + I(time * intervention), data = unit_data)
fit_deaths <- lm(deaths ~ time + intervention + I(time * intervention), data = unit_data)

# Summarize the models
summary(fit_cases)
summary(fit_deaths)

# Plot the fitted values
unit_data <- unit_data %>%
  mutate(
    fitted_cases = predict(fit_cases, newdata = unit_data),
    fitted_deaths = predict(fit_deaths, newdata = unit_data)
  )

ggplot(unit_data, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue") +
  geom_line(aes(y = fitted_cases), color = "blue", linetype = "dashed") +
  geom_line(aes(y = deaths), color = "red") +
  geom_line(aes(y = fitted_deaths), color = "red", linetype = "dashed") +
  labs(
    title = "Interrupted Time Series Analysis of COVID-19 Cases and Deaths",
    x = "Date",
    y = "Count"
  ) +
  theme_minimal()
