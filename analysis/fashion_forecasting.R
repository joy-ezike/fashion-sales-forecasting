# Artifact for time series analysis of fashion trends using social media data
# Eyitemi Joy Ezike/ 30078244/ M.Sc. Data Science
# Load all the necessary libraries that will be used for this project
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("forecast")) install.packages("forecast")
if (!require("zoo")) install.packages("zoo")
if (!require("randomForest")) install.packages("randomForest")
if (!require("gbm")) install.packages("gbm")
if (!require("caret")) install.packages("caret")
if (!require("mlr")) install.packages("mlr")
if (!require("prophet")) install.packages("prophet")

library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)
library(randomForest)
library(gbm)
library(caret)
library(mlr)
library(prophet)

# Read the data file into a data frame name it fashion_data
fashion_data <- read.csv("C:\\Users\\personal\\OneDrive - University of South Wales\\R_Tutorials\\Fashion_data_2018_2022 EXCEL.csv", header = TRUE)

# Convert the month and year to a date object for easier analysis
fashion_data$date <- make_date(fashion_data$year_of_sale, fashion_data$month_of_sale)

# Data Cleaning and Consistency Checks
fashion_data <- fashion_data %>%
  mutate(
    month_of_sale = as.integer(month_of_sale),
    year_of_sale = as.integer(year_of_sale),
    price = as.numeric(price),
    sales_count = as.integer(sales_count)
  ) %>%
  filter(!is.na(sales_count))

# Perform Exploratory Data Analysis on the data frame
# Summary of the dataset
summary(fashion_data)

# Visualization of Sales Trends Over Time
sales_trends <- fashion_data %>%
  group_by(year_of_sale, month_of_sale) %>%
  summarise(Total_Sales = sum(sales_count), .groups = 'drop')

ggplot(sales_trends, aes(x = make_date(year_of_sale, month_of_sale), y = Total_Sales)) +
  geom_line() +
  labs(title = "Sales Trends Over Time", x = "Date", y = "Total Sales")

# Sales Distribution by various categories
categories <- c("category", "pattern", "age_group", "gender", "color", "product_name", "season", "material")
lapply(categories, function(cat) {
  fashion_data %>%
    group_by(!!sym(cat)) %>%
    summarise(Total_Sales = sum(sales_count), .groups = 'drop') %>%
    ggplot(aes(x = reorder(!!sym(cat), Total_Sales), y = Total_Sales, fill = !!sym(cat))) +
    geom_col() +
    labs(title = paste("Sales Distribution by", cat), x = cat, y = "Total Sales") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) %>%
    print()
})

# Boxplot of Sales by Month to see seasonality
fashion_data %>%
  mutate(Month = factor(month(date), levels = 1:12, labels = month.name)) %>%
  ggplot(aes(x = Month, y = sales_count, fill = Month)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Monthly Sales Distribution", x = "Month", y = "Sales")

#Perform Time Series Analysis
# Data Segmentation for the Time Series Analysis
fashion_segmented <- fashion_data %>%
  group_by(date) %>%
  summarise(Total_Sales = sum(sales_count), .groups = 'drop')

# Decomposing the Time Series of fashion data for Color and Pattern
monthly_sales_by_color <- fashion_data %>%
  group_by(color, date) %>%
  summarise(sales_count = sum(sales_count), .groups = 'drop')

monthly_sales_by_pattern <- fashion_data %>%
  group_by(pattern, date) %>%
  summarise(sales_count = sum(sales_count), .groups = 'drop')

# Function to perform and plot decomposition
plot_decomposition <- function(data, attribute_name, attribute_type) {
  filtered_data <- data %>%
    filter(!!sym(attribute_type) == attribute_name) %>%
    mutate(quarter = as.yearqtr(date)) %>%
    group_by(quarter) %>%
    summarise(sales_count = sum(sales_count), .groups = 'drop')
  
  if (nrow(filtered_data) < 8) {
    message(paste("Not enough data points for", attribute_type, ":", attribute_name))
    return()
  }
  
  ts_data <- ts(filtered_data$sales_count, start = c(year(min(filtered_data$quarter)), quarter(min(filtered_data$quarter))), frequency = 4)
  decomposition <- stl(ts_data, s.window = "periodic")
  plot(decomposition, main = paste("Seasonal Decomposition of", attribute_name, attribute_type, "Sales"))
}

# Apply decomposition function to each color and pattern
lapply(unique(monthly_sales_by_color$color), function(color) plot_decomposition(monthly_sales_by_color, color, "color"))
lapply(unique(monthly_sales_by_pattern$pattern), function(pattern) plot_decomposition(monthly_sales_by_pattern, pattern, "pattern"))

# Time Series Decomposition and Forecasting for Total Sales
sales_ts <- ts(fashion_segmented$Total_Sales, start = c(2018, 1), frequency = 12)
decomposed_sales <- stl(sales_ts, s.window = "periodic")
plot(decomposed_sales, main = "Decomposition of Total Monthly Sales")

# Building the Models and forecasting
# SARIMA Model
sarima_model <- auto.arima(sales_ts, seasonal = TRUE, stepwise = TRUE, approximation = FALSE, trace = TRUE)
future_sarima_forecast <- forecast(sarima_model, h = 48)
plot(future_sarima_forecast, main = "SARIMA Model Forecast for 4 Years", xlab = "Date", ylab = "Sales")
checkresiduals(sarima_model)

# STL + ARIMA Model
stl_decomp <- stl(sales_ts, s.window = "periodic")
sales_stl_adjusted <- seasadj(stl_decomp)
stl_arima_model <- auto.arima(sales_stl_adjusted)
stl_forecast <- forecast(stl_arima_model, h = 48)
stl_forecast$mean <- stl_forecast$mean + rep(stl_decomp$time.series[,"seasonal"], length.out = 48)
plot(stl_forecast, main = "STL + ARIMA Model Forecast", xlab = "Date", ylab = "Sales")
checkresiduals(stl_arima_model)

# ETS Model
ets_model <- ets(sales_ts)
ets_forecast <- forecast(ets_model, h = 48)
plot(ets_forecast, main = "ETS Model Forecast", xlab = "Date", ylab = "Sales")
checkresiduals(ets_model)

# Prophet Model
prophet_data <- data.frame(ds = as.Date(time(sales_ts)), y = as.numeric(sales_ts))
prophet_model <- prophet(prophet_data)
future <- make_future_dataframe(prophet_model, periods = 48, freq = 'month')
prophet_forecast <- predict(prophet_model, future)
prophet_plot <- plot(prophet_model, prophet_forecast) +
  ggtitle("Prophet Model Forecast") +
  ylab("Sales") + xlab("Date") + theme_minimal()
print(prophet_plot)
lines(as.Date(time(sales_ts)), sales_ts, col = "red")

# Calculate forecast accuracy on the test set
prophet_test <- data.frame(ds = as.Date(time(sales_ts)), y = as.numeric(sales_ts))
prophet_accuracy <- accuracy(prophet_forecast$yhat[1:length(sales_ts)], prophet_test$y)
print(prophet_accuracy)

#Building the Machine Learning models chosen
# Feature Engineering for Machine Learning Models
fashion_data <- fashion_data %>%
  mutate(
    month_sin = sin(2 * pi * month(date) / 12),
    month_cos = cos(2 * pi * month(date) / 12)
  )

# Splitting the Data into Training and Testing Sets
set.seed(123)
training_indices <- createDataPartition(fashion_segmented$Total_Sales, p = 0.8, list = FALSE)
train_data <- fashion_segmented[training_indices, ]
test_data <- fashion_segmented[-training_indices, ]

# Add features to the train and test data
train_data <- train_data %>%
  mutate(
    month_sin = sin(2 * pi * month(date) / 12),
    month_cos = cos(2 * pi * month(date) / 12)
  )

test_data <- test_data %>%
  mutate(
    month_sin = sin(2 * pi * month(date) / 12),
    month_cos = cos(2 * pi * month(date) / 12)
  )

# Linear Regression Model
lm_model <- lm(Total_Sales ~ month_sin + month_cos, data = train_data)
lm_predictions <- predict(lm_model, test_data)

# Random Forest Model
rf_model <- randomForest(Total_Sales ~ month_sin + month_cos, data = train_data, ntree = 500)
rf_predictions <- predict(rf_model, test_data)

# Gradient Boosting Model
gbm_model <- gbm(Total_Sales ~ month_sin + month_cos, data = train_data, distribution = "gaussian", n.trees = 1000, interaction.depth = 4)
gbm_predictions <- predict(gbm_model, newdata = test_data, n.trees = 1000)

# Combine the 3 ML Models using a Voting Regressor
models <- list(lm = lm_model, rf = rf_model, gbm = gbm_model)
predictions <- data.frame(
  lm = lm_predictions,
  rf = rf_predictions,
  gbm = gbm_predictions
)

# Voting mechanism: simple average ensemble
ensemble_predictions <- rowMeans(predictions)

# Calculating the Performance Assessments of all models used
actual <- test_data$Total_Sales

# Calculate the RMSE
lm_rmse <- sqrt(mean((lm_predictions - actual)^2))
rf_rmse <- sqrt(mean((rf_predictions - actual)^2))
gbm_rmse <- sqrt(mean((gbm_predictions - actual)^2))
ensemble_rmse <- sqrt(mean((ensemble_predictions - actual)^2))
sarima_rmse <- sqrt(mean((future_sarima_forecast$mean[1:length(actual)] - actual)^2))
stl_rmse <- sqrt(mean((stl_forecast$mean[1:length(actual)] - actual)^2))
ets_rmse <- sqrt(mean((ets_forecast$mean[1:length(actual)] - actual)^2))
prophet_rmse <- sqrt(mean((prophet_forecast$yhat[1:length(actual)] - actual)^2))

# Calculate the MAE
lm_mae <- mean(abs(lm_predictions - actual))
rf_mae <- mean(abs(rf_predictions - actual))
gbm_mae <- mean(abs(gbm_predictions - actual))
ensemble_mae <- mean(abs(ensemble_predictions - actual))
sarima_mae <- mean(abs(future_sarima_forecast$mean[1:length(actual)] - actual))
stl_mae <- mean(abs(stl_forecast$mean[1:length(actual)] - actual))
ets_mae <- mean(abs(ets_forecast$mean[1:length(actual)] - actual))
prophet_mae <- mean(abs(prophet_forecast$yhat[1:length(actual)] - actual))

# Calculate the MPE
lm_mpe <- mean((lm_predictions - actual) / actual) * 100
rf_mpe <- mean((rf_predictions - actual) / actual) * 100
gbm_mpe <- mean((gbm_predictions - actual) / actual) * 100
ensemble_mpe <- mean((ensemble_predictions - actual) / actual) * 100
sarima_mpe <- mean((future_sarima_forecast$mean[1:length(actual)] - actual) / actual) * 100
stl_mpe <- mean((stl_forecast$mean[1:length(actual)] - actual) / actual) * 100
ets_mpe <- mean((ets_forecast$mean[1:length(actual)] - actual) / actual) * 100
prophet_mpe <- mean((prophet_forecast$yhat[1:length(actual)] - actual) / actual) * 100

# Create a comparison table for the accuracy metrics
accuracy_metrics <- data.frame(
  Model = c("Linear Regression", "Random Forest", "Gradient Boosting", "Voting Regressor", "SARIMA", "STL + ARIMA", "ETS", "Prophet"),
  RMSE = c(lm_rmse, rf_rmse, gbm_rmse, ensemble_rmse, sarima_rmse, stl_rmse, ets_rmse, prophet_rmse),
  MAE = c(lm_mae, rf_mae, gbm_mae, ensemble_mae, sarima_mae, stl_mae, ets_mae, prophet_mae),
  MPE = c(lm_mpe, rf_mpe, gbm_mpe, ensemble_mpe, sarima_mpe, stl_mpe, ets_mpe, prophet_mpe)
)

print(accuracy_metrics)

# Plot the results to visualize how each model performed against the actual sales
par(mar = c(5, 4, 4, 8), xpd = TRUE)
plot(actual, type = 'l', col = 'black', ylim = range(c(actual, ensemble_predictions)), ylab = "Sales", xlab = "Date", main = "Models Versus Actual Sales Data")
lines(lm_predictions, col = 'red')
lines(rf_predictions, col = 'blue')
lines(gbm_predictions, col = 'green')
lines(ensemble_predictions, col = 'purple')
lines(future_sarima_forecast$mean[1:length(actual)], col = 'cyan')
lines(stl_forecast$mean[1:length(actual)], col = 'pink')
lines(ets_forecast$mean[1:length(actual)], col = 'orange')
lines(prophet_forecast$yhat[1:length(actual)], col = 'brown')
legend("topright", inset = c(-0.3, 0), legend = c("Actual", "LM", "RF", "GBM", "Ensemble", "SARIMA", "STL + ARIMA", "ETS", "Prophet"), col = c("black", "red", "blue", "green", "purple", "cyan", "pink", "orange", "brown"), lty = 1)

# Forecasting using the Ensemble Model
future_dates <- seq.Date(from = max(fashion_data$date) + months(1), by = "month", length.out = 48)
future_features <- data.frame(
  date = future_dates,
  month_sin = sin(2 * pi * month(future_dates) / 12),
  month_cos = cos(2 * pi * month(future_dates) / 12)fas
)

lm_future_predictions <- predict(lm_model, newdata = future_features)
rf_future_predictions <- predict(rf_model, newdata = future_features)
gbm_future_predictions <- predict(gbm_model, newdata = future_features, n.trees = 1000)

future_ensemble_predictions <- rowMeans(data.frame(
  lm = lm_future_predictions,
  rf = rf_future_predictions,
  gbm = gbm_future_predictions
))

plot(future_dates, future_ensemble_predictions, type = 'l', col = 'purple', ylim = range(future_ensemble_predictions), ylab = "Sales", xlab = "Date", main = "Ensemble Model Forecast for 4 Years")
lines(future_dates, lm_future_predictions, col = 'red', lty = 2)
lines(future_dates, rf_future_predictions, col = 'blue', lty = 2)
lines(future_dates, gbm_future_predictions, col = 'green', lty = 2)
legend("topright", inset = c(-0.3, 0), legend = c("Ensemble", "LM", "RF", "GBM"), col = c("purple", "red", "blue", "green"), lty = 1:2)


