df_data3 <- read.csv("C:/Users/ridim/OneDrive/Documents/DMML/flight_price_prediction.csv")
head(df_data3, 5)
print("Count of duplicate values in flight data")
sum(duplicated(df_data3))
df_data3 <- df_data3[, !(names(df_data3) %in% c("X", "flight"))]
df_data3
dim(df_data3)
print("Count of missing values:")
features_with_na <- names(df_data3)[colSums(is.na(df_data3)) > 0]
print("Missing values for flight data")
features_with_na

df_encoded <- df_data3
dim(df_encoded)
#visualisation 2
avg_price <- aggregate(price ~ airline, df_data3, mean)
avg_price <- avg_price[order(-avg_price$price),]

colors <- c('#4C72B0', '#55A868', '#C44E52', '#8172B2', '#CCB974', '#64B5CD')
barplot(avg_price$price, names.arg = avg_price$airline, col = colors,
        main = "Average Ticket Price by Airline", xlab = "Airline", ylab = "Average Price")

#specify categorical columns
categorical_columns <- c("airline", "source_city", "departure_time", 
                         "stops", "arrival_time", "destination_city", "class")

#performing label encoding for each categorical column
for (col in categorical_columns) {
  df_encoded[[paste0(col, "_encoded")]] <- as.numeric(factor(df_encoded[[col]], levels = unique(df_encoded[[col]])))
}

#dropping the original categorical columns as new columns are created
df_encoded <- df_encoded[, !colnames(df_encoded) %in% categorical_columns]


df_encoded <- data.frame(lapply(df_encoded, function(x) as.numeric(as.character(x))))
str(df_encoded)
#feature selection using information gain
install.packages("randomForest")
library(randomForest)
target_variable <- "price"

rf_model <- randomForest(price ~ ., data = df_encoded, ntree = 50)
print(rf_model$importance)
varImpPlot(rf_model)

k <- 10  
selected_features <- rownames(rf_model$importance)[order(rf_model$importance[, 1], decreasing = TRUE)[1:k]]
cat("Selected Features:", selected_features, "\n")

df_outliers2 <- df_encoded
numeric_columns2 <- df_outliers2[sapply(df_outliers2, is.numeric)]

count_outliers <- function(x, multiplier = 3) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr
  x_outliers <- x < lower_bound | x > upper_bound
  num_outliers <- sum(x_outliers, na.rm = TRUE)
  return(num_outliers)
}

outliers_count <- sapply(numeric_columns2, count_outliers)

for (i in seq_along(outliers_count)) {
  cat("Variable:", names(outliers_count)[i], "\n")
  cat("Number of Outliers:", outliers_count[[i]], "\n\n")
}

colnames(df_outliers2)
#removing outliers
df_outliers2 <- df_outliers2[df_outliers2$duration <= 42, ]
print("After removing outliers")
dim(df_outliers2)
library(e1071)
#checking skewness
skewness_values3 <- sapply(df_outliers2, skewness)

print(skewness_values3)
numeric_colsnew <- sapply(df_outliers2, is.numeric)
df_outliers2[numeric_colsnew] <- lapply(df_outliers2[numeric_colsnew], log1p)
skewness_values2 <- sapply(df_outliers2, skewness)

print(skewness_values2)



numerical_colsridima <- names(df_outliers2)[sapply(df_outliers2, is.numeric)]


#random forest for feature selection
install.packages("randomForest")
library(randomForest)
target_variable <- "price"

rf_model <- randomForest(price ~ ., data = df_label_new, ntree = 50)
print(rf_model$importance)
varImpPlot(rf_model)

#top 10 features based on importance
k <- 10  
selected_features <- rownames(rf_model$importance)[order(rf_model$importance[, 1], decreasing = TRUE)[1:k]]
cat("Selected Features:", selected_features, "\n")

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)
set.seed(123)




#linear method main
df_outliers2$price <- as.numeric(as.character(df_outliers2$price))
str(df_outliers2)

Yridima <- df_outliers2$price
str(Yridima)
Xridima <- df_outliers2[, c('duration','airline_encoded','source_city_encoded','stops_encoded','arrival_time_encoded','destination_city_encoded','class_encoded','days_left')]
dim(Xridima)
set.seed(123)
splitIndex <- createDataPartition(y = Yridima, p = 0.7, list = FALSE,times=1)
x_train_ridima <- Xridima[splitIndex, ]
x_test_ridima <- Xridima[-splitIndex, ]
y_train_ridima <- Yridima[splitIndex]
y_test_ridima <- Yridima[-splitIndex]
str(y_train_ridima)
dim(x_train_ridima)
dim(y_train_ridima)
lm_modelridima <- lm(y_train_ridima ~ ., data = x_train_ridima)


y_predridima <- predict(lm_modelridima, newdata = x_test_ridima)
summary(lm_modelridima)
str(X_data)
str(Y_data)


#R-squared

mseridima <- mean((y_predridima - y_test_ridima)^2)
rmseridima <- sqrt(mseridima)
print("Summary for linear regression for flight prediction")
summary_lm <- summary(lm_modelridima)



cat("Mean Squared Error (MSE) for linear regression for flight:", format(mseridima, digits = 2), "\n")
cat("Root Mean Squared Error (RMSE) for linear regression for flight:", format(rmseridima, digits = 2), "\n")
maeridima <- mean(abs(y_predridima - y_test_ridima))
cat("Mean Absolute Error (MAE) for linear regression for flight:", format(maeridima, digits = 2), "\n")
#qqplot
residuals_lm <- residuals(lm_modelridima)

qqnorm(residuals_lm)
qqline(residuals_lm)



#random forest
install.packages("randomForest")
library(randomForest)


rf_model_ridima <- randomForest(y_train_ridima ~ ., data = x_train_ridima, ntree = 50)

predictions_rf_ridima <- predict(rf_model_ridima, newdata = x_test_ridima)


mse_rf_ridima <- mean((predictions_rf_ridima - y_test_ridima)^2)

rmse_rf_ridima <- sqrt(mse_rf_ridima)

mae_rf_ridima <- mean(abs(predictions_rf_ridima - y_test_ridima))
rss_rf_ridima <- sum((y_test_ridima - predictions_rf_ridima)^2)
tss_rf_ridima <- sum((y_test_ridima - mean(y_test_ridima))^2)
r_squared_rf_ridima <- 1 - (rss_rf_ridima / tss_rf_ridima)

cat("R-squared for random forest for flight price prediction:", format(r_squared_rf_ridima, digits = 4), "\n")

# Display the evaluation metrics
cat("Mean Squared Error (MSE) for random forest for flight price :", format(mse_rf_ridima, digits = 2), "\n")
cat("Root Mean Squared Error (RMSE) for flight price:", format(rmse_rf_ridima, digits = 2), "\n")
cat("Mean Absolute Error (MAE) for flight price:", format(mae_rf_ridima, digits = 2), "\n")
summary(rf_model_ridima)
print(rf_model_ridima)

residuals <- y_train_ridima - y_pred_ridima

tss <- sum((y_train_ridima - mean(y_train_ridima))^2)

rss <- sum(residuals^2)

r_squared <- 1 - (rss / tss)

cat("R-squared:", format(r_squared_rf_ridima, digits = 4), "\n")

summary_rf <- summary(rf_model_ridima)
print(summary_rf)
residuals_rf <- residuals(rf_model_ridima)

any(is.na(residuals_rf))

str(residuals_rf)
summary(residuals_rf)
