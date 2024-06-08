install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("yardstick")
library(yardstick)
install.packages("caret")
library(caret)

df_booking <- read.csv("C:/Users/ridim/OneDrive/Documents/DMML/hotel_cancellation_prediction/booking.csv")
print(df_booking)
summary(df_booking)

str(df_booking)


df_booking <- subset(df_booking, select = -Booking_ID)

cat("Shape:", dim(df_booking), "\n")
head(df_booking)

#missing values
missing_values <- sort(colSums(is.na(df_booking)), decreasing = TRUE)
print("Missing Values for flight data:")
print(missing_values)

column_names <- names(df_booking)
print(column_names)

booking2_encoded <- booking2

#label encoding
columns_to_encode <- c("type.of.meal", "car.parking.space", "room.type", "market.segment.type", "booking.status")

for (col in columns_to_encode) {
  df_booking[[col]] <- as.integer(factor(df_booking[[col]]))
}

head(df_booking)
print(df_booking)
dim(df_booking)


#converting average price from float to integer
df_booking[["average.price"]] <- as.integer(round(df_booking[["average.price"]]))

str(df_booking)
#random forest selector for feature selection
install.packages("randomForest")
library(randomForest)
target_variable <- "booking.status"

rf_model <- randomForest(booking.status ~ ., data = df_booking, ntree = 50)
print(rf_model$importance)
varImpPlot(rf_model)

#top 10 features based on importance
k <- 10  
selected_features <- rownames(rf_model$importance)[order(rf_model$importance[, 1], decreasing = TRUE)[1:k]]
cat("Selected Features:", selected_features, "\n")


#fselector method
install.packages("FSelector")


library(FSelector)


df_booking$booking.status <- as.factor(df_booking$booking.status)

#features and target
features <- df_booking[, !(names(df_booking) %in% c("booking.status"))]
target <- df_booking$booking.status

#feature selection using Information Gain
selected_features <- information.gain(target ~ ., data = cbind(features, target))

#top 10 selected features
top_features <- head(selected_features, 10)
print(top_features)

df_cleanhotel <- df_booking
boxplot(df_cleanhotel$average.price, main = "Boxplot of average.price", ylab = "Average Price")

df_cleanhotel <- df_ridima[df_cleanhotel$average.price <= 400, ]
print("Total no of rows after removing outliers")
dim(df_cleanhotel)
str(df_cleanhotel)
#date column cleaning

df_cleanhotel$date.of.reservation <- as.Date(df_cleanhotel$date.of.reservation, format="%m/%d/%Y")

df_cleanhotel$date.of.reservation <- format(df_cleanhotel$date.of.reservation, "%Y-%m-%d")
df_cleanhotel
dim(df_cleanhotel)

#invalid dates
invalid_dates <- df_cleanhotel[is.na(df_cleanhotel$date.of.reservation) | !grepl("\\d{4}-\\d{2}-\\d{2}", df_cleanhotel$date.of.reservation), ]

print("Invalid dates:")

print(invalid_dates)
print("total no of invalid dates:")
dim(invalid_dates)
#drop invalid dates (38 rows)
df_cleanhotel <- df_cleanhotel[!(df_cleanhotel$date.of.reservation %in% invalid_dates$date.of.reservation), ]
print("total no of rows after removing invalid dates")
dim(df_cleanhotel)
print(dim(df_cleanhotel))
dim(df_cleanhotel)

library(dplyr)

df_cleanhotel$date.of.reservation <- as.Date(df_cleanhotel$date.of.reservation, format="%Y-%m-%d")

df_cleanhotel$year <- format(df_cleanhotel$date.of.reservation, "%Y")
df_cleanhotel$month <- format(df_cleanhotel$date.of.reservation, "%m")
df_cleanhotel$day <- format(df_cleanhotel$date.of.reservation, "%d")

#dropping the original date column
df_cleanhotel <- df_cleanhotel %>% select(-date.of.reservation)
df_cleanhotel
colnames(df_cleanhotel)
dim(df_cleanhotel)
head(df_cleanhotel)

#converting all columns to numeric and booking.factor to as factor
str(df_cleanhotel)
df_cleanhotel[] <- lapply(df_cleanhotel, as.numeric)

df_cleanhotel$booking.status <- as.factor(df_cleanhotel$booking.status)
str(df_cleanhotel)
#anova test
anova_results <- lapply(df_cleanhotel[sapply(df_cleanhotel, is.numeric) & names(df_cleanhotel) != "booking.status"], function(x) aov(x ~ booking.status, data = df_cleanhotel))

p_values <- list()

numeric_variable_names <- c("number.of.adults", "number.of.children", "number.of.weekend.nights",
                            "number.of.week.nights", "type.of.meal", "car.parking.space",
                            "room.type", "lead.time", "market.segment.type", "repeated",
                            "P.C", "P.not.C", "average.price", "special.requests",
                            "year", "month", "day")
for (variable in numeric_variable_names) {
  anova_results <- aov(as.formula(paste(variable, "~ booking.status")), data = df_cleanhotel)
  p_values[[variable]] <- summary(anova_results)[[1]][["Pr(>F)"]]
}

print(p_values)





# Print ANOVA results
for (variable in names(anova_result_numeric)) {
  cat("ANOVA for", variable, "\n")
  print(anova_result_numeric[[variable]])
  cat("\n")
}

#checking skewness
install.packages("moments")
library(moments)
df_skewhotel <- df_cleanhotel
#log transformation
df_skewhotel$number.of.week.nights<-log1p(df_skewhotel$number.of.week.nights)
df_skewhotel$number.of.children<-log1p(df_skewhotel$number.of.children)

df_skewhotel$car.parking.space<-log1p(df_skewhotel$car.parking.space)
df_skewhotel$lead.time<-log1p(df_skewhotel$lead.time)
df_skewhotel$repeated<-log1p(df_skewhotel$repeated)
df_skewhotel$P.C<-log1p(df_skewhotel$P.C)
df_skewhotel$P.not.C<-log1p(df_skewhotel$P.not.C)
df_skewhotel$special.requests<-log1p(df_skewhotel$special.requests)
#skewness after transformation
#checking skewness after log
skewness_values3 <- sapply(df_skewhotel, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA)
print("Skewness before after transformation")
print(skewness_values3)
#converting to 0 1
df_skewhotel$booking.status <- ifelse(df_skewhotel$booking.status == 2, 1, 0)


#model
df_ridmodel <- df_skewhotel
df_ridmodel$booking.status
columns_to_drop <- c("P.C", "P.not.C", "number.of.children")
install.packages("lightgbm")
library(lightgbm)
# Load required libraries
library(lightgbm)
library(dplyr)
library(caret)

#Separate into X and Y

Xhotel <- df_ridmodel %>%
  select(-c(P.C, number.of.children, P.not.C, booking.status))
dim(Xhotel)
colnames(Xhotel)
colnames(df_ridmodel)
Yhotel <- df_ridmodel$booking.status
write.csv(df_ridmodel, "C:/Users/ridim/OneDrive/Documents/DMML/new/hotel.csv", row.names = FALSE)


#split into X_train, X_test, Y_train, Y_test
set.seed(123)  # for reproducibility
train_indices <- createDataPartition(Yhotel, p = 0.7, list = FALSE)
X_trainhotel <- Xhotel[train_indices, ]
X_testhotel <- Xhotel[-train_indices, ]
Y_trainhotel <- Yhotel[train_indices]
Y_testhotel <- Yhotel[-train_indices]
dim(X_trainhotel)
dim(X_testhotel)
dim(Y_trainhotel)

#lightgbm
library(lightgbm)
library(caret)

Y_trainhotel <- as.factor(Y_trainhotel)

dim(X_trainhotel)
colnames(X_trainhotel)

dim(X_testhotel)
colnames(X_testhotel)

#LightGBM model
gb_model <- lgb.train(
  data = lgb.Dataset(data = as.matrix(X_trainhotel), label = as.numeric(Y_trainhotel) - 1),
  nrounds = 100
)

predictionslgbm <- predict(lgb_model, as.matrix(X_testhotel))

predictions1_lgb <- as.factor(ifelse(predictionslgbm > 0.5, 1, 0))
#if code then fails, then running this for getting on the same level
levels_combined <- union(levels(predictions1_lgb), levels(Y_testhotel))
predictions1_lgb <- factor(predictions1_lgb, levels = levels_combined)
#fin
Y_testhotel <- factor(Y_testhotel, levels = levels_combined)

confusion_matrixlgbm <- confusionMatrix(predictions1_lgb, Y_testhotel)
print("Evaluation metrics lightgbm")
print(confusion_matrixlgbm)
install.packages("pROC")
library(pROC)
#roc curve
roc_curve <- roc(Y_testhotel, as.numeric(predictions1_lgb))

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

abline(a = 0, b = 1, lty = 2, col = "red")

auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

#svm
library(e1071)
library(caret)

Y_trainhotel <- as.factor(Y_trainhotel)

dim(X_trainhotel)
colnames(X_trainhotel)
dim(X_testhotel)
colnames(X_testhotel)

svm_model <- svm(Y_trainhotel ~ ., data = X_trainhotel, kernel = "radial")

predictions_svm <- predict(svm_model, X_testhotel)

levels(predictions_svm) <- levels(Y_testhotel)

confusion_matrix_svm <- confusionMatrix(predictions_svm, Y_testhotel)
print("Evaluation metrics for svm for hotel reservation")
print(confusion_matrix_svm)
roc_curve_svm <- roc(Y_testhotel, as.numeric(predictions_svm))

plot(roc_curve_svm, main = "ROC Curve (SVM)", col = "green", lwd = 2)

abline(a = 0, b = 1, lty = 2, col = "red")

auc_value_svm <- auc(roc_curve_svm)
legend("bottomright", legend = paste("AUC =", round(auc_value_svm, 2)), col = "green", lwd = 2)

