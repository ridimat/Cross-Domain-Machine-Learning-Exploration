df_salary <- read.csv("C:/Users/ridim/OneDrive/Documents/Data Mining and Machine Learning/salary.csv")
head(df_salary, 5)
print(df_salary)

#categories on raw data
categorical_columns <- sapply(df_salary, function(x) is.character(x) || is.factor(x))
categorical_data <- df_salary[, categorical_columns]
unique_categories <- lapply(categorical_data, function(x) unique(x))
print(unique_categories)
summary(df_salary)
str(df_salary)

#checking for missing values

cat(paste("\033[94mNumber of duplicate entries in the dataset are: ", sum(duplicated(df_salary)), "\n"))
cat(paste("\033[94mNumber of missing values in the dataset are: ", sum(is.na(df_salary)), "\n"))

#removing duplicates

df_salary <- df_salary[!duplicated(df_salary), ]
cat("\033[94mNumber of rows after removing duplicates: ", dim(df_salary), "\n")
dim(df_salary)

#making a new df for preprocessing
df_cleansalary <- df_salary
dim(df_cleansalary)

#identifying character columns
char_cols <- sapply(df_cleansalary, is.character)
print(char_cols)
#remove extra spaces from character columns
df_cleansalary[, char_cols] <- lapply(df_cleansalary[, char_cols], trimws)

#replace '?' with 'NA'
df_cleansalary[df_cleansalary == '?'] <- NA
dim(df_cleansalary)

missing_values <- colSums(is.na(df_cleansalary))
#replacing missing values with "Unknown" label in each feature
df_cleansalary$workclass[is.na(df_cleansalary$workclass)] <- "Unknown Class"
df_cleansalary$occupation[is.na(df_cleansalary$occupation)] <- "Unknown Occupation"
df_cleansalary$native.country[is.na(df_cleansalary$native.country)] <- "Unknown Country"
#checking whether the modification has been made or not
print("No. of NA values")
print(colSums(is.na(df_cleansalary))[colSums(is.na(df_cleansalary)) > 0])
print(df_cleansalary)
dim(df_cleansalary)


if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
#replacing labels
df_cleansalary <- df_cleansalary %>%
  mutate(
    workclass = case_when(
      workclass == 'State-gov' | workclass == 'Federal-gov' | workclass == 'Local-gov' ~ 'Govt.',
      workclass == 'Self-emp-not-inc' | workclass == 'Self-emp-inc' ~ 'self_emp',
      workclass == 'Without-pay' | workclass == 'Never-worked' ~ 'Unknown Class',
      TRUE ~ workclass
    ),
    marital.status = case_when(
      marital.status %in% c('Married-civ-spouse', 'Married-AF-spouse') ~ 'Married',
      marital.status %in% c('Divorced', 'Married-spouse-absent', 'Separated', 'Widowed') ~ 'DASW',
      TRUE ~ marital.status
    ),
    occupation = case_when(
      occupation %in% c('Adm-clerical', 'Exec-managerial', 'Handlers-cleaners', 'Prof-specialty') ~ c('Adminstration', 'Executive', 'Handlers', 'Profesionals')[match(occupation, c('Adm-clerical', 'Exec-managerial', 'Handlers-cleaners', 'Prof-specialty'))],
      occupation == 'Other-service' ~ 'Unknown Occupation',
      occupation == 'Craft-repair' ~ 'Repairing',
      occupation == 'Farming-fishing' ~ 'Farming',
      occupation == 'Transport-moving' ~ 'Transportation',
      occupation == 'Machine-op-inspct' ~ 'MachineOp',
      occupation == 'Protective-serv' ~ 'ProtectiveServ',
      occupation == 'Priv-house-serv' ~ 'HouseServ',
      TRUE ~ occupation
    ),
    native.country = case_when(
      native.country == 'United-States' ~ 'USA',
      native.country == 'South' ~ 'SouthKorea',
      native.country == 'Puerto-Rico' ~ 'PuertoRico',
      native.country == 'Dominican-Republic' ~ 'DominicRep',
      native.country == 'Outlying-US(Guam-USVI-etc)' ~ 'OutlyingUSA',
      native.country == 'Trinadad&Tobago' ~ 'Tri&Tob',
      native.country == 'Holand-Netherlands' ~ 'Netherlands',
      native.country == 'Hong' ~ 'HongKong',
      TRUE ~ native.country
    ),
    race = case_when(
      race %in% c('Asian-Pac-Islander', 'Amer-Indian-Eskimo') ~ c('APAC', 'NatAm')[match(race, c('Asian-Pac-Islander', 'Amer-Indian-Eskimo'))],
      TRUE ~ race
    )
  )
df_cleansalary
categorical_columns <- sapply(df_cleansalary, function(x) is.character(x) || is.factor(x))
categorical_data <- df_cleansalary[, categorical_columns]
unique_categories <- lapply(categorical_data, function(x) unique(x))
print(unique_categories)

#feature selection
library(FSelector)
df_cleansalary$salary <- as.factor(df_cleansalary$salary)

features <- df_cleansalary[, !(names(df_cleansalary) %in% c("salary"))]
target <- df_cleansalary$salary
#feature selection using information gain
selected_features <- information.gain(target ~ ., data = cbind(features, target))

#top 10 selected features
top_features <- head(selected_features, 15)
print(top_features)

top_feature_names <- rownames(top_features)
print(top_feature_names)


#numerical features
num_df <- df_cleansalary[, c('age', 'fnlwgt', 'capital.gain', 'capital.loss', 'hours.per.week')]

#categorical features
cat_df <- df_cleansalary[, c('workclass', 'education', 'education.num', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'native.country', 'salary')]
print(num_df)
print(cat_df)
df_cleansalary$salary
#label encoding
label_encode <- function(column) {
  as.numeric(as.factor(column))
}

cat_df_encoded <- cat_df %>%
  mutate(across(everything(), label_encode))
main_dfridima <- cbind(num_df, cat_df_encoded)
head(main_dfridima, 5)
print(main_dfridima)
str(main_dfridima)
main_dfridima$salary <- ifelse(main_dfridima$salary == 1, 0, 1)
main_dfridima$salary
dim(main_dfridima)

install.packages("moments")
library(moments)
str(main_dfridima)
#converting all variables to numeric
main_dfridima$workclass <- as.numeric(main_dfridima$workclass)

#'education' to numeric
main_dfridima$education <- as.numeric(main_dfridima$education)

#'marital.status' to numeric
main_dfridima$marital.status <- as.numeric(main_dfridima$marital.status)

#'occupation' to numeric
main_dfridima$occupation <- as.numeric(main_dfridima$occupation)

#'relationship' to numeric
main_dfridima$relationship <- as.numeric(main_dfridima$relationship)

#'race' to numeric
main_dfridima$race <- as.numeric(main_dfridima$race)

#'sex' to numeric
main_dfridima$sex <- as.numeric(main_dfridima$sex)

#'native.country' to numeric
main_dfridima$native.country <- as.numeric(main_dfridima$native.country)


#anova
#anova
print(df_final)
#convertin 'salary' to a factor
main_dfridima$salary <- as.factor(main_dfridima$salary)
#age
main_dfridima$salary <- as.factor(main_dfridima$salary)
anova_result <- aov(age ~ salary, data = main_dfridima)
anova_summary <- summary(anova_result, stars = TRUE)
print(anova_summary)
#fnlwgt
main_dfridima$salary <- as.factor(main_dfridima$salary)
anova_result_fnlwgt <- aov( salary~ main_dfridima$fnlwgt, data = main_dfridima)
anova_summary_fnlwgt <- summary(anova_result, stars = TRUE)
print(anova_summary_fnlwgt)
#capital gain
main_dfridima$salary <- as.factor(main_dfridima$salary)
anova_result_capitalgain <- aov(salary~capital.gain , data = main_dfridima)
anova_summary_capitalgain <- summary(anova_result, stars = TRUE)
print(anova_summary_capitalgain)
#capital loss
main_dfridima$salary <- as.factor(main_dfridima$salary)
anova_result_capitalloss <- aov(capital.loss ~ salary, data = main_dfridima)
anova_summary_capitalloss <- summary(anova_result, stars = TRUE)
print(anova_summary_capitalloss)
#anova for all variables
#ANOVA for 'age'
anova_age <- aov(salary ~ age, data = main_dfridima)
cat("\nANOVA for age:\n")
print(summary(anova_age))

#ANOVA for 'workclass'
anova_workclass <- aov(salary ~ workclass, data = main_dfridima)
cat("\nANOVA for workclass:\n")
print(summary(anova_workclass))

#ANOVA for 'fnlwgt'
anova_fnlwgt <- aov(salary ~ fnlwgt, data = main_dfridima)
cat("\nANOVA for fnlwgt:\n")
print(summary(anova_fnlwgt))

#ANOVA for 'capital.gain'
anova_capitalgain <- aov(salary ~ capital.gain, data = main_dfridima)
cat("\nANOVA for capital.gain:\n")
print(summary(anova_capitalgain))

#ANOVA for 'capital.loss'
anova_capitalloss <- aov(salary ~ capital.loss, data = main_dfridima)
cat("\nANOVA for capital.loss:\n")
print(summary(anova_capitalloss))

#ANOVA for 'hours.per.week'
anova_hoursperweek <- aov(salary ~ hours.per.week, data = main_dfridima)
cat("\nANOVA for hours.per.week:\n")
print(summary(anova_hoursperweek))

#ANOVA for 'education'
anova_education <- aov(salary ~ education, data = main_dfridima)
cat("\nANOVA for education:\n")
print(summary(anova_education))

#ANOVA for 'education.num'
anova_educationnum <- aov(salary ~ education.num, data = main_dfridima)
cat("\nANOVA for education.num:\n")
print(summary(anova_educationnum))

#ANOVA for 'marital.status'
anova_maritalstatus <- aov(salary ~ marital.status, data = main_dfridima)
cat("\nANOVA for marital.status:\n")
print(summary(anova_maritalstatus))

#ANOVA for 'occupation'
anova_occupation <- aov(salary ~ occupation, data = main_dfridima)
cat("\nANOVA for occupation:\n")
print(summary(anova_occupation))

#ANOVA for 'relationship'
anova_relationship <- aov(salary ~ relationship, data = main_dfridima)
cat("\nANOVA for relationship:\n")
print(summary(anova_relationship))

#ANOVA for 'race'
anova_race <- aov(salary ~ race, data = main_dfridima)
cat("\nANOVA for race:\n")
print(summary(anova_race))

#ANOVA for 'native.country'
anova_nativecountry <- aov(salary ~ native.country, data = main_dfridima)
cat("\nANOVA for native.country:\n")
print(summary(anova_nativecountry))

#'salary' to numeric
main_dfridima$salary <- as.numeric(as.character(main_dfridima$salary))
main_dfridima$capital.gain <- as.numeric(main_dfridima$capital.gain)
main_dfridima$capital.loss <- as.numeric(main_dfridima$capital.loss)
main_dfridima$hours.per.week <- as.numeric(main_dfridima$hours.per.week)
main_dfridima$age <- as.numeric(main_dfridima$age)
main_dfridima$fnlwgt <- as.numeric(main_dfridima$fnlwgt)
str(main_dfridima)
#checking skewness
skewness_values5 <- sapply(main_dfridima, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA)
print("Skewness before log transformation")
print(skewness_values5)
#removing outliers

main_dfridima <- main_dfridima[!(main_dfridima$capital.loss >= 4356), ]
print("After removing outliers for capital loss:")
dim(main_dfridima)

#remove rows where age is equal to 87 or 86
main_dfridima <- main_dfridima[!(main_dfridima$age %in% c(87, 86)), ]
print("After removing outliers for age:")
dim(main_dfridima)

#log transformation
main_dfridima$fnlwgt<-log1p(main_dfridima$fnlwgt)
main_dfridima$capital.loss<-log1p(main_dfridima$capital.loss)
main_dfridima$capital.gain<-log1p(main_dfridima$capital.gain)
#checking skewness after log
skewness_values6 <- sapply(main_dfridima, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA)
print("Skewness after log transformation")
print(skewness_values6)

#modelling
main_dfridima
main_main <- main_dfridima
#reverifying
main_main$salary <- ifelse(main_main$salary == 1, 0, 1)

head(main_main)
#final code
#decision tree 
install.packages("tree")
install.packages("rpart")
install.packages("xgboost")
install.packages("ada")
install.packages("randomForest")
install.packages("e1071")
install.packages("caret")

library(rpart)
library(xgboost)
library(ada)
library(randomForest)
library(e1071)
library(caret)

Xcal <- main_dfridima[, !(names(main_dfridima) %in% c("fnlwgt", "sex", "race", "salary"))]
dim(Xcal)
#Y contains only the 'salary' column
Ycal <- main_dfridima$salary
dim(Ycal)

#splitting into test and train
set.seed(123)  
split_index <- sample(1:nrow(main_dfridima), 0.8 * nrow(main_dfridima))
X_traincal <- Xcal[split_index, ]
X_testcal <- Xcal[-split_index, ]
Y_traincal <- Ycal[split_index]
Y_testcal <- Ycal[-split_index]
dim(X_traincal)
dim(X_testcal)
install.packages("rpart")
library(rpart)

tree_model <- rpart(Y_traincal ~ ., data = cbind(X_traincal, Y_traincal), method = "class")

predictions <- predict(tree_model, cbind(X_testcal), type = "class")

accuracy <- sum(predictions == Y_testcal) / length(Y_testcal)
cat("Accuracy of the Decision Tree model:", accuracy, "\n")

predictions <- as.factor(predictions)
Y_testcal <- as.factor(Y_testcal)

conf_matrixdec <- confusionMatrix(predictions, Y_testcal)
print("Evaluation metrics for decision tree for salary classification")
print(conf_matrixdec)
#roc
install.packages("pROC")
library(pROC)

probabilities <- predict(tree_model, cbind(X_testcal), type = "prob")

roc_curve <- roc(Y_testcal, probabilities[, "1"])

plot(roc_curve, main = "ROC Curve for Decision Tree Model", col = "blue", lwd = 2)

auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue", cex = 1.2)

abline(a = 0, b = 1, col = "gray", lty = 2)




#xgb
install.packages("xgboost")
library(xgboost)
library(caret)

Y_traincal <- as.factor(Y_traincal)
Y_testcal <- as.factor(Y_testcal)

#only for xgb refression for same levels (0,`1)`
Y_traincal <- as.factor(Y_traincal)
Y_traincal <- as.numeric(Y_traincal) - 1  

nrounds <- 100

xgb_model <- xgboost(data = as.matrix(X_traincal), label = Y_traincal,
                     objective = "binary:logistic", nrounds = nrounds)


xgb_predictions <- predict(xgb_model, as.matrix(X_testcal))
xgb_predictions <- ifelse(xgb_predictions > 0.5, 1, 0)  1

xgb_predictions <- as.factor(ifelse(xgb_predictions > 0.5, 1, 0))
Y_testcal <- as.factor(Y_testcal)

conf_matrix_xgb <- confusionMatrix(xgb_predictions, Y_testcal)
print("Evaluation metrics for xgb for salary classification")
print(conf_matrix_xgb)

#roc curve
library(pROC)

library(pROC)

roc_curve_xgb <- roc(as.numeric(as.character(Y_testcal)), as.numeric(as.character(xgb_predictions)))

plot(roc_curve_xgb, col = "blue", main = "ROC Curve", lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c(paste("XGBoost (AUC =", round(auc(roc_curve_xgb), 2), ")")), col = c("blue"), lty = 1, lwd = 2)



#ada
install.packages("adabag")
library(adabag)
library(caret)

# Convert Y_train and Y_test to factors
Y_traincal <- as.factor(Y_traincal)
Y_testcal <- as.factor(Y_testcal)


Y_traincal <- as.factor(Y_traincal)
Y_traincal <- as.numeric(Y_traincal) - 1  

library(ada)

ada_model <- ada(class ~ ., data = data.frame(class = Y_traincal, X_traincal), iter = 100)

ada_predictions <- predict(ada_model, data.frame(X_testcal))

ada_predictions <- as.factor(ifelse(ada_predictions == "1", 1, 0))

Y_testcal <- as.factor(Y_testcal)

conf_matrix_ada <- confusionMatrix(ada_predictions, Y_testcal)
print("Evaluation metrics for ada boost for salary classfication")
print(conf_matrix_ada)
#roc

#roc final
library(pROC)

roc_curve_ada <- roc(as.numeric(as.character(Y_testcal)), as.numeric(as.character(ada_predictions)))

plot(roc_curve_ada, col = "green", main = "ROC Curve", lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2, lwd = 2)

legend("bottomright", legend = c(paste("AdaBoost (AUC =", round(auc(roc_curve_ada), 2), ")")), col = c("green"), lty = 1, lwd = 2)


#naive
library(e1071)
library(e1071)

naive_model <- naiveBayes(as.factor(Y_traincal) ~ ., data = X_traincal)

naive_predictions <- predict(naive_model, newdata = X_testcal)
print("Evaluation metrics for naive bayes for salary classification")
print(confusionMatrix(naive_predictions, Y_testcal))
library(caret)
#roc
library(pROC)



roc_curve_naive <- roc(as.numeric(as.character(Y_testcal)), as.numeric(as.character(naive_predictions)))

plot(roc_curve_naive, col = "blue", main = "ROC Curve (Naive Bayes)", lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c(paste("Naive Bayes (AUC =", round(auc(roc_curve_naive), 2), ")")), col = c("blue"), lty = 1, lwd = 2)






#knn
install.packages("class")
library(class)

k <- 5

knn_model <- knn(train = as.matrix(X_traincal), test = as.matrix(X_testcal), cl = Y_traincal, k = k)

conf_matrix_knn <- confusionMatrix(knn_model, Y_testcal)
print("Evaluations metrics for knn for salary classfication")
print(conf_matrix_knn)
#roc curve
knn_predictions <- factor(knn_model, levels = levels(Y_testcal))

roc_curve_knn <- roc(as.numeric(as.character(Y_testcal)), as.numeric(as.character(knn_predictions)))
plot(roc_curve_knn, col = "blue", main = "ROC Curve (KNN)", lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2, lwd = 2)

auc_value_knn <- auc(roc_curve_knn)
legend("bottomright", legend = c(paste("KNN (AUC =", round(auc_value_knn, 2), ")")), col = c("blue"), lty = 1, lwd = 2)

