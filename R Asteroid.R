rm(list = ls())

library(ggplot2)
library(class)
library(e1071)
asteroid_data = dataset

# Plots
asteroid_data$class <- factor(asteroid_data$class, labels = c("IEO", "ATE", "APO", "AST", "AMO", "MCA", "IMB", "MBA", "OMB", "TJN", "CEN", "TNO", "HYA"))
ggplot(asteroid_data, aes(x = a, y = e, color = class)) +
  geom_point(shape = ".", size = 3) +
  xlim(c(0, 6)) +
  labs(x = "Semi-major Axis in AU (a)", y = "Eccentricity (e)") +
  ggtitle("Eccentricity vs Semi-major Axis for Asteroids")


# Cleaning the dataset
asteroid_data = dataset
clean_data = na.omit(asteroid_data)
class_mapping <- c("ATE" = 1, "APO" = 2, "AMO" = 3, "MCA" = 4, "IMB" = 5, 
                   "MBA" = 6, "OMB" = 7, "TJN" = 8, "CEN" = 9, "TNO" = 10)
clean_data$class <- as.integer(factor(clean_data$class, levels = names(class_mapping)))
clean_data = na.omit(clean_data)
clean_data$neo <- ifelse(clean_data$neo == "N", 0, 1)
clean_data$pha <- ifelse(clean_data$pha == "N", 0, 1)




# KNN Model
# Assigning test and training
variables <- clean_data[, c("neo", "pha", "H", "diameter", "albedo", "e", "a", "q", "i", "per_y", "moid", "class")]

set.seed(123)
train_indices <- sample(1:nrow(variables), 0.8 * nrow(variables))
train_data <- variables[train_indices, ]
test_data <- variables[-train_indices, ]

X_train <- train_data[, -which(names(train_data) == "pha")]
Y_train <- train_data$pha
X_test <- test_data[, -which(names(test_data) == "pha")]
Y_test <- test_data$pha

# Normalizing the predictors
scaled_X_train <- scale(X_train)
scaled_X_test <- scale(X_test)

# This will take an hour to compute. Please do not compute.
# K = 5 and K = 35 are the best performing
# max accuracy values at 0.9988562 
# minimum error rate values at 0.001143816 
k_values <- seq(1, 100, by = 1)
accuracy_values <- numeric(length(k_values))
ErrorRate_values <- numeric(length(k_values))
for (i in 1:length(k_values)) {
  knn_model <- knn(train = scaled_X_train, test = scaled_X_test, cl = Y_train, k = k_values[i])
  accuracy_values[i] <- sum(knn_model == Y_test) / length(Y_test)
  ErrorRate_values[i] <- mean(knn_model != Y_test)
}

#Plot is included in the report
plot(k_values, accuracy_values, type = "b", 
     xlab = "Number of Neighbors (k)", ylab = "Accuracy", 
     main = "KNN Accuracy vs. Number of Neighbors")
plot(k_values, ErrorRate_values, type = "b", 
     xlab = "Number of Neighbors (k)", ylab = "Error Rate", 
     main = "KNN Error Rate vs. Number of Neighbors")


# Run this, Using the Best K Run and Evaluating the model
for (K in c(5, 35)){
  set.seed(123)
  knn_model <- knn(train = scaled_X_train, test = scaled_X_test, cl = Y_train, k = K)
  accuracy <- sum(knn_model == Y_test) / length(Y_test)
  print(paste("Accuracy:", accuracy))
  print(paste("Error Rate:", mean(knn_model != Y_test)))
  print(paste("K =", K))
}


# Fit the KNN model on the full dataset (clean_data)
predictors <- variables[, -which(names(variables) == "pha")]
Y <- clean_data$pha
scaled_variables <- scale(predictors)
knn_model_full <- knn(train = scaled_variables, test = scaled_variables, cl = Y, k = 35)
print(summary(knn_model_full))
accuracy <- sum(knn_model_full == clean_data$pha) / length(clean_data$pha)
print(paste("Accuracy:", accuracy))
print(paste("Error Rate:", mean(knn_model_full != clean_data$pha)))

# Results include predicted, pha, along with full_name and variables used for model
results <- cbind(clean_data, predicted = knn_model_full)
results <- results[results$pha == 1, c("predicted", "pha", "neo", "class", "full_name", "H", "diameter", "albedo", "e", "a", "q", "i", "per_y", "moid")]
print(results)





# SVM Model
# Factoring PHA 0 for N and 1 for Y
clean_data$pha <- as.factor(clean_data$pha)
variables <- clean_data[, c("neo", "pha", "H", "diameter", "albedo", "e", "a", "q", "i", "per_y", "moid", "class")]

set.seed(123)
train_indices <- sample(1:nrow(variables), 0.8 * nrow(variables))
train_data <- variables[train_indices, ]
test_data <- variables[-train_indices, ]


# SVM LINEAR
tune.out.linear=tune(svm,
              pha~.,
              data=train_data,
              kernel="linear",
              scale=TRUE,
              ranges = list(cost = seq(1, 100)))
summary(tune.out.linear)

svmfit.linear=svm(pha~., data=train_data, kernel="linear", cost = 65, scale=TRUE)
summary(svmfit.linear)

train_pred.linear <- predict(svmfit.linear, train_data)
test_pred.linear <- predict(svmfit.linear, test_data)
table(pred=train_pred.linear, true=train_data$pha)
print(paste("Accuracy Train:", sum(train_pred.linear == train_data$pha) / length(train_data$pha)))
print(paste("Accuracy Test:", sum(test_pred.linear == test_data$pha) / length(test_data$pha)))
print(paste("Error Rate Train:", mean(train_pred.linear != train_data$pha)))
print(paste("Error Rate Test:", mean(test_pred.linear != test_data$pha)))


# SVM POLY
tune.out.poly=tune(svm,
              pha~.,
              data=train_data,
              kernel="polynomial",
              scale=TRUE,
              ranges=list(cost=c(0.01, 0.1, 1, 5, 10, 100), degree=c(2,3,4)))
summary(tune.out.poly)

svmfit.poly=svm(pha~., data=train_data, kernel="polynomial", cost = 100, degree = 2, scale=TRUE)
summary(svmfit.poly)

train_pred.poly <- predict(svmfit.poly, train_data)
test_pred.poly <- predict(svmfit.poly, test_data)
table(pred=train_pred.poly, true=train_data$pha)
print(paste("Accuracy Train:", sum(train_pred.poly == train_data$pha) / length(train_data$pha)))
print(paste("Accuracy Test:", sum(test_pred.poly == test_data$pha) / length(test_data$pha)))
print(paste("Error Rate Train:", mean(train_pred.poly != train_data$pha)))
print(paste("Error Rate Test:", mean(test_pred.poly != test_data$pha)))


# SVM RADIAL
tune.out.radial=tune(svm,
                   pha~.,
                   data=train_data,
                   kernel="radial",
                   scale=TRUE,
                   ranges=list(cost=c(1, 5),
                   gamma=c(0.1, 0.25, 0.5, 0.75)))
summary(tune.out.radial)

svmfit.radial=svm(pha~., data=train_data, kernel="radial", cost = 5, gamma = 0.5, scale=TRUE)
summary(svmfit.radial)

train_pred.radial <- predict(svmfit.radial, train_data)
test_pred.radial <- predict(svmfit.radial, test_data)
table(pred=train_pred.radial, true=train_data$pha)
print(paste("Accuracy Train:", sum(train_pred.radial == train_data$pha) / length(train_data$pha)))
print(paste("Accuracy Test:", sum(test_pred.radial == test_data$pha) / length(test_data$pha)))
print(paste("Error Rate Train:", mean(train_pred.radial != train_data$pha)))
print(paste("Error Rate Test:", mean(test_pred.radial != test_data$pha)))


# SVM vs FULL CLEAN DATASET
svmfit.linear.full = svm(pha~., data=variables, kernel="linear", cost = 65, scale=TRUE)
svmfit.poly.full = svm(pha~., data=variables, kernel="polynomial", cost = 100, degree = 2, scale=TRUE)
svmfit.radial.full = svm(pha~., data=variables, kernel="radial", cost = 5, gamma = 0.5, scale=TRUE)

model_list = list(svmfit.linear.full, svmfit.poly.full, svmfit.radial.full)
for (model in model_list){
  model_name <- deparse(substitute(model))
  model_pred = predict(model, clean_data)
  print(paste("Model: ", model_name))
  print(paste("Accuracy:", sum(model_pred == clean_data$pha) / length(clean_data$pha)))
  print(paste("Error Rate:", mean(model_pred != clean_data$pha)))
  table(pred=model_pred, true=clean_data$pha)
}

