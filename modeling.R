#=====================================================
#STEP 1: LOAD ALL NECESSARY LIBRARIES FOR OUR PROJECT
#=====================================================
#List of the libraries
packages <- c("SASxport", "dplyr", "caret", "DAAG", "glmnet");
#Function for checking if library is installed if not then install it and load it ot R so we can use it.
install_libraries <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}
install_libraries(packages)


#================================================================================
#STEP 2: LOAD THE DATE SET TO R AND DISPLAY BASIC INFORMATION ABOUT THE DATA SET
#================================================================================
#Read data into R
read_data <- function(){
  #Read the data and assign them to global scope
  assign("data", read.csv("Ready_data.csv"), envir = .GlobalEnv)
  #Show number of observations and features of first 10 examples and its type
  str(data)
  #Shows the median, mean and Min, Max values
  summary(data)
}
read_data()



#=============================
#STEP 3: MODELING AND TESTING
#=============================
#Evaluating row model
row_data_modeling <- function(){
  test_number <- round(0.25 * nrow(data))
  assign("data_shuffled", data[sample(nrow(data)),], envir = .GlobalEnv)
  end_row <- nrow(data_shuffled);
  data_test <- data_shuffled[1:test_number-1,]
  data_train <- data_shuffled[test_number:end_row , ]
  
  model <- lm(X.BMI5 ~., data = data_train)
  prediction <- predict(model,data_test)
  
  SSE <- sum((data_test$X.BMI5 - prediction) ^ 2)
  SST <- sum((data_test$X.BMI5 - mean(data_test$X.BMI5)) ^ 2)
  model_preformance <- 1 - SSE/SST
  sprintf("Model Accuracy: %s", model_preformance)
}
row_data_modeling()


#Evaluating model where BMI splited into 4 categories
four_categories_bmi_data_modeling <- function(){
  data_bmi <- data
  data_bmi$X.BMI5 <- ifelse(data$X.BMI5 > 30, 4, data_bmi$X.BMI5)
  data_bmi$X.BMI5 <- ifelse(data$X.BMI5 < 31, 3, data_bmi$X.BMI5)
  data_bmi$X.BMI5 <- ifelse(data$X.BMI5 < 25, 2, data_bmi$X.BMI5)
  data_bmi$X.BMI5 <- ifelse(data$X.BMI5 < 20, 1, data_bmi$X.BMI5)
  
  test_number <- round(0.25 * nrow(data_bmi))
  data_shuffled <- data_bmi[sample(nrow(data_bmi)),]
  end_row <- nrow(data_shuffled);
  data_test <- data_shuffled[1:test_number-1,]
  data_train <- data_shuffled[test_number:end_row , ]
  
  model <- lm(X.BMI5 ~., data = data_train)
  prediction <- predict(model,data_test)
  
  SSE <- sum((data_test$X.BMI5 - prediction) ^ 2)
  SST <- sum((data_test$X.BMI5 - mean(data_test$X.BMI5)) ^ 2)
  model_preformance <- 1 - SSE/SST
  sprintf("Model Accuracy: %s", model_preformance)
}
four_categories_bmi_data_modeling()


#Evaluating model where AGE splited into 6 categories
four_categories_bmi_data_modeling <- function(){
  data_age <- data
  data_age$X.AGE80 <- ifelse(data$X.AGE80 >= 65, 6, data_age$X.AGE80)
  data_age$X.AGE80 <- ifelse(data$X.AGE80 < 65, 5, data_age$X.AGE80)
  data_age$X.AGE80 <- ifelse(data$X.AGE80 < 55, 4, data_age$X.AGE80)
  data_age$X.AGE80 <- ifelse(data$X.AGE80 < 45, 3, data_age$X.AGE80)
  data_age$X.AGE80 <- ifelse(data$X.AGE80 < 35, 2, data_age$X.AGE80)
  data_age$X.AGE80 <- ifelse(data$X.AGE80 < 25, 1, data_age$X.AGE80)
  
  test_number <- round(0.25 * nrow(data_age))
  data_shuffled <- data_age[sample(nrow(data_age)),]
  end_row <- nrow(data_shuffled);
  data_test <- data_shuffled[1:test_number-1,]
  data_train <- data_shuffled[test_number:end_row , ]
  
  model <- lm(X.BMI5 ~., data = data_train)
  prediction <- predict(model,data_test)
  
  SSE <- sum((data_test$X.BMI5 - prediction) ^ 2)
  SST <- sum((data_test$X.BMI5 - mean(data_test$X.BMI5)) ^ 2)
  model_preformance <- 1 - SSE/SST
  sprintf("Model Accuracy: %s", model_preformance)
}
four_categories_bmi_data_modeling()


#Converting some features to factores
factors_data_modeling <- function(){
  data_factors <- data
  data_factors$X.RACE <- as.factor(data_factors$X.RACE)
  data_factors$SEX <- as.factor(data_factors$SEX)
  data_factors$X.CHLDCNT <- as.factor(data_factors$X.CHLDCNT)
  data_factors$MARITAL <- as.factor(data_factors$MARITAL)
  data_factors$EMPLOY1 <- as.factor(data_factors$EMPLOY1)
  data_factors$EXERANY2 <- as.factor(data_factors$EXERANY2)
  data_factors$INTERNET <- as.factor(data_factors$INTERNET)
  data_factors$RENTHOM1 <- as.factor(data_factors$RENTHOM1)
  data_factors$X.SMOKER3 <- as.factor(data_factors$X.SMOKER3)
  data_factors$DRNKANY5 <- as.factor(data_factors$DRNKANY5)
  assign("data_factors", data_factors, envir = .GlobalEnv)
  
  test_number <- round(0.25 * nrow(data_factors))
  data_shuffled <- data_factors[sample(nrow(data_factors)),]
  end_row <- nrow(data_shuffled);
  data_test <- data_shuffled[1:test_number-1,]
  data_train <- data_shuffled[test_number:end_row , ]
  
  model <- lm(X.BMI5 ~., data = data_train)
  prediction <- predict(model,data_test)
  
  SSE <- sum((data_test$X.BMI5 - prediction) ^ 2)
  SST <- sum((data_test$X.BMI5 - mean(data_test$X.BMI5)) ^ 2)
  model_preformance <- 1 - SSE/SST
  sprintf("Model Accuracy: %s", model_preformance)
}
factors_data_modeling()




#=======================================
#STEP 4: MODELING WITH CROSS VALIDATION
#=======================================
#Cross-Validation with 10 folds, outputs the result of predicted value so you can compare to the real value.
cross_val <- function(num_predictions, export){
  model <- lm(X.BMI5 ~., data = data_shuffled)
  cv_model <- cv.lm(data_shuffled, model, m=10)
  assign("cv_model", cv_model, envir = .GlobalEnv)
  predicted_features <- data.frame(cv_model$X.BMI5, cv_model$Predicted, cv_model$cvpred)
  if(export == TRUE){
    write.csv(predicted_features, file = "Predicted_features.csv")
  }
  else{
    head(predicted_features, num_predictions)
  }
}
#cross_val (par1, par2); par1 - number of exmaples to be displayed in console in R, par2 - set true or false to export all predicted value as csv to your root R folder on your machine. 
cross_val(100, FALSE)



#===================================================
#STEP 5: CROSS VAL WITH FEATURE SELECTION TECHNIQUE
#===================================================

add_labels <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
  plot(fit, xvar="lambda")
  add_labels(fit)
}

#Feautre selection technique for identifying important variables
feature_selection_technique <- function() {
  
}




















