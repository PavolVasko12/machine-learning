#=====================================================
#STEP 1: LOAD ALL NECESSARY LIBRARIES FOR OUR PROJECT
#=====================================================
#List of the libraries
packages <- c("SASxport", "dplyr", "caret", "DAAG", "glmnet", "mlbench", "psych");
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
  assign("data", read.csv("project/Ready_data.csv"), envir = .GlobalEnv)
  #Show number of observations and features of first 10 examples and its type
  str(data)
  #Shows the median, mean and Min, Max values
  summary(data)
}
read_data()



#=================
#STEP 3: MODELING 
#=================
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


#===========================
#STEP 4: IMPROVING MODELING 
#===========================
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
  #data_shuffled <- data_factors[sample(nrow(data_factors)),]
  data_factors <- data_factors[sample(nrow(data_factors)),]
  end_row <- nrow(data_factors);
  data_test <- data_factors[1:test_number-1,]
  data_train <- data_factors[test_number:end_row , ]
  
  model <- lm(X.BMI5 ~., data = data_train)
  prediction <- predict(model,data_test)
  
  SSE <- sum((data_test$X.BMI5 - prediction) ^ 2)
  SST <- sum((data_test$X.BMI5 - mean(data_test$X.BMI5)) ^ 2)
  model_preformance <- 1 - SSE/SST
  sprintf("Model Accuracy: %s", model_preformance)
}
factors_data_modeling()

#Converting numerical values to binary
binary_data_modeling <- function() {
  data_binary <- data_shuffled
}





#=======================================
#STEP 5: MODELING WITH CROSS VALIDATION
#=======================================
#Cross-Validation with 10 folds, outputs the result of predicted value so you can compare to the real value.
cross_val <- function(num_predictions, export){
  model <- lm(X.BMI5 ~., data = data_factors)
  cv_model <- cv.lm(data_factors, model, m=10)
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



#====================================
#STEP 6: FEATURE SELECTION TECHNIQUE
#====================================

plot_fit <- function(fit, ...) {
  plot(fit, xvar="lambda")
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}

#Feautre selection technique for identifying important variables
#The more coefficienet the feature has the more impact it has on our depended variable (BMI)
feature_selection_technique_lasso <- function(data_set) {
  data_set <- data_shuffled
  bmi <- select(data_set, "X.BMI5")
  drop_column <- c("X.BMI5") 
  data_set_without_BMI <- data_set[, !(names(data_set) %in%  drop_column)]
  fit <- glmnet(as.matrix(data_set_without_BMI), as.matrix(bmi), standardize = TRUE, alpha = 1)
  plot_fit(fit)
}
feature_selection_technique_lasso(data_shuffled)


feature_selection_technique_boruta <- function(data_set, data_set_size){
   data_sample <- sample_n(data_set, data_set_size)
   Boruta_fst <- Boruta(X.BMI5 ~ ., data = data_sample, doTrace = 2)
   assign("Boruta_fst", Boruta_fst, envir = .GlobalEnv)
   return(Boruta_fst)
}

feature_selection_technique_boruta(data_factors, 100)


#Only selected feature with kFold = 10 to clearly see the imprtance of the features
only_selected_features <- function(data_set) {
  bmi <- select(data_set, "X.BMI5")
  selected_features <- select(data_set, "GENHLTH", "EXERANY2", "INTERNET", "SEX", "X.FRUTSU1", "X.EDUCAG", "GENERAL.MERCHANDISER", "X.SMOKER3")
  fit <- glmnet(as.matrix(selected_features), as.matrix(bmi), standardize = TRUE, alpha = 1)
  #fit <- cv.glmnet(as.matrix(selected_features), as.matrix(bmi),standardize = TRUE,type.measure = "mse", nfolds = 10, alpha = 1)
  plot_fit(fit)
}
only_selected_features(data_shuffled)






#==================================================================
#STEP 7: My OWN CROSS VALIDATION WITH FEATURE SELECTION TECHNIQUE
#==================================================================
model_predict <- function(data_train, data_test){
  print("INSIDE MODELING")
  model <- lm(X.BMI5 ~., data = data_train)
  print("BEFORE PREDICTION")
  #print("!!!!! MODEL : \n")
  #print(summary(model))
  #print("!!!!! DATA TRAIN : \n")
  #print(str(data_train))
  #print("!!!!! DATA TEST : \n")
  #print(str(data_test))
  prediction <- predict(model, data_test)
  print("AFTER PREDICTION")
  single_performance <- evaluate_performance(prediction, data_test)
  return(single_performance)
}

evaluate_performance <- function(prediction, data_test){
  print("INSIDE EVALUATION")
  SSE <- sum((data_test$X.BMI5 - prediction) ^ 2)
  SST <- sum((data_test$X.BMI5 - mean(data_test$X.BMI5)) ^ 2)
  model_preformance <- 1 - SSE/SST
  return(model_preformance)
}



CV_with_FST <- function(data_set, kFold, ...){
  list_argument <- list(...)
  if(!is.null(list_argument$sample_size)) {
    data_set <- sample_n(data_set, list_argument$sample_size)
    print("INSIDE IF ADDITIONAL PARAMETER")
  }
  print(nrow(data_set))
  data_set_row_length <- nrow(data_set)
  kFold_size <- floor(data_set_row_length / kFold)
  newCounter <- kFold_size
  oldCounter <- 0
  average_performance <- 0;
  test <- 0
  
  for (i in 1:kFold) {
    cat(sprintf('i:  %i kfold out of: %i \n',i, kFold)) 
    data_test <- data_set[c(oldCounter:newCounter),]
    data_train <- data_set[-c(oldCounter:newCounter),]
    
    data_set_size <- nrow(data_train)
    
    boruta_result <- feature_selection_technique_boruta(data_train, data_set_size)
    boruta_signifificant <- names(boruta_result$finalDecision[boruta_result$finalDecision %in% c("Confirmed", "Tentative")])
    
    data_test_after_selection <- data_test[, c("X.BMI5", boruta_signifificant)]
    data_train_after_selection <- data_train[, c("X.BMI5", boruta_signifificant)]
    
    model_prediction <- model_predict(data_train_after_selection, data_test_after_selection)
    average_performance <- average_performance + model_prediction
    print(average_performance)
    cat(sprintf('Single model prediction:  %f \n', model_prediction)) 
    oldCounter <- newCounter + 1
    newCounter <- newCounter + kFold_size
  }
  regression_accuracy <- average_performance / kFold
  sprintf("Single Average Accuracy after %s kFold is: %s", i, regression_accuracy)
}

CV_with_FST(data_factors, 3, sample_size=400)






















