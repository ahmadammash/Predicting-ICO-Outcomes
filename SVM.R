
# Support Vector Machine ----------------------------------------------------------------------
library(kernlab)
library(ggplot2)
library(dplyr)
library(caret)
data_SVM <- data

sum(is.na(data_SVM))
str(data_SVM)


# Impute Missing Values with the missForest package ---------------------------------------------
library(missForest)
imputed_values <- missForest(data_SVM)
data_SVM <- imputed_values$ximp
dim(data_SVM)
sum(is.na(data_SVM))
set.seed(585)

#splitting the data to train and test
ratioSVM <- 0.85
smp_sizeSVM <- floor(ratioSVM*nrow(data_SVM)) 
set.seed(585)
train_ind_SVM <- sample(nrow(data_SVM),smp_sizeSVM)
data_SVM_train <- data_SVM[train_ind_SVM,]
data_SVM_test <- data_SVM[-train_ind_SVM,]

# Feature Selection ---------------------------------------------------------------------------
# library(caret)
# control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# results <- rfe(select(data_SVM_train,-success),
#                data_SVM_train$success, sizes = c(1:21), rfeControl = control)
# print(results)
# predictors(results)
# plot(results, type = c("g","o"),main = "RFE Accuracy Plot for Feature Combinations - SVM")

#8 features chosen
data_SVM_train <- data_SVM_train %>% select(c(success,rating,teamSize,startYear,endYear, duration,
                                              hasVideo, hasReddit, endMonth, startMonth, hasGithub,
                                              priceUSD, is_most_friendly, startDay))

data_SVM_test <- data_SVM_test %>% select(c(success,rating,teamSize,startYear,endYear, duration,
                                            hasVideo, hasReddit, endMonth, startMonth, hasGithub,
                                            priceUSD, is_most_friendly, startDay))

merged_data_SVM_train <- data_SVM_train
merged_data_SVM_test <- data_SVM_test



# choosing the cost parameter C - LINEAR SVM --------------------------------------------------
recall_list_CostFinder <- as.numeric()
precision_list_CostFinder <- as.numeric()
auc_list_CostFinder <- as.numeric()

cost_values <- c(1, seq(from = 5, to = 40, by = 5))
performance_metrics <- sapply(cost_values, function(x) {
  m <- ksvm(success ~., data = merged_data_SVM_train, kernel = "vanilladot", C = x, prob.model = TRUE)
  pred <- predict(m, select(merged_data_SVM_test,-success))
  pred_prob <- predict(m, select(merged_data_SVM_test,-success),type = "probabilities")
  results <- data.frame(
    actual_result = merged_data_SVM_test$success,
    predicted_result = pred,
    prob_success = round(pred_prob[,2],5),
    prob_failure = round(pred_prob[,1],5)
  )
  CM <- confusionMatrix(results$predicted_result, results$actual_result, positive = "Y")
  precision <- CM$byClass['Precision']
  recall <- CM$byClass['Recall']
  
  #create a prediction object
  pred_object_SVM_CostFinder <- ROCR::prediction(results$prob_success,results$actual_result)
  #create the AUC object
  auc_object_SVM_CostFinder <- ROCR::performance(pred_object_SVM_CostFinder, measure = "auc")
  #find the AUC value
  auc_SVM_CostFinder <- auc_object_SVM_CostFinder@y.values[[1]]
  
  recall_list_CostFinder <- append(recall_list_CostFinder,recall)
  precision_list_CostFinder <- append(precision_list_CostFinder,precision)
  auc_list_CostFinder <- append(auc_list_CostFinder,auc_SVM_CostFinder)
  metrics <- data.frame(
    Precision = precision_list_CostFinder,
    Recall = recall_list_CostFinder,
    AUC = auc_list_CostFinder
  )
  return(metrics)
})

performance_metrics <- as.data.frame(t(performance_metrics))
writexl::write_xlsx(x = performance_metrics,path = "test.xlsx")

par(mfrow=c(3,1))

#precision plot
plot(cost_values, performance_metrics$Precision,type = "b",xlab = "Cost Value",ylab = "Precision",
     col = "green",lwd=3,cex.lab = 1.6,cex.axis=1.4)
title("Plots of Precision Metrics for Different Linear SVM Cost Values",cex.main = 2)

#recall plot
plot(cost_values, performance_metrics$Recall,type = "b",xlab = "Cost Value",ylab = "Recall",
     col = "purple",lwd=3,cex.lab = 1.6,cex.axis=1.4)

#AUC plot
plot(cost_values, performance_metrics$AUC,type = "b",xlab = "Cost Value",ylab = "AUC",
     col = "orange",lwd=3,cex.lab = 1.6,cex.axis=1.4)


par(mfrow=c(1,1))

# Apply 10-fold Cross Validation on Linear SVM ------------------------------------------------

data_SVM_CV <- data

data_SVM_CV <- data_SVM_CV %>% select(c(success,rating,teamSize,startYear,endYear, duration,
                                        hasVideo, hasReddit, endMonth, startMonth, hasGithub,
                                        priceUSD, is_most_friendly, startDay))


# Impute Missing Values with the missForest package ---------------------------------------------
library(missForest)
imputed_values <- missForest(data_SVM_CV)
data_SVM_CV <- imputed_values$ximp
dim(data_SVM_CV)
sum(is.na(data_SVM_CV))

#create empty lists for storing measures
precision_list_SVM_CV <- as.numeric()
recall_list_SVM_CV <- as.numeric()
auc_list_SVM_CV <- as.numeric()
accuracy_list_SVM_CV <- as.numeric()
specificity_list_SVM_CV <- as.numeric()
F1_list_SVM_CV <- as.numeric()

#Automating the 10-fold CV for SVM
library(pROC)
library(tidyverse)
library(caret)
set.seed(585)
K = 10
#create the folds
set.seed(585)
folds_SVM <- createFolds(data_SVM_CV$success,k = K)
str(folds_SVM)
#we can see the 10 folds

#loop through the 10 folds
for(i in 1:K) {
  #split the data to training and testing
  data_SVM_CV_fold_test <- data_SVM_CV[folds_SVM[[i]],]
  data_SVM_CV_fold_train <- data_SVM_CV[-folds_SVM[[i]],]
  data_SVM_CV_fold_train_labels <- data_SVM_CV[-folds_SVM[[i]],"success"]
  data_SVM_CV_fold_test_labels <- data_SVM_CV[folds_SVM[[i]],"success"]
  
  #train a linear SVM model
  ICO_classifier_SVM_CV_linear <- kernlab::ksvm(success~., data = data_SVM_CV_fold_train, 
                                                kernel = "vanilladot",
                                             prob.model = TRUE, C = 15)
  
  
  ICO_predictions_SVM_CV_linear <- predict(ICO_classifier_SVM_CV_linear,select(data_SVM_CV_fold_test,-success))
  head(ICO_predictions_SVM_CV_linear)
  ICO_probabilities_SVM_CV_linear <- predict(ICO_classifier_SVM_CV_linear,
                                          select(data_SVM_CV_fold_test,-success), type = 'probabilities')
  head(ICO_probabilities_SVM_CV_linear)
  data_SVM_CV_fold_test_labels <- data_SVM_CV_fold_test$success
  
  #combine prediction results into a data frame
  SVM_CV_linear_results <- data.frame(
    actual_result = data_SVM_CV_fold_test$success,
    predicted_result = ICO_predictions_SVM_CV_linear,
    prob_success = round(ICO_probabilities_SVM_CV_linear[,2],5),
    prob_failure = round(ICO_probabilities_SVM_CV_linear[,1],5)
  )
  
  #Calculating the performance metrics using the caret package
  library(caret)
  SVM_CV_linear_CM <- confusionMatrix(SVM_CV_linear_results$predicted_result, SVM_CV_linear_results$actual_result, positive = "Y")
  print(SVM_CV_linear_CM)
  
  #precision
  precision_SVM_CV_linear <- SVM_CV_linear_CM$byClass['Precision']
  precision_SVM_CV_linear
  
  #recall
  recall_SVM_CV_linear <- SVM_CV_linear_CM$byClass['Recall']
  recall_SVM_CV_linear
  
  #accuracy
  accuracy_SVM_CV_linear <- SVM_CV_linear_CM$overall['Accuracy']
  
  #specificity
  specificity_SVM_CV_linear <- SVM_CV_linear_CM$byClass['Specificity']
  
  #F1
  F1_SVM_CV_linear <- SVM_CV_linear_CM$byClass['F1']
  
  #create the prediction object
  library(ROCR)
  pred_object_SVM_CV_linear <- ROCR::prediction(SVM_CV_linear_results$prob_success, 
                                          SVM_CV_linear_results$actual_result)
  
  #create the ROC object
  roc_object_SVM_CV_linear <- performance(pred_object_SVM_CV_linear, measure = "tpr", x.measure = "fpr")
  
  #plot the ROC curve
  plot(roc_object_SVM_CV_linear, lwd = 2, col = "blue",main = paste0("ROC Curve for Linear SVM CV (C = 15) - Fold ",i))
  abline(0,1,lty=2,lwd=2)
  
  #find the AUC value
  auc_object_SVM_CV_linear <- performance(pred_object_SVM_CV_linear, measure = "auc")
  auc_SVM_CV_linear <- auc_object_SVM_CV_linear@y.values[[1]]
  auc_SVM_CV_linear
  
  recall_list_SVM_CV <- append(recall_list_SVM_CV,recall_SVM_CV_linear)
  precision_list_SVM_CV <- append(precision_list_SVM_CV,precision_SVM_CV_linear)
  auc_list_SVM_CV <- append(auc_list_SVM_CV,auc_SVM_CV_linear)
  accuracy_list_SVM_CV <- append(accuracy_list_SVM_CV,accuracy_SVM_CV_linear)
  specificity_list_SVM_CV <- append(specificity_list_SVM_CV,specificity_SVM_CV_linear)
  F1_list_SVM_CV <- append(F1_list_SVM_CV,F1_SVM_CV_linear)
  
  if(i==10) {bestROC_SVM <- roc_object_SVM_CV_linear}
  
}

SVM_CV_linear_performance_metrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Average","SD"),  
  Precision = c(precision_list_SVM_CV,mean(precision_list_SVM_CV),sd(precision_list_SVM_CV)),
  Recall = c(recall_list_SVM_CV,mean(recall_list_SVM_CV),sd(recall_list_SVM_CV)),
  AUC = c(auc_list_SVM_CV,mean(auc_list_SVM_CV),sd(auc_list_SVM_CV)),
  Accuracy = c(accuracy_list_SVM_CV,mean(accuracy_list_SVM_CV),sd(accuracy_list_SVM_CV)),
  Specificity = c(specificity_list_SVM_CV,mean(specificity_list_SVM_CV),sd(specificity_list_SVM_CV)),
  F1 = c(F1_list_SVM_CV,mean(F1_list_SVM_CV),sd(F1_list_SVM_CV))
)

writexl::write_xlsx(x = SVM_CV_linear_performance_metrics, path = "SVM Linear CV performance metrics.xlsx")

