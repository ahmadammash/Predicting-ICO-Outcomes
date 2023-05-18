
# AdaBoost - Iterate to find the best number of trials ------------------------------------------
library(C50)
#improving the model performance
#MISSING VALUES IMPUTED WITHIN C5.0 ALGORITHM
trial_values <- c(seq(from = 1, to = 15, by = 1))
recall_list <- as.numeric()
precision_list <- as.numeric()
auc_list <- as.numeric()
for (trial in trial_values) {
  ICO_classifier_DT_boost <- C5.0(x = select(DT_train,-c(success)),y = DT_train$success, trials = trial,
                                  na.action = na.impute)
  ICO_test_pred_DT_boost <- predict(ICO_classifier_DT_boost, select(DT_test,-success))
  ICO_test_prob_DT_boost <- predict(ICO_classifier_DT_boost, select(DT_test,-success),type = "prob")
  
  ICO_test_DT_boost_results <- data.frame(
    actual_result = DT_test$success,
    predicted_result = ICO_test_pred_DT_boost,
    prob_failure = round(ICO_test_prob_DT_boost[,1],5),
    prob_success = round(ICO_test_prob_DT_boost[,2],5)
  )
  
  
  ICO_DT_boost_CM <- confusionMatrix(ICO_test_DT_boost_results$predicted_result
                                     , ICO_test_DT_boost_results$actual_result, positive = "Y")
  
  recall_list <- append(recall_list, ICO_DT_boost_CM$byClass['Recall'])
  precision_list <- append(precision_list, ICO_DT_boost_CM$byClass['Precision'])
  
  
  ICO_test_DT_pred_object <-  ROCR::prediction(ICO_test_DT_boost_results$prob_success, 
                                         ICO_test_DT_boost_results$actual_result)
  
  auc_test_DT_object <-  performance(ICO_test_DT_pred_object, measure = "auc")
  auc_test_DT <- auc_test_DT_object@y.values[[1]]
  auc_list <- append(auc_list,auc_test_DT)
  
}
trial_iterations_performance_metrics <- data.frame(
  Trial = seq(1,15,1),
  Precision = precision_list,
  Recall = recall_list,
  AUC = auc_list
)
writexl::write_xlsx(x = trial_iterations_performance_metrics, path = "DT trials iterations.xlsx")  

library(gridExtra)  
precision_plot <- ggplot(aes(x = trial_values,y = Precision),data = trial_iterations_performance_metrics) + 
  geom_point() + xlab('Trial Values') + ylab('Precision') + scale_x_continuous(breaks = seq(1,15,1)) + 
  theme(text = element_text(size = 15))
recall_plot <- ggplot(aes(x = trial_values,y = Recall),data = trial_iterations_performance_metrics) + 
  geom_point() + xlab('Trial Values') + ylab('Recall') + scale_x_continuous(breaks = seq(1,15,1)) + 
  theme(text = element_text(size = 15))
AUC_plot <- ggplot(aes(x = trial_values,y = AUC),data = trial_iterations_performance_metrics) + 
  geom_point() + xlab('Trial Values') + ylab('AUC') + scale_x_continuous(breaks = seq(1,15,1)) + 
  theme(text = element_text(size = 15))

grid.arrange(precision_plot, recall_plot, AUC_plot,
             top = textGrob("Plot of AdaBoost C5.0 Performance Metrics for Different Numbers of Trials",
                            gp = gpar(fontsize = 20)))
             


# Applying Cross Validation to C5.0 Boost 13 -------------------------------------------------------

data_boost13_CV <- data_DT

#Creating empty lists to store performance metrics
precision_list_boost13 <- as.numeric()
recall_list_boost13 <- as.numeric()
AUC_list_boost13 <- as.numeric()
accuracy_list_boost13 <- as.numeric()
F1_list_boost13 <- as.numeric()
specificity_list_boost13 <- as.numeric()

K = 10

#create the folds
set.seed(585)
folds_boost13 <- createFolds(data_boost13_CV$success,k = K)
str(folds_boost13)
#we can see the 13 folds

#now loop through the folds
for (i in 1:K) {
  
  #sampling for training and testing
  ICO_boost13_test_fold <- data_boost13_CV[folds_boost13[[i]],]
  ICO_boost13_train_fold <- data_boost13_CV[-folds_boost13[[i]],]
  
  #train the model with the C5.0
  ICO_boost13 <- C5.0(select(ICO_boost13_train_fold,-success), ICO_boost13_train_fold$success,trials = 13,
                      na.action = na.impute)
  
  #predict on the test data
  ICO_boost13_predict <- predict(ICO_boost13,ICO_boost13_test_fold)
  ICO_boost13_predict_prob <- predict(ICO_boost13,select(ICO_boost13_test_fold,-success), type = "prob")
  
  #create a dataframe
  ICO_boost13_CV_results <- data.frame(
    actual_result = ICO_boost13_test_fold$success,
    predicted_result = ICO_boost13_predict,
    prob_failure = round(ICO_boost13_predict_prob[,1],5),
    prob_success = round(ICO_boost13_predict_prob[,2],5)
  )
  
  #generate the confusion matrix
  ICO_boost13_CM <- caret::confusionMatrix(data = ICO_boost13_predict, reference = ICO_boost13_test_fold$success,
                                           positive = "Y")
  print(ICO_boost13_CM)

  #view the precision measure
  ICO_boost13_pred_precision <- ICO_boost13_CM$byClass['Precision']
  
  #view the recall measure
  ICO_boost13_pred_recall <- ICO_boost13_CM$byClass['Recall']
  
  #view the accuracy measure
  ICO_boost13_pred_accuracy <- ICO_boost13_CM$overall['Accuracy']
  
  #view the specificity measure
  ICO_boost13_pred_specificity <- ICO_boost13_CM$byClass['Specificity']
  
  #view the F1 measure
  ICO_boost13_pred_F1 <- ICO_boost13_CM$byClass['F1']
  

  #append the performance metrics
  precision_list_boost13 <- append(precision_list_boost13,ICO_boost13_pred_precision)
  recall_list_boost13 <- append(recall_list_boost13,ICO_boost13_pred_recall)
  accuracy_list_boost13 <- append(accuracy_list_boost13,ICO_boost13_pred_accuracy)
  specificity_list_boost13 <- append(specificity_list_boost13,ICO_boost13_pred_specificity)
  F1_list_boost13 <- append(F1_list_boost13,ICO_boost13_pred_F1)
  
  #create an ROC curve
  #create a prediction object
  pred_object_boost13_CV <- ROCR::prediction(ICO_boost13_CV_results$prob_success,ICO_boost13_CV_results$actual_result)
  
  #calculate the ROC curve
  roc_boost13_fold <- performance(pred_object_boost13_CV,measure = "tpr",x.measure = "fpr")
  
  #plot the ROC curve
  plot(roc_boost13_fold,lwd=2,col="blue",main = paste("ROC of Boosted CV (13 Trials) - Fold",i))
  abline(0,1,lwd=2,lty=2)
  
  #calculate the AUC value
  auc_boost13_fold_object <- performance(pred_object_boost13_CV,measure = "auc")
  auc_boost13_fold <- auc_boost13_fold_object@y.values[[1]]
  
  #append the AUC values
  AUC_list_boost13 <- append(AUC_list_boost13,auc_boost13_fold)
  
  if(i==3) {best_ROC_AdaBoost <- roc_boost13_fold}
  
  
}

DT_boost13_CV_performance_metrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Average","SD"),
  Precision = c(precision_list_boost13,mean(precision_list_boost13),sd(precision_list_boost13)),
  Recall = c(recall_list_boost13,mean(recall_list_boost13),sd(recall_list_boost13)),
  AUC = c(AUC_list_boost13,mean(AUC_list_boost13),sd(AUC_list_boost13))
)

DT_boost13_CV_otherPerformanceMetrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Average","SD"),
  Accuracy = c(accuracy_list_boost13,mean(accuracy_list_boost13),sd(accuracy_list_boost13)),
  Specificity = c(specificity_list_boost13,mean(specificity_list_boost13),sd(specificity_list_boost13)),
  F1 = c(F1_list_boost13,mean(F1_list_boost13),sd(F1_list_boost13))
)

writexl::write_xlsx(x = DT_boost13_CV_performance_metrics, path = "DT boost 13 CV performance metrics.xlsx")



