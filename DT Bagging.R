# Applying Bagging to Decision Tree ----------------------------------------------------------


# Iterate to find the optimal number of bags -------------------------------------------------
#MISSING VALUES ARE NOT A PROBLEM AND ARE LEFT AS IS

library(dplyr)
library(ipred)
library(ROCR)
library(caret)
set.seed(585)
recall_bag_list <- as.numeric()
precision_bag_list <- as.numeric()
auc_bag_list <- as.numeric()
nbagg_list <- as.numeric()
for (i in seq(1,150,5)) {
  ICO_bag <- bagging(success ~., data = DT_train, nbagg = i)
  ICO_bag_pred <- predict(ICO_bag,DT_test)
  ICO_bag_prob <- predict(ICO_bag,select(DT_test,-success), type = c("prob"))
  ICO_bag_results <- data.frame(
    actual_result = DT_test$success,
    predicted_result = ICO_bag_pred,
    prob_success = round(ICO_bag_prob[,2],5),
    prob_failure = round(ICO_bag_prob[,1],5)
  )
  ICO_bag_CM <- confusionMatrix(ICO_bag_pred, DT_test$success, positive = "Y")
  recall_bag <- ICO_bag_CM$byClass['Recall']
  precision_bag <- ICO_bag_CM$byClass['Precision']
  
  ICO_bag_predobject <- ROCR::prediction(ICO_bag_results$prob_success, ICO_bag_results$actual_result)
  auc_bag_object <- performance(ICO_bag_predobject, measure = "auc")
  auc_bag <- auc_bag_object@y.values[[1]]
  
  recall_bag_list <- append(recall_bag_list, recall_bag)
  auc_bag_list <- append(auc_bag_list, auc_bag)
  precision_bag_list <- append(precision_bag_list,precision_bag)
  nbagg_list <- append(nbagg_list,i)
}
library(ggplot2)
bags_performance_metrics <- data.frame(
  nb_bags = nbagg_list,
  precision = precision_bag_list,
  recall = recall_bag_list,
  AUC = auc_bag_list
)

precision_plot <- ggplot(data = bags_performance_metrics,aes(x=nb_bags,y=precision)) + geom_point() + 
  scale_x_continuous(breaks = seq(0,150,5)) + xlab('Number of Bags') + ylab('Precision') +
  theme(text = element_text(size = 15))
recall_plot <- ggplot(data = bags_performance_metrics,aes(x=nb_bags,y=recall)) + geom_point() + 
  scale_x_continuous(breaks = seq(0,150,5)) + xlab('Number of Bags') + ylab('Recall') + 
  theme(text = element_text(size = 15))
AUC_plot <- ggplot(data = bags_performance_metrics,aes(x=nb_bags,y=AUC)) + geom_point() + 
  scale_x_continuous(breaks = seq(0,150,5)) + xlab('Number of Bags') + ylab('AUC') + 
  theme(text = element_text(size = 15))

library(gridExtra)
grid.arrange(precision_plot, recall_plot, AUC_plot,
             top = textGrob("Plot of Bagged DT Performance Metrics for Different Numbers of Bags",
                            gp = gpar(fontsize = 20)))
# 46 bags optimal

# Applying Cross Validation to Bagging with 46 trees -----------------------------------------------
#MISSING VALUES ARE NOT A PROBLEM HERE
data_bagging_CV <- data_DT

#Creating empty lists to store performance metrics
recall_list_bagging <- as.numeric()
precision_list_bagging <- as.numeric()
AUC_list_bagging <- as.numeric()
accuracy_list_bagging <- as.numeric()
specificity_list_bagging <- as.numeric()
F1_list_bagging <- as.numeric()

K = 10

#create the folds
set.seed(585)
folds_bagging <- createFolds(data_bagging_CV$success,k = K)
str(folds_bagging)
#we can see the 46 folds


#now loop through the folds
for (i in 1:K) {
  
  #sampling for training and testing
  ICO_bagging_test_fold <- data_bagging_CV[folds_bagging[[i]],]
  ICO_bagging_train_fold <- data_bagging_CV[-folds_bagging[[i]],]
  
  #train the bagging model
  ICO_bagging <- bagging(success~., data = ICO_bagging_train_fold, nbagg = 46)
  #predict on the test data
  ICO_bagging_predict <- predict(ICO_bagging,ICO_bagging_test_fold)
  ICO_bagging_predict_prob <- predict(ICO_bagging,select(ICO_bagging_test_fold,-success), type = "prob")
  
  #create a dataframe
  ICO_bagging_CV_results <- data.frame(
    actual_result = ICO_bagging_test_fold$success,
    predicted_result = ICO_bagging_predict,
    prob_failure = round(ICO_bagging_predict_prob[,1],5),
    prob_success = round(ICO_bagging_predict_prob[,2],5)
  )
  
  #generate the confusion matrix
  ICO_bagging_CM <- caret::confusionMatrix(data = ICO_bagging_predict, reference = ICO_bagging_test_fold$success,
                                           positive = "Y")
  print(ICO_bagging_CM)
  #view the recall measure
  ICO_bagging_pred_recall <- ICO_bagging_CM$byClass['Recall']

  #view the precision measure
  ICO_bagging_pred_precision <- ICO_bagging_CM$byClass['Precision']
  
  #view the accuracy measure
  ICO_bagging_pred_accuracy <- ICO_bagging_CM$overall['Accuracy']
  
  #view the specificity measure
  ICO_bagging_pred_specificity <- ICO_bagging_CM$byClass['Specificity']
  
  #view the F1 measure
  ICO_bagging_pred_F1 <- ICO_bagging_CM$byClass['F1']
  
  
  #append the performance metrics
  recall_list_bagging <- append(recall_list_bagging,ICO_bagging_pred_recall)
  precision_list_bagging <- append(precision_list_bagging,ICO_bagging_pred_precision)
  accuracy_list_bagging <- append(accuracy_list_bagging,ICO_bagging_pred_accuracy)
  specificity_list_bagging <- append(specificity_list_bagging,ICO_bagging_pred_specificity)
  F1_list_bagging <- append(F1_list_bagging,ICO_bagging_pred_F1)
  
  #create an ROC curve
  #create a prediction object
  pred_object_bagging_CV <- ROCR::prediction(ICO_bagging_CV_results$prob_success,ICO_bagging_CV_results$actual_result)
  
  #calculate the ROC curve
  roc_bagging_fold <- performance(pred_object_bagging_CV,measure = "tpr",x.measure = "fpr")
  
  
  #plot the ROC curve
  plot(roc_bagging_fold,lwd=2,col="blue",main = paste("ROC Curve for a Decision Tree with 46 Bags - Fold",i))
  abline(0,1,lwd=2,lty=2)
  
  #calculate the AUC value
  auc_bagging_fold_object <- performance(pred_object_bagging_CV,measure = "auc")
  auc_bagging_fold <- auc_bagging_fold_object@y.values[[1]]
  
  #append the AUC values
  AUC_list_bagging <- append(AUC_list_bagging,auc_bagging_fold)
  
  if (i == 3) { best_ROC_Bagged <- roc_bagging_fold }
  
}

bagging46_CV_performance_metrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Accuracy","SD"),
  Precision = c(precision_list_bagging,mean(precision_list_bagging),sd(precision_list_bagging)),
  Recall = c(recall_list_bagging,mean(recall_list_bagging),sd(recall_list_bagging)),
  AUC = c(AUC_list_bagging,mean(AUC_list_bagging),sd(AUC_list_bagging)),
  Accuracy = c(accuracy_list_bagging,mean(accuracy_list_bagging),sd(accuracy_list_bagging)),
  Specificity = c(specificity_list_bagging,mean(specificity_list_bagging),sd(specificity_list_bagging)),
  F1 = c(F1_list_bagging,mean(F1_list_bagging),sd(F1_list_bagging))
)



writexl::write_xlsx(x = bagging46_CV_performance_metrics, path = "DT bagging 46 CV.xlsx")

