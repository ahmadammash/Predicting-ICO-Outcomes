

# Random Forest Decision Tree -----------------------------------------------------------------
DT_train_RF <- DT_train
DT_test_RF <- DT_test

#Dealing with the missing values
library(mice)
corrgram(DT_train_RF)
missing_values <- select(DT_train_RF,c(priceUSD,teamSize))
imi <- mice(missing_values)
missing_values <- complete(imi)
DT_train_RF$priceUSD <- missing_values$priceUSD
DT_train_RF$teamSize <- missing_values$teamSize
sum(is.na(DT_train_RF))

corrgram(DT_test_RF)
missing_values <- select(DT_test_RF,c(priceUSD,teamSize))
imi <- mice(missing_values)
missing_values <- complete(imi)
DT_test_RF$priceUSD <- missing_values$priceUSD
DT_test_RF$teamSize <- missing_values$teamSize
sum(is.na(DT_test_RF))


# Iterate to find the optimal number of trees -------------------------------------------------
library(randomForest)
randomForest_recall_list <- as.numeric()
randomForest_precision_list <- as.numeric()
randomForest_auc_list <- as.numeric()
set.seed(585)
for (i in seq(1,1000,50)) {
  ICO_randomForest <- randomForest(formula = success~., data = DT_train_RF, 
                                   ntree = i)
  ICO_randomForest_pred <- predict(ICO_randomForest, select(DT_test_RF,-success), type = "response")
  ICO_randomForest_prob <- predict(ICO_randomForest,select(DT_test_RF,-success),type = "prob")
  CrossTable(ICO_randomForest_pred, DT_test_RF$success)
  ICO_randomForest_results <- data.frame(
    actual_result = DT_test_RF$success,
    predicted_result = ICO_randomForest_pred,
    prob_failure = round(ICO_randomForest_prob[,1],5),
    prob_success = round(ICO_randomForest_prob[,2],5)
  )
  head(ICO_randomForest_results)
  ICO_randomForest_CM <- confusionMatrix(ICO_randomForest_results$predicted_result, 
                                         ICO_randomForest_results$actual_result,
                                         positive = "Y")
  ICO_randomForest_recall <- ICO_randomForest_CM$byClass['Recall']
  ICO_randomForest_precision <- ICO_randomForest_CM$byClass['Precision']
  
  ICO_randomForest_predobject <- ROCR::prediction(ICO_randomForest_results$prob_success,
                                            ICO_randomForest_results$actual_result)
  ICO_randomForest_AUC_obj <- performance(ICO_randomForest_predobject, measure = "auc")
  ICO_randomForest_AUC <- ICO_randomForest_AUC_obj@y.values[[1]]
  
  randomForest_recall_list <- append(randomForest_recall_list,ICO_randomForest_recall)
  randomForest_precision_list <- append(randomForest_precision_list,ICO_randomForest_precision)
  randomForest_auc_list <- append(randomForest_auc_list, ICO_randomForest_AUC)
}

randomForest_iterations_performance_metrics <- data.frame(
  Precision = randomForest_precision_list,
  Recall = randomForest_recall_list,
  AUC = randomForest_auc_list
)
library(gridExtra)
sequence = seq(1,1000,50)
precision_plot <- ggplot(aes(x = sequence,y = Precision),data = randomForest_iterations_performance_metrics) + 
  geom_point() + xlab('Number of Trees') + ylab('Precision') + scale_x_continuous(breaks = seq(0,1000,100)) + 
  theme(text = element_text(size = 15))
recall_plot <- ggplot(aes(x = sequence,y = Recall),data = randomForest_iterations_performance_metrics) + 
  geom_point() + xlab('Number of Trees') + ylab('Recall') + scale_x_continuous(breaks = seq(0,1000,100)) + 
  theme(text = element_text(size = 15))
AUC_plot <- ggplot(aes(x = sequence,y = AUC),data = randomForest_iterations_performance_metrics) + 
  geom_point() + xlab('Number of Trees') + ylab('AUC') + scale_x_continuous(breaks = seq(0,1000,100)) + 
  theme(text = element_text(size = 15))

grid.arrange(precision_plot, recall_plot, AUC_plot,
             top = textGrob("Plot of Random Forest Performance Metrics for Different Numbers of Trees",
                            gp = gpar(fontsize = 20)))

#850 optimal

# Apply Cross Validation on Random Forest 850 Trees -------------------------------------------

data_RF_CV <- data_DT
str(data_RF_CV)

# Dealing with Missing Values -----------------------------------------------------------------

missing_values <- select(data_RF_CV,c(priceUSD,teamSize))
imi <- mice(missing_values)
missing_values <- complete(imi)
data_RF_CV$priceUSD <- missing_values$priceUSD
data_RF_CV$teamSize <- missing_values$teamSize
sum(is.na(data_RF_CV))


#Creating empty lists to store performance metrics
recall_list_RF <- as.numeric()
precision_list_RF <- as.numeric()
AUC_list_RF <- as.numeric()
accuracy_list_RF <- as.numeric()
specificity_list_RF <- as.numeric()
F1_list_RF <- as.numeric()

K = 10

#create the folds
library(caret)
set.seed(585)
folds_RF <- createFolds(data_RF_CV$success,k = K)
str(folds_RF)
#we can see the 10 folds

#now loop through the folds
for (i in 1:K) {
  
  #sampling for training and testing
  ICO_RF_test_fold <- data_RF_CV[folds_RF[[i]],]
  ICO_RF_train_fold <- data_RF_CV[-folds_RF[[i]],]
  
  #train the model with the RF
  library(randomForest)
  ICO_RF <- randomForest(formula = success~., data = ICO_RF_train_fold, 
                         ntree = 850)
  ICO_randomForest_pred <- predict(ICO_randomForest, select(DT_test_RF,-success), type = "response")
  ICO_randomForest_prob <- predict(ICO_randomForest,select(DT_test_RF,-success),type = "prob")
  
  #predict on the test data
  ICO_RF_predict <- predict(ICO_RF,select(ICO_RF_test_fold,-success), type = "response")
  ICO_RF_predict_prob <- predict(ICO_RF,select(ICO_RF_test_fold,-success), type = "prob")
  
  #create a dataframe
  ICO_RF_CV_results <- data.frame(
    actual_result = ICO_RF_test_fold$success,
    predicted_result = ICO_RF_predict,
    prob_failure = round(ICO_RF_predict_prob[,1],5),
    prob_success = round(ICO_RF_predict_prob[,2],5)
  )
  
  #generate the confusion matrix
  ICO_RF_CM <- caret::confusionMatrix(data = ICO_RF_predict, reference = ICO_RF_test_fold$success,
                                      positive = "Y")
  print(ICO_RF_CM)
  
  #view the precision measure
  ICO_RF_pred_precision <- ICO_RF_CM$byClass['Precision']
  
  #view the recall measure
  ICO_RF_pred_recall <- ICO_RF_CM$byClass['Recall']
  
  #view the accuracy measure
  ICO_RF_pred_accuracy <- ICO_RF_CM$overall['Accuracy']
  
  #view the specificity measure
  ICO_RF_pred_specificity <- ICO_RF_CM$byClass['Specificity']
  
  #view the F1 measure
  ICO_RF_pred_F1 <- ICO_RF_CM$byClass['F1']
  
  #append the performance metrics
  recall_list_RF <- append(recall_list_RF,ICO_RF_pred_recall)
  precision_list_RF <- append(precision_list_RF,ICO_RF_pred_precision)
  accuracy_list_RF <- append(accuracy_list_RF,ICO_RF_pred_accuracy)
  specificity_list_RF <- append(specificity_list_RF,ICO_RF_pred_specificity)
  F1_list_RF <- append(F1_list_RF,ICO_RF_pred_F1)
  
  #create an ROC curve
  library(ROCR)
  #create a prediction object
  pred_object_RF_CV <- ROCR::prediction(ICO_RF_CV_results$prob_success,ICO_RF_CV_results$actual_result)
  
  #calculate the ROC curve
  roc_RF_fold <- performance(pred_object_RF_CV,measure = "tpr",x.measure = "fpr")
  
  #plot the ROC curve
  plot(roc_RF_fold,lwd=2,col="blue",main = paste("ROC Curve of Random Forest with 850 Trees - Fold",i))
  abline(0,1,lwd=2,lty=2)
  
  #calculate the AUC value
  auc_RF_fold_object <- performance(pred_object_RF_CV,measure = "auc")
  auc_RF_fold <- auc_RF_fold_object@y.values[[1]]
  
  #append the AUC values
  AUC_list_RF <- append(AUC_list_RF,auc_RF_fold)
  
  if(i == 3) {best_ROC_RF <- roc_RF_fold}
}

RF_CV_performance_metrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Accuracy","SD"),
  Precision = c(precision_list_RF,mean(precision_list_RF),sd(precision_list_RF)),
  Recall = c(recall_list_RF,mean(recall_list_RF),sd(recall_list_RF)),
  AUC = c(AUC_list_RF,mean(AUC_list_RF),sd(AUC_list_RF)),
  Accuracy = c(accuracy_list_RF,mean(accuracy_list_RF),sd(accuracy_list_RF)),
  Specificity = c(specificity_list_RF,mean(specificity_list_RF),sd(specificity_list_RF)),
  F1 = c(F1_list_RF,mean(F1_list_RF),sd(F1_list_RF))
)

writexl::write_xlsx(x = RF_CV_performance_metrics, path = "DT RF CV performance metrics.xlsx")



