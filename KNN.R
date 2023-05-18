# NEAREST NEIGHBOURS -------------------------------------------------------------------------------
library(class)
#copy the dataset
data_NN <- data
dim(data_NN)
str(data_NN)
sum(is.na(data_NN))


# Create the Normalization Function -----------------------------------------------------------
#create the normalize function
normalize <- function(x) {
  return ((x-mean(x,na.rm = T)) / sd(x,na.rm = T))
}

#Need to scale all the numeric columns
#use either max-min normalization if we know the plausible min and max values ahead of time
#or the z-score standardization under the assumption that the future examples will have similar
#mean and standard deviation as the training examples.
#Need to dummy code all categorical columns
#could use max and min as 5 and 1 or as the observed ones - try both

# Normalizing the Numeric Features -----------------------------------------------------------------
data_NN$rating <- normalize(data_NN$rating)
data_NN$coinNum <- normalize(data_NN$coinNum)
data_NN$teamSize <- normalize(data_NN$teamSize)
data_NN$distributedPercentage <- normalize(data_NN$distributedPercentage)
data_NN$duration <- normalize(data_NN$duration)
data_NN$priceUSD <- normalize(data_NN$priceUSD)
data_NN$Slogan_Length <- normalize(data_NN$Slogan_Length)
data_NN$startYear <- normalize(data_NN$startYear)
data_NN$endYear <- normalize(data_NN$endYear)
data_NN$startMonth <- normalize(data_NN$startMonth)
data_NN$endMonth <- normalize(data_NN$endMonth)
data_NN$startDay <- normalize(data_NN$startDay)
data_NN$endDay <- normalize(data_NN$endDay)



# Splitting the data to train and test --------------------------------------------------------
ratioNN <- 0.85
smp_sizeNN <- floor(ratioNN*nrow(data_NN)) 
set.seed(585)
train_ind_NN <- sample(nrow(data_NN),smp_sizeNN)
data_NN_train <- data_NN[train_ind_NN,]
data_NN_test <- data_NN[-train_ind_NN,]
merged_train_NN <- data_NN_train
merged_test_NN <- data_NN_test

# MISSING VALUES IMPUTATION -------------------------------------------------------------------
library(VIM)
merged_train_NN <- kNN(data = merged_train_NN, variable = c("teamSize","priceUSD"), k = 41)
merged_test_NN <- kNN(data = merged_test_NN,variable = c("teamSize","priceUSD"),k = 41)
merged_train_NN <- merged_train_NN %>% select(-teamSize_imp,-priceUSD_imp)
merged_test_NN <- merged_test_NN %>% select(-teamSize_imp,-priceUSD_imp)

#Feature Selection ---------------------------------------------------------------------------

# NN_train_FeatureSelection <- merged_train_NN
# control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# results <- rfe(select(NN_train_FeatureSelection,-success),
#                NN_train_FeatureSelection$success, sizes = c(1:21), rfeControl = control)
# print(results)
# predictors(results)
# plot(results, type = c("g","o"),main = "RFE Accuracy Plot for Feature Combinations - Nearest Neighbours",
#      cex.main = 2)

merged_train_NN <- merged_train_NN %>% select(success, rating, teamSize, startYear, endYear, hasVideo,
                                              duration, endMonth, hasReddit, startMonth, hasGithub,
                                              priceUSD, is_most_friendly, startDay,hasReddit)

merged_test_NN <- merged_test_NN %>% select(success, rating, teamSize, startYear, endYear, hasVideo,
                                            duration, endMonth, hasReddit, startMonth, hasGithub,
                                            priceUSD, is_most_friendly, startDay,hasReddit)

train_labels_NN <- merged_train_NN$success
test_labels_NN <- merged_test_NN$success

merged_train_NN <- merged_train_NN %>% select(-success)
merged_test_NN <- merged_test_NN %>% select(-success)


# Choosing the Best value of k  ---------------------------------------------------------------
library(caret)
library(ROCR)
library(class)
precision_list_kFinder <- as.numeric()
recall_list_kFinder <- as.numeric()
auc_list_kFinder <- as.numeric()
kValues <- as.numeric()
for(K in seq(from = 1, to = 110, by = 5))
{
  ICO_kFinder <- knn(train = merged_train_NN, test = merged_test_NN,
                     cl = train_labels_NN, k = K,prob = TRUE)
  
  ICO_kFinder_prob <- predict(knn3(merged_train_NN, train_labels_NN, k = K), merged_test_NN)
  
  ICO_results_kFinder <- data.frame(
    actual_result = test_labels_NN,
    predicted_result = ICO_kFinder,
    prob_success = round(ICO_kFinder_prob[,2],5),
    prob_failure = round(ICO_kFinder_prob[,1],5)
  )
  
  kFinder_CM <- caret::confusionMatrix(ICO_results_kFinder$predicted_result, ICO_results_kFinder$actual_result, 
                                       positive = "Y")
  precision_kFinder <- kFinder_CM$byClass['Precision']
  recall_kFinder <- kFinder_CM$byClass['Recall']
  
  
  predobject_kFinder <- ROCR::prediction(ICO_results_kFinder$prob_success, ICO_results_kFinder$actual_result)
  auc_object_kFinder <- performance(predobject_kFinder,measure = "auc")
  auc_kFinder <- auc_object_kFinder@y.values[[1]]
  
  precision_list_kFinder <- append(precision_list_kFinder,precision_kFinder)
  recall_list_kFinder <- append(recall_list_kFinder,recall_kFinder)
  auc_list_kFinder <- append(auc_list_kFinder,auc_kFinder)
  kValues <- append(kValues,K)
  
}
kFinder <- data.frame(k = kValues,
                      precision = precision_list_kFinder,
                      recall = recall_list_kFinder,
                      AUC = auc_list_kFinder
                      
)

library(gridExtra)
library(ggplot2)

precision_plot_kFinder <- ggplot(data = kFinder,aes(x=kValues,y=precision)) + geom_point() + 
  scale_x_continuous(breaks = seq(1,110,5)) + xlab('Number of Neighbours') + ylab('Precision') +
  theme(text = element_text(size = 15))
recall_plot_kFinder <- ggplot(data = kFinder,aes(x=kValues,y=recall)) + geom_point() + 
  scale_x_continuous(breaks = seq(1,110,5)) + xlab('Number of Neighbours') + ylab('Recall') + 
  theme(text = element_text(size = 15))
AUC_plot_kFinder <- ggplot(data = kFinder,aes(x=kValues,y=AUC)) + geom_point() + 
  scale_x_continuous(breaks = seq(1,110,5)) + xlab('Number of Neighbours') + ylab('AUC') + 
  theme(text = element_text(size = 15))

gridExtra::grid.arrange(precision_plot_kFinder, recall_plot_kFinder,AUC_plot_kFinder,
                        top = textGrob("Plot of Nearest Neighbours Performance Metrics for Different k Values",
                                       gp = gpar(fontsize = 20)))


# Applying Cross Validation on kNN with k = 36 -----------------------------------------------------
data_NN_CV <- data

data_NN_CV <- data %>% select(c(success, rating, teamSize, startYear, endYear, hasVideo,
                                duration, endMonth, hasReddit, startMonth, hasGithub,
                                priceUSD, is_most_friendly, startDay,hasReddit))

#normalizing the numeric features
data_NN_CV$rating <- normalize(data_NN_CV$rating)
data_NN_CV$teamSize <- normalize(data_NN_CV$teamSize)
data_NN_CV$duration <- normalize(data_NN_CV$duration)
data_NN_CV$endMonth <- normalize(data_NN_CV$endMonth)
data_NN_CV$startYear <- normalize(data_NN_CV$startYear)
data_NN_CV$endYear <- normalize(data_NN_CV$endYear)
data_NN_CV$priceUSD <- normalize(data_NN_CV$priceUSD)
data_NN_CV$startDay <- normalize(data_NN_CV$startDay)
data_NN_CV$startMonth <- normalize(data_NN_CV$startMonth)

#Missing values
data_NN_CV <- VIM::kNN(data = data_NN_CV,variable = c("teamSize","priceUSD"),k = 36)
data_NN_CV <- data_NN_CV %>% select(-teamSize_imp)

#create empty lists for storing measures
precision_list_NN_CV <- as.numeric()
recall_list_NN_CV <- as.numeric()
auc_list_NN_CV <- as.numeric()
accuracy_list_NN_CV <- as.numeric()
specificity_list_NN_CV <- as.numeric()
F1_list_NN_CV <- as.numeric()


#Automating the 10-fold CV for kNN 
library(pROC)
library(tidyverse)
library(caret)
K = 10
#create the folds
set.seed(585)
folds_KNN <- createFolds(data_NN_CV$success,k = K)
str(folds_KNN)
#we can see the 10 folds

#loop through the 10 folds
for (i in 1:K) {
  #split the data to training and testing
  data_NN_CV_fold_test <- data_NN_CV[folds_KNN[[i]],]
  data_NN_CV_fold_train <- data_NN_CV[-folds_KNN[[i]],]
  data_NN_CV_fold_train_labels <- data_NN_CV[-folds_KNN[[i]],"success"]
  data_NN_CV_fold_test_labels <- data_NN_CV[folds_KNN[[i]],"success"]
  data_NN_CV_fold_test <- data_NN_CV_fold_test %>% select(-success)
  data_NN_CV_fold_train <- data_NN_CV_fold_train %>% select(-success)
  
  #train the model
  ICO_pred_NN_CV <- class::knn(train = data_NN_CV_fold_train,
                        test = data_NN_CV_fold_test, 
                        cl = data_NN_CV_fold_train_labels, k = 36)
  ICO_prob_NN_CV <- predict(knn3(data_NN_CV_fold_train,
                                 data_NN_CV_fold_train_labels, k = 36),
                            data_NN_CV_fold_test)
  head(ICO_prob_NN_CV)
  #generate the confusion matrix
  ICO_NN_CV_CM <- confusionMatrix(data = ICO_pred_NN_CV, 
                                  reference = (data_NN_CV_fold_test_labels), positive = "Y")
  print(ICO_NN_CV_CM)
  
  #create dataframe showing the predicted and actual results
  ICO_pred_NN_CV_results = data.frame(
    actual_result = data_NN_CV_fold_test_labels,
    predicted_result = ICO_pred_NN_CV,
    prob_failure = round(ICO_prob_NN_CV[,1],5),
    prob_success = round(ICO_prob_NN_CV[,2],5)
  )

  #view the precision measure
  ICO_pred_fold_precision <- ICO_NN_CV_CM$byClass['Precision']

  #view the recall measure
  ICO_pred_fold_recall <- ICO_NN_CV_CM$byClass['Recall']
  
  #view the accuracy measure
  ICO_pred_fold_accuracy <- ICO_NN_CV_CM$overall['Accuracy']
  
  #view the specificity measure
  ICO_pred_fold_specificity <- ICO_NN_CV_CM$byClass['Specificity']
  
  #view the F1 measure
  ICO_pred_fold_F1 <- ICO_NN_CV_CM$byClass['F1']

  #create the prediction object
  pred_object_NN_CV <- ROCR::prediction(predictions = ICO_pred_NN_CV_results$prob_success,
                                  labels = ICO_pred_NN_CV_results$actual_result)
  head(ICO_pred_NN_CV_results)
  #create the ROC curve
  roc_NN_fold <- performance(pred_object_NN_CV, measure = "tpr",x.measure = "fpr")
  plot(roc_NN_fold, main = paste("ROC of kNN CV (k = 36) - Fold",i),lwd=2,col="blue")
  abline(0,1,lwd=2,lty=2)

  #create the AUC object and get AUC value
  auc_NN_fold_object <- performance(pred_object_NN_CV, measure = "auc")
  auc_NN_fold <- auc_NN_fold_object@y.values[[1]]

  #append the performance measures
  precision_list_NN_CV <- append(precision_list_NN_CV, ICO_pred_fold_precision)
  recall_list_NN_CV <- append(recall_list_NN_CV, ICO_pred_fold_recall)
  auc_list_NN_CV <- append(auc_list_NN_CV, auc_NN_fold)
  accuracy_list_NN_CV <- append(accuracy_list_NN_CV,ICO_pred_fold_accuracy)
  specificity_list_NN_CV <- append(specificity_list_NN_CV,ICO_pred_fold_specificity)
  F1_list_NN_CV <- append(F1_list_NN_CV,ICO_pred_fold_F1)
  
  if(i == 10) {best_ROC_kNN <- roc_NN_fold}
}

knn_CV_performance_metrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Average","SD"),
  Precision = c(precision_list_NN_CV,mean(precision_list_NN_CV),sd(precision_list_NN_CV)),
  Recall = c(recall_list_NN_CV,mean(recall_list_NN_CV),sd(recall_list_NN_CV)),
  AUC = c(auc_list_NN_CV,mean(auc_list_NN_CV),sd(auc_list_NN_CV)),
  Accuracy = c(accuracy_list_NN_CV,mean(accuracy_list_NN_CV),sd(accuracy_list_NN_CV)),
  Specificity = c(specificity_list_NN_CV,mean(specificity_list_NN_CV),sd(specificity_list_NN_CV)),
  F1 = c(F1_list_NN_CV,mean(F1_list_NN_CV),sd(F1_list_NN_CV))
)
write_xlsx(knn_CV_performance_metrics, path = "kNN CV.xlsx")

