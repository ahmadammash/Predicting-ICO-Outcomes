#ANN
library(dplyr)
data_ANN <- data
dim(data_ANN)
str(data_ANN)
sum(is.na(data_ANN))

#All features must be numeric and normalized
#create the normalization function
normalize <- function(x) {
  return ((x-mean(x,na.rm = T)) / sd(x,na.rm = T))
}

#normalize the numeric features
data_ANN$rating <- normalize(data_ANN$rating)
data_ANN$priceUSD <- normalize(data_ANN$priceUSD)
data_ANN$startYear <- normalize(data_ANN$startYear)
data_ANN$endYear <- normalize(data_ANN$endYear)
data_ANN$startMonth <- normalize(data_ANN$startMonth)
data_ANN$endMonth <- normalize(data_ANN$endMonth)
data_ANN$startDay <- normalize(data_ANN$startDay)
data_ANN$endDay <- normalize(data_ANN$endDay)
data_ANN$coinNum <- normalize(data_ANN$coinNum)
data_ANN$distributedPercentage <- normalize(data_ANN$distributedPercentage)
data_ANN$teamSize <- normalize(data_ANN$teamSize)
data_ANN$duration <- normalize(data_ANN$duration)
data_ANN$Slogan_Length <- normalize(data_ANN$Slogan_Length)



# Impute the Missing Values with the missForest Package ---------------------------------------
library(missForest)
set.seed(585)
imputed_values <- missForest(data_ANN)
data_ANN <- imputed_values$ximp
dim(data_ANN)
sum(is.na(data_ANN))

#splitting the data to train and test
ratioANN <- 0.85
smp_sizeANN <- floor(ratioANN*nrow(data_ANN)) 
set.seed(585)
train_ind_ANN <- sample(nrow(data_ANN),smp_sizeANN)

data_ANN_train <- data_ANN[train_ind_ANN,]
data_ANN_test <- data_ANN[-train_ind_ANN,]
ANN_train_labels <- data_ANN_train$success
ANN_test_labels <- data_ANN_test$sucess
dim(data_ANN_train)
dim(data_ANN_test)


# Feature Selection ---------------------------------------------------------------------------
library(caret)
# ANN_train_FeatureSelection <- data_ANN_train
# control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# results <- rfe(select(ANN_train_FeatureSelection,-success),
#                ANN_train_FeatureSelection$success, sizes = c(1:19), rfeControl = control)
# print(results)
# predictors(results)
# plot(results, type = c("g","o"), main = "RFE Accuracy Plot for Feature Combinations - ANN",cex.main = 2)

data_ANN_train <- data_ANN_train %>% select(success, rating, teamSize, endYear, startYear, duration,
                                            hasReddit, endMonth, hasVideo, startMonth, hasGithub,
                                            priceUSD, is_most_friendly, startDay, is_USA, 
                                            distributedPercentage, endDay, coinNum)

data_ANN_test <- data_ANN_test %>% select(success, rating, teamSize, endYear, startYear, duration,
                                          hasReddit, endMonth, hasVideo, startMonth, hasGithub,
                                          priceUSD, is_most_friendly, startDay, is_USA, 
                                          distributedPercentage, endDay, coinNum)



# Tuning ANN parameters -----------------------------------------------------------------------
library(nnet)
library(caret)
library(ROCR)

#Define training control
train_control <- trainControl(method = "repeatedcv",number = 10)
fit <- train(success~., data = data_ANN_train, method = "nnet", trControl = train_control)
ICO_ANN <- nnet(success~., data = data_ANN_train, size = 1, decay = 0.0001, maxit = 1000)

library(neuralnet)
library(NeuralNetTools)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


# Apply Cross Validation on ANN ---------------------------------------------------------------

data_ANN_CV <- data
dim(data_ANN_CV)
str(data_ANN_CV)
sum(is.na(data_ANN_CV))
data_ANN_CV <- data_ANN_CV %>% select(c(success, rating, teamSize, endYear, startYear, duration,
                        hasReddit, endMonth, hasVideo, startMonth, hasGithub,
                        priceUSD, is_most_friendly, startDay, is_USA, 
                        distributedPercentage, endDay, coinNum))

data_ANN_CV$rating <- normalize(data_ANN$rating)
data_ANN_CV$priceUSD <- normalize(data_ANN$priceUSD)
data_ANN_CV$startYear <- normalize(data_ANN$startYear)
data_ANN_CV$endYear <- normalize(data_ANN$endYear)
data_ANN_CV$startMonth <- normalize(data_ANN$startMonth)
data_ANN_CV$endMonth <- normalize(data_ANN$endMonth)
data_ANN_CV$startDay <- normalize(data_ANN$startDay)
data_ANN_CV$endDay <- normalize(data_ANN$endDay)
data_ANN_CV$coinNum <- normalize(data_ANN$coinNum)
data_ANN_CV$distributedPercentage <- normalize(data_ANN$distributedPercentage)
data_ANN_CV$teamSize <- normalize(data_ANN$teamSize)
data_ANN_CV$duration <- normalize(data_ANN$duration)


# Impute the Missing Values with the missForest Package ---------------------------------------
library(missForest)
set.seed(585)
imputed_values <- missForest(data_ANN_CV)
data_ANN_CV <- imputed_values$ximp
dim(data_ANN_CV)
sum(is.na(data_ANN_CV))
K = 10
#Create the folds
set.seed(585)
folds_ANN <- createFolds(data_ANN_CV$success,k = K)
str(folds_ANN)

precision_list_ANN_CV <- as.numeric()
recall_list_ANN_CV <- as.numeric()
auc_list_ANN_CV <- as.numeric()
accuracy_list_ANN_CV <- as.numeric()
specificity_list_ANN_CV <- as.numeric()
F1_list_ANN_CV <- as.numeric()


for(i in 1:K) {
  data_ANN_CV_fold_test <- data_ANN_CV[folds_ANN[[i]],]
  data_ANN_CV_fold_train <- data_ANN_CV[-folds_ANN[[i]],]
  data_ANN_CV_fold_test_labels <- data_ANN_CV[folds_ANN[[i]],"success"]
  data_ANN_CV_fold_train_labels <- data_ANN_CV[-folds_ANN[[i]],"success"]
  
  #train an ANN
  ICO_ANN_CV <- nnet(success~., data = data_ANN_CV_fold_train, size = 1, decay = 0.0001, maxit = 1000)

  #generate the predictions
  predicted_classes <- predict(ICO_ANN_CV,data_ANN_CV_fold_test,type="class")
  probability_success <- predict(ICO_ANN_CV, select(data_ANN_CV_fold_test,-success),type="raw")
  probability_classes <- data.frame(
    N = 1 - probability_success,
    Y = probability_success
  )
  ICO_ANN_CV_results <- data.frame(
    actual_result = data_ANN_CV_fold_test$success,
    predicted_result = predicted_classes,
    prob_success = probability_classes$Y,
    prob_failure = probability_classes$N
  )
  ICO_ANN_CV_CM <- confusionMatrix(as.factor(ICO_ANN_CV_results$predicted_result),
                                   as.factor(ICO_ANN_CV_results$actual_result)
                                , positive = "Y")
  print(ICO_ANN_CV_CM)
  predObject_ANN_CV <- ROCR::prediction(ICO_ANN_CV_results$prob_success,ICO_ANN_CV_results$actual_result)
  rocObject_ANN_CV <- ROCR::performance(predObject_ANN_CV,measure = "tpr",x.measure = "fpr")
  plot(rocObject_ANN_CV,lwd=2,col="blue",main = paste0("ROC Curve for ANN CV - Fold ",i))
  abline(0,1,lwd=2,lty=2)
  aucObject_ANN_CV <- ROCR::performance(predObject_ANN_CV,measure="auc")
  auc_ANN_CV <- aucObject_ANN_CV@y.values[[1]]
  
  precision_ANN_CV <- ICO_ANN_CV_CM$byClass['Precision']
  recall_ANN_CV <- ICO_ANN_CV_CM$byClass['Recall']
  accuracy_ANN_CV <- ICO_ANN_CV_CM$overall['Accuracy']
  specificity_ANN_CV <- ICO_ANN_CV_CM$byClass['Specificity']
  f1_ANN_CV <- ICO_ANN_CV_CM$byClass['F1']

  precision_list_ANN_CV <- append(precision_list_ANN_CV,precision_ANN_CV)
  recall_list_ANN_CV <- append(recall_list_ANN_CV,recall_ANN_CV)
  auc_list_ANN_CV <- append(auc_list_ANN_CV,auc_ANN_CV)
  accuracy_list_ANN_CV <- append(accuracy_list_ANN_CV,accuracy_ANN_CV)
  specificity_list_ANN_CV <- append(specificity_list_ANN_CV,specificity_ANN_CV)
  F1_list_ANN_CV <- append(F1_list_ANN_CV,f1_ANN_CV)
  
  plot.nnet(ICO_ANN_CV,col = "blue",main = paste0("Neural Network Plot - Fold ",i),cex = 1.3,
            cex.main = 1.8)
  if(i == 3) {best_ROC_ANN <- rocObject_ANN_CV}
  
}
ANN_CV_performance_metrics <- data.frame(
  FoldNumber = c(seq(1,10,1),"Average","SD"),
  Precision = c(precision_list_ANN_CV,mean(precision_list_ANN_CV),sd(precision_list_ANN_CV)),
  Recall = c(recall_list_ANN_CV,mean(recall_list_ANN_CV),sd(recall_list_ANN_CV)),
  AUC = c(auc_list_ANN_CV,mean(auc_list_ANN_CV),sd(auc_list_ANN_CV)),
  Accuracy = c(accuracy_list_ANN_CV,mean(accuracy_list_ANN_CV),sd(accuracy_list_ANN_CV)),
  Specificity = c(specificity_list_ANN_CV,mean(specificity_list_ANN_CV),sd(specificity_list_ANN_CV)),
  F1 = c(F1_list_ANN_CV,mean(F1_list_ANN_CV),sd(F1_list_ANN_CV))
)

writexl::write_xlsx(x = ANN_CV_performance_metrics,path = "ANN CV performance metrics.xlsx")
