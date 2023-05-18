# DECISION TREE -------------------------------------------------------------------------------
library(ROCR)
library(caret)
library(dplyr)

data_DT <- data
data_DT$duration <- as.numeric(data_DT$duration)
sum(is.na(data_DT))
sum(is.na(data_DT$priceUSD))
sum(is.na(data_DT$teamSize))
colnames(data_DT)


# Split the data to training and testing ------------------------------------------------------

ratioDT <- 0.85
smp_sizeDT <- floor(ratioDT*nrow(data_DT))
set.seed(585)
train_ind_DT <- sample(nrow(data_DT),smp_sizeDT)
DT_train <- data_DT[train_ind_DT,]
DT_test <- data_DT[-train_ind_DT,]
str(DT_train)


# Missing Values & Feature Selection ---------------------------------------------------------------------------

# DT_train_FeatureSelection <- DT_train
# corrgram(DT_train_FeatureSelection)
# missing_values <- select(DT_train_FeatureSelection,c(priceUSD,teamSize))
# imi <- mice(missing_values)
# missing_values <- complete(imi)
# DT_train_FeatureSelection$priceUSD <- missing_values$priceUSD
# DT_train_FeatureSelection$teamSize <- missing_values$teamSize
# sum(is.na(DT_train_FeatureSelection))
# 
# control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# results <- rfe(select(DT_train_FeatureSelection,-success),
#                DT_train_FeatureSelection$success, sizes = c(1:21), rfeControl = control)
# print(results)
# predictors(results)
# plot(results, type = c("g","o"),main = "RFE Accuracy Plot for Feature Combinations - Decision Tree Models",
#      cex.main = 2)

DT_train <- DT_train %>% select(c(success,rating, teamSize, endYear, startYear, hasVideo, duration, 
                                  endMonth, hasReddit, startMonth, hasGithub, priceUSD,
                                  is_most_friendly, is_USA, startDay))
DT_test <- DT_test %>% select(c(success,rating, teamSize, endYear, startYear, hasVideo, duration, 
                                endMonth, hasReddit, startMonth, hasGithub, priceUSD,
                                is_most_friendly, is_USA, startDay))


data_DT <- data_DT %>% select(c(success,rating, teamSize, endYear, startYear, hasVideo, duration, 
                                endMonth, hasReddit, startMonth, hasGithub, priceUSD,
                                is_most_friendly, is_USA, startDay, endDay, distributedPercentage))