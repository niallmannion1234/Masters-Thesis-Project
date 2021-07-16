# 7. gbm
gbmGrid <- expand.grid(interaction.depth = seq(1,20, by = 2),
                       n.trees = c(25, 50, 100, 200), shrinkage = c(0, 1),
                       n.minobsinnode = 10)
boostctrl <- trainControl(method = "repeatedcv", number = 10, 
                          repeats = 1, classProbs = FALSE, savePredictions = T)
set.seed(323)
tuning_gbm <- caret::train(Vaccination_Status ~ ., 
                           data = trained, method = "gbm", trControl = boostctrl,
                           verbose = FALSE, tuneGrid = gbmGrid)
plot(tuning_gbm, main = "Tuning GBM")

# build tuned/untuned model
training <- trains
training$Vaccination_Status <- as.numeric(training$Vaccination_Status)-1
set.seed(232)
untuned_gbm <- gbm(Vaccination_Status ~ ., data = training, n.trees = 100)
untuned_gbm_pred <- predict(untuned_gbm, test, n.trees =  100)
untuned_gbm_pred <- ifelse(untuned_gbm_pred >= 0.5, 0, 1)
target <- as.numeric(test[,14])-1
untuned_gbm_pred <- as.factor(target)

set.seed(3233)
tuned_gbm <- gbm(Vaccination_Status ~ ., data = training,
                 n.trees = 200, interaction.depth = 20)
tuned__gbm_pred <- predict(tuned_gbm, test, n.trees = 200)
tuned__gbm_pred <- ifelse(tuned__gbm_pred >= 0, 0, 1)
tuned__gbm_pred <- as.factor(tuned__gbm_pred)

testing <- test
testing$Vaccination_Status <- as.numeric(testing$Vaccination_Status)-1
testing$Vaccination_Status <- as.factor(testing$Vaccination_Status)
confusionMatrix(untuned_gbm_pred,
                testing$Vaccination_Status)
confusionMatrix(tuned__gbm_pred,
                testing$Vaccination_Status)