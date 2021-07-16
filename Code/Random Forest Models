# Random Forest
trained <- sample_n(trains, 10000)
trains <- na.omit(trains)
test <- na.omit(test)
set.seed(3232)
tuning_rf <- tuneRF(trains[,-14], trains[,14], stepFactor = 2.5, plot = TRUE)
plot(tuning_rf)
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')
tunegrid <- expand.grid(.mtry=c(16))
modellist <- list()
for(ntree in c(100, 250, 500, 1000)) {set.seed(3233)
  fit <- caret::train(Vaccination_Status~., data = trained,
                      method = "rf", tuneGrid = tunegrid, trControl = control, ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit}
results <- resamples(modellist)
summary(results)

set.seed(1234)
untuned_rf_model <- randomForest(Vaccination_Status ~., data = trains)
untuned_rf_pred <- predict(untuned_rf_model, test)
set.seed(323)
tuned_rf_model <- randomForest(Vaccination_Status ~., data = trains, ntree = 100, mtry = 16)
tuned_rf_pred <- predict(untuned_rf_model, test)

confusionMatrix(untuned_rf_pred,
                test$Vaccination_Status)
confusionMatrix(tuned_rf_pred,
                test$Vaccination_Status)
