# Bagging
set.seed(322) 
untuned_bag <- bagging( 
  formula = Vaccination_Status ~ ., 
  data = trains) 
untuned_bagging_pred <- predict(untuned_bag, test) 
untuned_bagging_target <- as.factor(untuned_bagging_pred$class)
set.seed(322) 
bagging_model <- bagging( 
  formula = Vaccination_Status ~ ., 
  data = trains, 
  nbagg = 100)
tuned_bagging_pred <- predict(bagging_model, test) 
tuned_bagging_target <- as.factor(tuned_bagging_pred$class) 

confusionMatrix(untuned_bagging_target,
                test$Vaccination_Status)
confusionMatrix(tuned_bagging_target,
                test$Vaccination_Status) 
