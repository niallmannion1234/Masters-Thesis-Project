# 5. Naive Bayes 
features <- setdiff(names(trained), "Vaccination_Status") 
x <- trained[, features] 
y <- trained$Vaccination_Status 
train_control <- trainControl(method = "cv", number = 10) 
search_grid <- expand.grid(usekernel = c(TRUE, FALSE),
                           fL = 0:5, adjust = seq(0, 5, by = 1)) 
set.seed(1234) 
tuning_nb_model <- caret::train( 
  x = x, 
  y = y, 
  method = "nb",   trControl = train_control, 
  tuneGrid = search_grid, 
  preProc = c("BoxCox", "center", "scale")) 
plot(tuning_nb_model)

set.seed(434)
untuned_naive_bayes_model <- naiveBayes(Vaccination_Status ~ ., data = trains)
untuned_naive_bayes <- predict(untuned_naive_bayes_model, test)
search_grid <- expand.grid(usekernel = c(TRUE), fL = 1, adjust = 1)  
set.seed(237)
tuned_nb_model <- caret::train( x = x, y = y, method = "nb",  tuneGrid = search_grid,   
                                preProc = c("BoxCox", "center", "scale", "pca"))  
tuned_naive_bayes <- predict(tuned_nb_model, test, type = "raw") 
confusionMatrix(untuned_naive_bayes,
                test$Vaccination_Status) 
confusionMatrix(tuned_naive_bayes,
                test$Vaccination_Status)