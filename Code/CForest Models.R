# 8. C-Forest
ctree_control <- trainControl(method = 'cv', number=5,summaryFunction=defaultSummary)  
ctree_tune_grid <- expand.grid(mincriterion=c(0.010, 0.255, 0.50, 0.745, 0.990),  
                               maxdepth = seq(15, 50, 5))

# create formula without region (c-forest produces errors with region because of it's many levels)
ctree_formula <- Vaccination_Status ~ Education_Status + Income_Group +   
  Number_Providers + Household_Size + Child_Number + Insurance_Type +   
  Provider_Facility + Marital_Status + Duration + Race + House_Ownership_Status +  
  Mother_Age_Group + Firstborn 
set.seed(1234)
ctree_tune <- caret::train(ctree_formula, data=trained, method = 'ctree2',
                           trControl = ctree_control,tuneGrid = ctree_tune_grid)  
plot(ctree_tune, main = "Tuning C-Forest", ylab = "Accuracy")

testing <- test[ ,c(1:16)]

# build tuned/untuned c-forest
set.seed(343)
untuned_cforest <- cforest(ctree_formula, data = trains)
untuned_ctree_pred <- predict(untuned_cforest, test) 

Grid <- expand.grid(mincriterion=c(0.01),  
                    maxdepth = c(15))
set.seed(1234)
ctree_tune <- caret::train(ctree_formula, data=trains, method = 'ctree2',   
                           trControl=fitControl,tuneGrid=Grid)  
tuned_ctree_pred <- predict(ctree_tune, testing) 

confusionMatrix(untuned_ctree_pred,
                test$Vaccination_Status)  
confusionMatrix(tuned_ctree_pred,
                test$Vaccination_Status)