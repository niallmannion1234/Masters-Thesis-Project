# 6. C5.0
# Tune C50 model
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, repeats = 3, returnResamp="all")
grid <- expand.grid( .winnow = c(TRUE), .trials=c(1, 25, 50, 75, 100), .model="tree" ) 
set.seed(1231)
C50_tune <- caret::train(x = trained[-14],
                         y = trained[,14], 
                         tuneGrid=grid, 
                         trControl=fitControl, 
                         method="C5.0",VERBOSE=FALSE) 
plot(C50_tune, main = "Plot of Optimal Trials for C5.0",
     xlab = "Trials", ylab = "accuracy")

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(100), .model="tree" )
set.seed(1231)
C50_tune_winnow <- caret::train(x=trained[-14] ,
                                y=trained[,14],
                                tuneGrid=grid,
                                trControl=fitControl,
                                method="C5.0",VERBOSE=FALSE)
winnowplot <- C50_tune_winnow$results
winnowplot <- ggplot(data=winnowplot, aes(x = Accuracy, y = winnow, fill = winnow)) +
  geom_bar(stat="identity") +
  ggtitle("Accuracy of C50 Model Based on Winnow Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Accuracy") + theme(legend.position = "none")
winnowplot

# Build tuned and untuned C50
set.seed(1244)
C50_default <- C5.0(trains[-14], trains$Vaccination_Status)
untuned_C50_pred <- predict(C50_default, test)
grid <- expand.grid( .winnow = c(FALSE), .trials=c(100), .model="tree" )
set.seed(1231)
C50_tuned <- caret::train(x=trains[-14] ,
                          y=trains[,14],
                          tuneGrid=grid,
                          method="C5.0",VERBOSE=FALSE)
tuned_C50_pred <- predict(C50_tuned, test)

confusionMatrix(untuned_C50_pred,
                test$Vaccination_Status)
confusionMatrix(tuned_C50_pred,
                test$Vaccination_Status)