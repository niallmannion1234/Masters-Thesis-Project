# 9. Neural Network
set.seed(1234)
tune_nnet <- tune.nnet(Vaccination_Status ~., data=trained, size = 1:15) 
plot(tune_nnet, main = "Tuning Neural Network")

fitControl <- trainControl(method = "repeatedcv",  
                           number = 10,  
                           repeats = 1,  
                           classProbs = TRUE,  
                           summaryFunction = twoClassSummary) 
nnetGrid <-  expand.grid(size = c(10),
                         decay = c(0.1, 0.5, 0.9, 1.3)) 
set.seed(1234) 
nnetFit <- caret::train(Vaccination_Status ~ .,  
                        data = trained, 
                        method = "nnet", 
                        metric = "ROC", 
                        trControl = fitControl, 
                        tuneGrid = nnetGrid, 
                        verbose = FALSE) 
nnetFit
plot(nnetFit, main = "Neural Network Accuracy By Weight Decay") 

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
train_nn <- trains
train_nn %<>% mutate_if(is.factor, as.numeric)
set.seed(4343)
nn <- neuralnet(Vaccination_Status ~.,
                data=train_nn,
                hidden = c(12,7),
                linear.output = F,
                lifesign = 'full',
                rep=1)
plot(nn,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

set.seed(232)
untuned_nnet <- nnet(Vaccination_Status ~., data=trains, size = 10)  
untuned_nnet_pred <- predict(untuned_nnet, test) 
untuned_nnet_pred <- ifelse(untuned_nnet_pred >= 0.5,0,1)  
untuned_nnet_pred <- as.factor(untuned_nnet_pred)
set.seed(434)
tuned_nnet <- nnet(Vaccination_Status ~., data=trains, size = 10, decay = 0.1)
tuned_nnet_pred <- predict(tuned_nnet, test)
tuned_nnet_pred <- ifelse(tuned_nnet_pred >= 0.5,0,1)  
tuned_nnet_pred <- as.factor(tuned_nnet_pred)

confusionMatrix(untuned_nnet_pred,
                nnet_test)
confusionMatrix(tuned_nnet_pred,
                nnet_test)
