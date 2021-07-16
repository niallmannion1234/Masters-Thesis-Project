# Feature importance
Project_Theme = theme(
  axis.title.x = element_text(size = 19),
  axis.text.x = element_text(size = 19),
  axis.title.y = element_text(size = 20),
  plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))
imb <- ggplot(compare_samples, aes(x= reorder(Method, -Accuracy), weight=Accuracy, fill = Method))
imb <- imb + geom_bar() + ggtitle("Comparison of Accuracy of Sampling Methods") + Project_Theme
imb <- imb + xlab("Sampling Method") + ylab("Accuracy") 
imb + scale_fill_discrete(name = "Sampling Method")

# use oversampling as training data
trains <- over_trains

# Create training and test dataset for Rmarkdown file
setwd("D:\\AD")
write.csv(test, file= "test_data.csv")
write.csv(trains,"training_data.csv")
# Variable Importance
# Random Forest
set.seed(232) 
imp_rf <- randomForest(Vaccination_Status ~ ., data=trains,  importance=TRUE) 
var_imp <- varImpPlot(imp_rf)
var_imp <- as.data.frame(var_imp)
var_imp$varnames <- rownames(var_imp)
rownames(var_imp) <- NULL  

ggplot(var_imp, aes(x=reorder(varnames, MeanDecreaseAccuracy),
                    weight=MeanDecreaseAccuracy, fill=varnames)) + 
  geom_bar() + scale_fill_discrete(name="Variable Group") +
  ylab("Importance") + xlab("Variable") +
  coord_flip() + theme(legend.position = "none") + 
  ggtitle("Random Forest Variable Importance") +
  theme(plot.title = element_text(hjust = 0.5))

# Boruta Algorithm
set.seed(432)
var_imp_boruta <- Boruta(Vaccination_Status~., data = trains, doTrace = 2)
print(var_imp_boruta)

# 1. Random Forest
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

# 2. Bagging
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

# 3. KNN
# Make the factor levels suitable for models such as knn. This keeps the variables as factors but changes
# the factor level names to simple numerical values that for models

selected <-c("Was_Child_Breastfed", "Household_Size", "Child_Number", "Firstborn", 
             "Insurance_Type", "Education_Status", "Was_Child_Breastfed", 
             "Race", "Mother_Age_Group", "Marital_Status", "House_Ownership_Status", 
             "Provider_Facility", "Number_Providers", "WIC", "Region")
for (i in selected){trains[,i] <- as.numeric(trains[,i])}
for (i in selected){trains[,i] <- as.factor(trains[,i])}
for (i in selected){test[,i] <- as.numeric(test[,i])}
for (i in selected){test[,i] <- as.factor(test[,i])}
# From https://rpubs.com/njvijay/16251
trains <- na.omit(trains)
test <- na.omit(test)
trained <- sample_n(trains, 10000)
set.seed(232)
knn.cross <- tune.knn(x = trained[,-14], y = trained[,14], k = 1:25, 
                      tunecontrol = tune.control(sampling = "cross"), cross=10) 
plot(knn.cross, main = "Optimal Value for K in KNN") 

# Train knn model with default and tuned values for k 
set.seed(324)
knn_untuned <- kNN(Vaccination_Status ~ .,trains, test, norm=FALSE) 
set.seed(232)
knn_tuned <- kNN(Vaccination_Status ~ .,trains, test, norm=FALSE, k = 1) 

confusionMatrix(knn_untuned,
                test$Vaccination_Status)
confusionMatrix(knn_tuned,
                test$Vaccination_Status)

# 4. Support Vector Machines 
# Comparing different svm kernels 
set.seed(1234)
svm_vanilla <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "vanilladot")
vanilla_pred <- predict(svm_vanilla, test)
confusionMatrix(vanilla_pred, test$Vaccination_Status)
vanillaconfusion$overall
vanilla <- vanillaconfusion$overall
vanilla
vanilla$kernel <- 'vanilla'
vanilla <- data.frame(as.list(vanilla))

set.seed(2313)
svm_rbfdot <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "rbfdot")
rbfdot_pred <- predict(svm_rbfdot, test)
rbfdotconfusion <- confusionMatrix(rbfdot_pred, test$Vaccination_Status)
rbfdotconfusion$overall
rbf <- rbfdotconfusion$overall
rbf
rbf$kernel <- 'rbf_dot'
rbf <- data.frame(as.list(rbf))

set.seed(1234)
svm_laplacedot <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "laplacedot")
laplacedot_pred <- predict(svm_laplacedot, test)
laplacedot_confusion <- confusionMatrix(laplacedot_pred, test$Vaccination_Status)
laplacedot_confusion$overall
laplacedot <- laplacedot_confusion$overall
laplacedot
laplacedot$kernel <- 'laplace_dot'
laplacedot <- data.frame(as.list(laplacedot))

set.seed(1234)
svm_classifier <- ksvm(Vaccination_Status ~ ., data = trains, kernel = "besseldot")
besseldot_pred <- predict(svm_classifier, test)
besseldot_confusion <- confusionMatrix(besseldot_pred, test$Vaccination_Status)
besseldot_confusion$overall
besseldot <- besseldot_confusion$overall
besseldot
besseldot$kernel <- 'bessel_dot'
besseldot <- data.frame(as.list(besseldot))
svmkernelcombined <- rbind(rbf, vanilla, laplacedot, besseldot)
ggplot(data=svmkernelcombined, aes(x=reorder(kernel, -Accuracy), y=Accuracy, fill = kernel)) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim=c(0.5, 0.633)) +
  ggtitle("Accuracy of Each SVM Kernel") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Kernel")

# Tune cost function of SVM
set.seed(1424)
svmtune_cost <- tune(svm,
                     Vaccination_Status ~ .,
                     data = trained,
                     kernel='linear',
                     ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 10)))
plot(svmtune_cost, main="Tuning SVM Model: Error vs Cost")
warnings()
set.seed(1424)
svmtune <- tune(svm,
                Vaccination_Status ~ .,
                data = trained,
                kernel='linear',
                ranges=list(sigma = c(0.001,0.003,0.006,0.009)))
plot(svmtune, main="Tuning SVM Model: Error vs Sigma")
warnings()
# Test tuned SVM model
set.seed(323)
untuned_svm_model <- ksvm(Vaccination_Status ~ ., data = trains)
untuned_svm_pred <- predict(untuned_svm_model, test)
set.seed(1424)
tuned_ksvm_model = ksvm(Vaccination_Status~., data=trains, 
                        kpar=list(sigma = .001), C = 10,
                        kernel = "besseldot")
tuned_svm_pred <- predict(tuned_ksvm_model, test)

confusionMatrix(untuned_svm_pred,
                test$Vaccination_Status)
confusionMatrix(tuned_svm_pred, 
                test$Vaccination_Status)

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

# 10. Superlearner
trained <- sample_n(trains, 10000)
test <- na.omit(test)
y <- as.numeric(trained[,14])-1
ytest <- as.numeric(test[,14])-1
x <- trained[ ,c(1:13, 15:17)]
xtest <- test[ ,c(1:13, 15:17)]
set.seed(323)
CV_sl <- CV.SuperLearner(y, x, V =3, 
                         family=binomial(), 
                         SL.library=list("SL.ranger",
                                         "SL.xgboost", "SL.ipredbagg"))
plot(CV_sl)

y <- as.numeric(trains[,14])-1
x <- trains[ ,c(1:13, 15:17)]
set.seed(323)
three_sl <- SuperLearner(y, 
                         x, 
                         family=binomial(), 
                         SL.library=list("SL.ranger",
                                         "SL.xgboost",
                                         "SL.ipredbagg")) 
three__sl_pred <- predict.SuperLearner(three_sl, newdata=xtest) 
conv.preds <- ifelse(three__sl_pred$pred>=0.5,0,1) 
conv.pred <- ytest 
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
three_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
three_sl_cm

set.seed(323)
two_sl <- SuperLearner(y, 
                       x, 
                       family=binomial(), 
                       SL.library=list("SL.ranger",
                                       "SL.xgboost")) 
two_sl_pred <- predict.SuperLearner(two_sl, newdata=xtest) 
conv.preds <- ifelse(two_sl_pred$pred>=0.5,0,1) 
conv.pred <- ytest
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
two_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
two_sl_cm

# one model- ranger 
set.seed(323)
single_sl <- SuperLearner(y, 
                          x, 
                          family=binomial(), 
                          SL.library=list("SL.ranger")) 
single_sl_pred <- predict.SuperLearner(single_sl, newdata=xtest) 
conv.preds <- ifelse(single_sl_pred$pred>=0.5,0,1) 
conv.pred <- ytest
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
single_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
single_sl_cm

# tuned
SL.tuning.ranger <- function(...){
  SL.ranger(..., num.trees=100, mtry=16)}
set.seed(323)
tuned_sl <- SuperLearner(y, 
                         x, 
                         family=binomial(), 
                         SL.library=list("SL.tuning.ranger",
                                         "SL.xgboost")) 
tuned_sl <- predict.SuperLearner(tuned_sl, newdata=xtest)
conv.preds <- ifelse(tuned_sl$pred>=0.5,0,1) 
conv.pred <- ytest
conv.preds <- as.factor(conv.preds) 
conv.pred <- as.factor(conv.pred) 
tuned_sl_cm <- confusionMatrix(conv.pred, conv.preds) 
tuned_sl_cm

# Model performance
Accuracy  <- c(72.7, 54.67, 64.55, 68.22, 68.59, 64.01, 62.59, 76.12, 59.47, 71.66)
Sensitivity <- c(17.8, 58.97, 40.86, 38.34, 26.24, 43.06, 46.62, 30.01, 55.29, 19.62)
Specificity <- c(87.09, 53.53, 70.78, 76.04, 79.68, 69.5, 66.77, 80, 60.56, 85.3)
models <- c('Random Forest', 'Bagging', 'Boosting', 'C-Forest', 'KNN', 'Neural Network', 
            'SVM', 'Superlearner', 'Naïve Bayes', 'C5.0')
combined <- cbind(Accuracy, Sensitivity, Specificity)
combined <- as.data.frame(combined)
combined <- cbind(combined, models)
plot_acc <- ggplot(data = combined, aes(x = reorder(models, Accuracy), y = Accuracy)) +
  geom_bar(stat="identity", fill = "steelblue") + xlab("Model") + coord_flip()
plot_sens <- ggplot(data = combined, aes(x = reorder(models, Sensitivity), y = Sensitivity)) +
  geom_bar(stat="identity", fill = "purple") + xlab("Model") + coord_flip()
plot_spec <- ggplot(data = combined, aes(x = reorder(models, Specificity), y = Specificity)) +
  geom_bar(stat="identity", fill = "red") + xlab("Model") + coord_flip()
plot_grid(plot_acc, plot_spec, plot_sens,
          nrow = 2, ncol = 2, labels = "AUTO", label_size = 18, align = "v")

# Time Series Forecasting
# For Windows
setwd("D:\\Niall Mannion")
# For Mac
setwd("/Users/Niall Mannion")

install.packages("plyr")
library(plyr)
library(reshape2)
# Preparing data for flexdashboard datatable
headers = read.csv('timeseries.csv', skip = 1, header = F, nrows = 1, as.is = T)
ts = read.csv('timeseries.csv', skip = 2, header = F)
colnames(ts)= headers
tsdf <- ts[ ,c(1,2,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,112,119,126,132,139,146,152)]
tsdf <- melt(tsdf, id.vars=c("Names"))
tsdf <- spread(tsdf, Names, value)
colnames(tsdf)
newsat <- tsdf[ ,c(1, 4, 5, 6, 7, 18, 21, 22, 23, 24, 29, 32, 34, 35, 39, 43, 44, 45, 46, 
                   51, 54, 55, 56, 62, 63, 64, 65, 70, 71, 72, 73, 74, 75, 76, 81,
                   82, 90, 91, 92, 96, 108, 109, 110, 111, 112, 126, 127, 128, 134, 138, 139)]
timeseries <- newsat
colnames(timeseries)
timeseries <- t(timeseries)
timeseries <- as.data.frame(timeseries)
write.csv(timeseries, "datatable_timeseries.csv")

# For Windows
setwd("D:\\Niall Mannion\\Documents")
# For Mac
setwd("/Users/Niall Mannion/Documents")
write.csv(trains, file= 'training_data')
write.csv(trains, file= 'test_data')
trains <- read.csv("training_data.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)
test <- read.csv("test_data.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)

timeseries <- read.csv("timeseries.csv", header=T, skip = 1, na.strings=c(""), stringsAsFactors = T)
tsdf <- timeseries[ ,c(1,2,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,112,119,126,132,139,146,152)]
news <- melt(tsdf, id.vars=c("Names"))
newsa <- spread(news, Names, value)
newsat <- newsa[ ,c(7, 91, 112)]
newsat

mymts = ts(newsat, frequency = 1, start = c(1995, 1))
plot(mymts, main = "Timeseries of Vaccination Rates")

# Outliers
newsat <- newsa[ ,c(112)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Texas Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Texas Vaccination Time Series Outlier Treatment", xlab = "Year")
Texas <- Treated

newsat <- newsa[ ,c(7)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Arkansas Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Arkansas Vaccination Time Series Outlier Treatment", xlab = "Year")
Arkansas <- Treated

newsat <- newsa[ ,c(91)] 
untreated = ts(newsat, frequency = 1, start = c(1995, 1))
outliers_excess_ts <- tso(untreated, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_excess_ts
plot(outliers_excess_ts)
title(main = "Outlier Plot of Oklahoma Timeseries", cex.main = 1.5, font.main = 1, line = 2.25)
title(ylab = "Vaccination Rate", line = 3, cex.lab = 1.2)
title(xlab = "Year", line = 4, cex.lab = 1.2)

outliers_excess_ts$outliers
(outliers_idx <- outliers_excess_ts$outliers$ind)
n <- length(untreated)
mo_tc <- outliers("TC", outliers_idx)
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_excess_ts$outliers["coefhat"])
tc_effect <- coefhat*tc
Outlier_Plot <- ts(tc_effect, frequency = frequency(untreated), start = start(untreated))
Treated <- untreated - Outlier_Plot
plot(cbind(Treated, untreated, Outlier_Plot),
     main = "Plot of Oklahoma Vaccination Time Series Outlier Treatment", xlab = "Year")
Oklahoma <- Treated

adjusted_ts <- cbind(Oklahoma, Texas, Arkansas)
apply(adjusted_ts, 2, adfTest, lags = 0, type = "c", 
      title = "ADF Testfor Vaccination Timeseries Data")

ts_window <- window(adjusted_ts, start = 1995, end = c(2012))
texas_ts <- adjusted_ts[,2]
stnry = diffM(ts_window)
var.a <- vars::VAR(stnry, lag.max = 10, ic = "AIC", type = "none") 
fcast = predict(var.a, n.ahead = 5)
Texas_fcast = fcast$fcst[2]
x = Texas_fcast$Texas[,1]
tail(ts_window)
x = cumsum(x) + 78.46

meanf_ts <- meanf(texas_ts, h = 5)
rwf_ts <- rwf(texas_ts, h = 5)
snaive_ts <- snaive(texas_ts, h = 5)

target_ts <- window(texas_ts, start = 2013)
target_ts <- as.numeric(target_ts)

forecast_ML <- x
accuracy(meanf_ts, target_ts)
accuracy(rwf_ts, target_ts)
accuracy(snaive_ts, target_ts)
accuracy(forecast_ML, target_ts)
meanf_ts
rwf_ts
snaive_ts
forecast_ML

stnry <- diffM(adjusted_ts)
autoplot(ts(stnry, start = c(1990, 1), frequency = 1), lwd =  1.6) +
  ggtitle("Time Series Plot of the stationary Texas Vaccination Time-Series") +
  ylab("Vaccination Rate") + xlab("Year")

var.a <- vars::VAR(stnry, lag.max = 10, ic = "AIC", type = "none")
fcast = predict(var.a, n.ahead = 5)
par(mar = c(2.5, 2.5, 2.5, 2.5))
plot(fcast)

Texas_fcast = fcast$fcst[2]
x = Texas_fcast$Texas[,1]
tail(adjusted_ts)
x = cumsum(x) + 74.1
par(mar = c(4, 4, 1, 4))
Forecast_Tex = ts(c(x), start= c(2017, 1), frequency = 1)
plot(Forecast_Tex, main = "Forecasted Vaccination Rates for Texas", xlab = "Year", ylab = "Vaccination Rates")
Texas_Forecast = ts(c(adjusted_ts[,2], x), start = c(1995, 1), frequency = 1)
Texas_df <- as.data.frame(Texas_Forecast[1:28])
colnames(Texas_df) <- c("Texas")

Arkansas_fcast = fcast$fcst[3] 
Arkansas_fcast
y = Arkansas_fcast$Arkansas[,1]
tail(adjusted_ts)
y = cumsum(y) + 73.89
par(mar = c(2.5,2.5,1,2.5))
Forecast_Ark = ts(c(y), start= c(2017, 1), frequency = 1)
plot(Forecast_Ark, main = "Forecasted Vaccination Rates for Arkansas", xlab = "Year", ylab = "Vaccination Rates")
Arkansas_Forecast =ts(c(adjusted_ts[,3], y), start = c(1995,1), frequency = 1)
Arkansas_df <- as.data.frame(Arkansas_Forecast[1:28]) 
colnames(Arkansas_df) <- c("y")

Oklahoma_fcast = fcast$fcst[1] 
Oklahoma_fcast
z = Oklahoma_fcast$Oklahoma[,1]
tail(adjusted_ts)
z = cumsum(z) + 71.4
par(mar = c(2.5,2.5,1,2.5))
Forecast_Okl = ts(c(z), start= c(2017, 1), frequency = 1)
plot(Forecast_Okl, main = "Forecasted Vaccination Rates for Oklahoma", xlab = "Year", ylab = "Vaccination Rates")
Oklahoma_Forecast =ts(c(adjusted_ts[,1], z), start = c(1995,1), frequency = 1)
Oklahoma_df <- as.data.frame(Oklahoma_Forecast[1:28]) 
colnames(Oklahoma_df) <- c("z")

combined_ts <- cbind(Texas_Forecast, Arkansas_Forecast, Oklahoma_Forecast)
combined_ts
combined_df <- as.data.frame(combined_ts)
combined_df$Year <- seq(1995, 2022, by = 1)
reshaped_ts <- melt(combined_df, id="Year")
forecast_theme <- theme(axis.title.x = element_text(size = 15),
                  axis.text.x = element_text(size = 15),
                  axis.title.y = element_text(size= 15),
                  plot.title = element_text(size = 17, hjust = 0.5))
ggplot(data = reshaped_ts, aes(x = Year, y = value, colour = variable)) + ylab("Immunization Rate") +
  ggtitle("Forecasted Immunization Rates for US States") + geom_line(size = 2.1) + forecast_theme
