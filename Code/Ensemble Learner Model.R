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