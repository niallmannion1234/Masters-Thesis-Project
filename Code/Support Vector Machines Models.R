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