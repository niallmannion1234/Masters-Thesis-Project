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