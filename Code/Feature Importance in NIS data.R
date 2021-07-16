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
