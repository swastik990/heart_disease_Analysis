#Loading the data set heart for analysis
hdata = read.csv('C:/Users/user/Desktop/IIMS College/6th Semester IIMS College/Data Mining/Assignment4_heartDA/heart.csv')

#overview of the dataset
head(hdata)
tail(hdata)

#number of rows and columns of the dataset
print(paste('Number of rows: ',nrow(hdata)))
print(paste('Number of columns: ',ncol(hdata)))

#looking for null values in the dataset
null_values = sum(is.na(hdata))
null_values

#summary of the dataset
summary(hdata)
str(hdata)

##setting some values as factors
hdata$sex = factor(hdata$sex)
hdata$scp = factor(hdata$cp)
hdata$fbs = factor(hdata$fbs)
hdata$restecg = factor(hdata$restecg)
hdata$exang = factor(hdata$exang)
hdata$slope = factor(hdata$slope)
hdata$ca = factor(hdata$ca)
hdata$thal = factor(hdata$thal)

#checking data types again
str(hdata)


#Box plot for age across heart disease
boxplot(age ~ target, data = hdata, main = "Age Distribution with Heart Disease", xlab = "Hear Disease", ylab = "Age")


#Bar chart for sex
barplot(table(hdata$sex), main = "Distribution of Gender", xlab = "Gender", ylab = "Count")


#bar plot using ggplot for heart disease
library(ggplot2)
ggplot(hdata, aes(x = factor(target), fill = factor(target))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Heart Disease", x = "Heart Disease", y = "Count") +
  theme_minimal()


#Bar chart of heart disease against chest pain
barplot(table(hdata$cp, hdata$target), 
        beside = TRUE, 
        legend = TRUE,
        col = c("lightblue", "lightyellow", "pink","lightgreen"),
        main = "Distribution of Chest Pain and Heart disease",
        xlab = "Chest Pain",
        ylab = "Count")


#Creating scatter plot of heart disease across age and max heart rate
library(ggplot2)
ggplot(hdata, aes(x = age, y = thalach, color = factor(target))) +
  geom_point() +
  labs(title = "Heart Disease across Age and Max Heart Rate",
       x = "Age",
       y = "Max Heart Rate",
       color = "Heart Disease") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()


library(corrplot)
#calculating correlation matrix
corr_matrix = cor(hdata[, c("age", "trestbps", "chol", "thalach", "oldpeak", "target")])

#plotting correlation matrix
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black")



##Training Testing and Splitting##
library(caTools)
set.seed(123)
split = sample.split(hdata$target, SplitRatio = 0.80)
training_set = subset(hdata, split ==TRUE)
test_set = subset(hdata, split ==FALSE)


#Decision Tree Modeling##
library(rpart)
set.seed(123)
decision_tree = rpart(target ~ ., data= training_set, method="class")

#Plotting Decision Tree)
library(rpart.plot)
rpart.plot(decision_tree, main = "Decision Tree for Heart Disease Predection")
decision_tree_pred = predict(decision_tree, newdata = test_set, type = "class")

#Calculating accuracy
accuracy = sum(decision_tree_pred == test_set$target) / length(test_set$target) * 100
print(paste("Accuracy of Decision Tree Model:", round(accuracy, 2), "%"))

##Confusion Matrix for Decision Tree Model Evaluation
conf_matrix = confusionMatrix(decision_tree_pred, reference = factor(test_set$target))
conf_matrix



##Logistic Regression Model##
library(glmnet)

#Training the logistic regression model
set.seed(200)
logistic_regression = glm(target ~ ., data = training_set, family = "binomial")

#Making predictions using the logistic regression model
logistic_regression_pred = predict(logistic_regression, newdata = test_set, type = "response")

#Converting predicted probabilities to binary predictions
logistic_regression_pred_binary = ifelse(logistic_regression_pred > 0.5, 1, 0)

#Calculating accuracy
accuracy_logistic_regression = sum(logistic_regression_pred_binary == test_set$target) / nrow(test_set) * 100
print(paste("Accuracy of Logistic Regression Model:", round(accuracy_logistic_regression, 2), "%"))

logistic_regression_pred_binary = as.factor(logistic_regression_pred_binary)

#Confusion Matrix for Logistic regression
cm = confusionMatrix(logistic_regression_pred_binary, reference = factor(test_set$target))
cm



#Visuaization of logistic regression usign ggplot##
#Fitting logistic regression model
logreg = glm(target ~ ., data = training_set, family = binomial)

#Prediction on the test set
test_pred_probs = predict(logreg, newdata = test_set, type = "response")

#Creating a data frame for visualization
visualization_data = data.frame(Actual = test_set$target, Predicted_Prob = test_pred_probs)

#Plotting the predicted probabilities against actual outcomes
library(ggplot2)
ggplot(visualization_data, aes(x = Predicted_Prob, y = Actual)) +
  geom_point(aes(color = factor(Actual)), alpha = 0.7) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(title = "Logistic Regression Results Visualization",
       x = "Predicted Probability",
       y = "Actual Outcome") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()


##Random Forest Classifier##
set.seed(70)
library(randomForest)
library(caret)

#Training random forest
random_Forest = randomForest(factor(target) ~ ., data = training_set, mtry = 1)
print(random_Forest)

#Making predictions
predictions = predict(random_Forest, test_set)

plot(random_Forest)


#confusion matrix for performance evaluation of Random forest
conf_matrix = confusionMatrix(predictions, reference = factor(test_set$target))
conf_matrix


#comparsion of models##

#Creating data frame for model accuracies
accuracy_data <- data.frame(
  Model = c("Decision Tree", "Logistic Regression", "Random Forest"),
  Accuracy = c(accuracy, accuracy_logistic_regression, conf_matrix$overall['Accuracy'] * 100))


#Plotting the comparison with percentages
library(ggplot2)
ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste(round(Accuracy, 2), "%")), vjust = -0.5, size = 3.5, position = position_dodge(width = 0.9)) +
  labs(title = "Comparison of Model Accuracies",
       x = "Model",
       y = "Accuracy (%)") +
  theme_minimal()

