library(shiny)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)
library(caret)

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

dataQNB <- read.csv("QueenstownNotBoard.csv", stringsAsFactors = FALSE)

train$Pclass <- factor(train$Pclass); test$Pclass <- factor(test$Pclass); dataQNB$Pclass <- factor(dataQNB$Pclass)
train$Sex <- factor(train$Sex); test$Sex <- factor(test$Sex); dataQNB$Sex <- factor(dataQNB$Sex)
train$Embarked <- factor(train$Embarked); test$Embarked <- factor(test$Embarked); dataQNB$Embarked <- factor(dataQNB$Embarked)
train$FamilySize <- train$SibSp + train$Parch + 1; test$FamilySize <- test$SibSp + test$Parch + 1

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize, data = train, method = "class")

prediction_survival <- predict(fit, dataQNB, type = "class")
levels(prediction_survival) <- c("Die", "Survive")

prediction_probs <- predict(fit, dataQNB)

prediction_probabilities <- c()

for (i in c(1:dim(prediction_probs)[1])){
        if(prediction_probs[i, 1] > prediction_probs[i, 2]){
                prediction_probabilities <- append(prediction_probabilities, prediction_probs[i, 1])
        }else{
                prediction_probabilities <- append(prediction_probabilities, prediction_probs[i, 2])
        }      
}

prediction_table <- data.frame(Names = dataQNB[,2], Outcome = prediction_survival, Probability = prediction_probabilities)

predictionTest <- predict(fit, test, type = "class")
CfMatrix <- confusionMatrix(predictionTest, test$Survived)

shinyServer(function(input, output){
        
        output$plot <- renderPlot(fancyRpartPlot(fit))
        
        output$dataQNB <- renderDataTable({dataQNB}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
        
        output$Prediction <- renderDataTable({prediction_table}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
        
        output$Accuracy <- renderPrint(CfMatrix)
})