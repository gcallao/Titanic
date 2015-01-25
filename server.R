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


fit.default <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize, data = train, method = "class")

fit.low <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize, data = train, 
                     method = "class", control=rpart.control(minsplit=200)) 

fit.high  <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + FamilySize, data = train, 
                        method = "class", control=rpart.control(cp=0))          
  
prediction_survival.default <- predict(fit.default, dataQNB, type = "class")
prediction_survival.low <- predict(fit.low, dataQNB, type = "class")
prediction_survival.high <- predict(fit.high, dataQNB, type = "class")

levels(prediction_survival.default) <- c("Die", "Survive")
levels(prediction_survival.low) <- c("Die", "Survive")
levels(prediction_survival.high) <- c("Die", "Survive")

prediction_probs.default <- predict(fit.default, dataQNB)
prediction_probs.low <- predict(fit.low, dataQNB)
prediction_probs.high <- predict(fit.high, dataQNB)

prediction_probabilities.default <- c()
prediction_probabilities.low <- c()
prediction_probabilities.high <- c()

for (i in c(1:dim(prediction_probs.default)[1])){
        if(prediction_probs.default[i, 1] > prediction_probs.default[i, 2]){
                prediction_probabilities.default <- append(prediction_probabilities.default, prediction_probs.default[i, 1])
        }else{
                prediction_probabilities.default <- append(prediction_probabilities.default, prediction_probs.default[i, 2])
        }      
}

for (i in c(1:dim(prediction_probs.low)[1])){
        if(prediction_probs.low[i, 1] > prediction_probs.low[i, 2]){
                prediction_probabilities.low <- append(prediction_probabilities.low, prediction_probs.low[i, 1])
        }else{
                prediction_probabilities.low <- append(prediction_probabilities.low, prediction_probs.low[i, 2])
        }      
}

for (i in c(1:dim(prediction_probs.high)[1])){
        if(prediction_probs.high[i, 1] > prediction_probs.high[i, 2]){
                prediction_probabilities.high <- append(prediction_probabilities.high, prediction_probs.high[i, 1])
        }else{
                prediction_probabilities.high <- append(prediction_probabilities.high, prediction_probs.high[i, 2])
        }      
}

prediction_table.default <- data.frame(Names = dataQNB[,2], Outcome = prediction_survival.default, Probability = prediction_probabilities.default)
prediction_table.low <- data.frame(Names = dataQNB[,2], Outcome = prediction_survival.low, Probability = prediction_probabilities.low)
prediction_table.high <- data.frame(Names = dataQNB[,2], Outcome = prediction_survival.high, Probability = prediction_probabilities.high)

predictionTest.default <- predict(fit.default, test, type = "class")
predictionTest.low <- predict(fit.low, test, type = "class")
predictionTest.high <- predict(fit.high, test, type = "class")

CfMatrix.default <- confusionMatrix(predictionTest.default, test$Survived)
CfMatrix.low <- confusionMatrix(predictionTest.low, test$Survived)
CfMatrix.high <- confusionMatrix(predictionTest.high, test$Survived)

shinyServer(function(input, output){

        output$plot <- renderPlot(if(input$select == "Default"){
                        fancyRpartPlot(fit.default)
                        }else{
                                if(input$select == "Low"){
                                    fancyRpartPlot(fit.low)  
                                }else{
                                        fancyRpartPlot(fit.high)  
                                }
                        }
                        )
        
        output$dataQNB <- renderDataTable({dataQNB}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
        
        output$Prediction <- renderDataTable({if(input$select == "Default"){
                                prediction_table.default
                                }else{
                                        if(input$select == "Low"){
                                                prediction_table.low    
                                        } else {
                                                prediction_table.high       
                                        }
                                }
        
        }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
        
        output$Accuracy <- renderPrint(if(input$select == "Default"){
                           CfMatrix.default
                        }else{
                                if(input$select == "Low"){
                                        CfMatrix.low  
                                }else{
                                        CfMatrix.high  
                                }
                        }
                )
})