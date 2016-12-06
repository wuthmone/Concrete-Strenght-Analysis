## Try to test MLP with concrete data

#reading the data
concrete <- read.csv("concrete.csv", header = T)
str(concrete)

#Normalize data to 0 to 1

normalize <- function(x){
        return((x - min(x))/(max(x)-min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)

#Training a Model on the Data ( 75 % for train set, 25 % for test set)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

#install.packages("neuralnet")
library(neuralnet)
library(grid)
library(MASS)
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                            data = concrete_train)
plot(concrete_model)
library(NeuralNetTools)
# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(concrete_model, alpha = 0.6)

#Evaluating Model Performance   
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

#Improving Model Performance
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                             data = concrete_train, hidden = 3)
#alternative plot using plotnet
par(mar = numeric(4), family = 'serif')
plotnet(concrete_model2, alpha = 0.6)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)