normalize the data
normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
#lapply
concrete_norm<-as.data.frame(lapply(concrete,normalize))
#concrete_norm<-scale(concrete)

#creating training and test data
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

#training the model on the data
#train the neuralnet model
library(neuralnet)

#simple ANN with  only single hidden neuron

concrete_model<-neuralnet(formula = strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=concrete_train)

#Visualize the network topology
windows()
plot(concrete_model)

##Evaluating model performance-----
#obtain model results
model_results<-compute(concrete_model, concrete_test[1:8])

#obtain predicted strength value
predicted_strength<-model_results$net.result

#examine the correlation between predict and actual value
cor(predicted_strength,concrete_test$strength)

#improving model performance
#a more complex neural network topology with 5 hidden neuron
concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=concrete_train,hidden=c(5,2))
#plot the network window
plot(concrete_model2)


#evaluate the result
model_results2<-compute(concrete_model2, concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)
