

library(plyr)
Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
Startups <- as.data.frame(Startups)

#normalize the data
normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
 }
#lapply
Startups_norm<-as.data.frame(lapply(Startups,normalize))
#concrete_norm<-scale(concrete)

#creating training and test data
Startups_train<-Startups_norm[1:35,]
Startups_test<-Startups_norm[36:50,]


#training the model on the data
#train the neuralnet model
library(neuralnet)

#simple ANN with  only single hidden neuron

Startups_model<-neuralnet(formula = Profit~RDSpend+Administration+MarketingSpend+State,data=Startups_train)

#Visualize the network topology
windows()
plot(Startups_model)

##Evaluating model performance-----
#obtain model results
model_results<-compute(Startups_model, Startups_test[1:4])

#obtain predicted profit value
predicted_Profit<-model_results$net.result

#examine the correlation between predict and actual value
cor(predicted_Profit,Startups_test$Profit)

#improving model performance
#a more complex neural network topology with 6 hidden neuron
Startups_model2<-neuralnet(formula = Profit~RDSpend+Administration+MarketingSpend+State,data=Startups_train,hidden=c(6,3))
#plot the network window
plot(Startups_model2)


#evaluate the result
Startups_model2<-compute(Startups_model2, Startups_test[1:4])
predicted_Profit2<-Startups_model2$net.result
cor(predicted_Profit2,Startups_test$Profit)

