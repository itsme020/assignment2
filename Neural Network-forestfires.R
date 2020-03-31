library(caret)
.libPaths()
library(randomForest)
model<-randomForest(iris$Species~.,data=iris,ntree=1000)

#view the Forest results.
print(model)
#importance of the variable-Lower gini

print (importance(model))
# Extract a single tree from a forest
#split var whch variable was used to split the nodes
#0 if the node is terminal
#split point where the best split is; see Deatails for categorical predictor
#status is the node terminal(-1) or not (1)
#prediction the prediction for the node; 0 if the node is not terminal
#getTree(model)
#Prediction
pred<-predict(model,iris[,-5])

table(pred,iris$Species)
