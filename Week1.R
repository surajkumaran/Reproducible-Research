library (kernlab)
data ("spam")

#Separating training and test datasets
set.seed(3435)
trainIndicator = rbinom(4601,size=1,prob=0.5)
table(trainIndicator)
trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]

#Exploratory analysis of dataset
names(trainSpam)
head(trainSpam)
table(trainSpam$type)

#Plots
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve +1) ~ trainSpam$type)

#Parallelized Relationships bw predictors
plot(log10(trainSpam[,1:4]+1))

#Hierarchial Clustering
hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

#After transformation
hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

#Logistic regression model
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y>0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam[i]), response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

#Which predictor has minimum cross validated error?
names(trainSpam[which.min(cvError)])

#Getting measure of uncertainty

#Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

#Get predictions on the test dataset
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

#Classify as spam for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

#Classification table
table(predictedSpam, testSpam$type)

#Error rate
(61+458)/(1346+61+458+449)
