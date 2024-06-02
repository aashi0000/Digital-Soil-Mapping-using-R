library(rpart)
library(ithir)
library(raster)
library(fBasics)
library(nortest)
library(ggplot2)
library(gstat)
library(randomForest)

#random forest
set.seed(123)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))

edge.rf.exp<- randomForest(log_cStock0_5~elevation+twi+radK+landsat_b3+landsat_b4,data=DSM_data,importance=TRUE,ntree=1000)
edge.rf.exp
varImpPlot(edge.rf.exp)

rf.pred.c<- predict(edge.rf.exp,DSM_data[training,])
goof(predicted=rf.pred.c,observed=DSM_data$log_cStock0_5[training])

rf.pred.v<- predict(edge.rf.exp,DSM_data[-training,])
goof(predicted=rf.pred.v,observed=DSM_data$log_cStock0_5[-training])

map.rf.r1<- predict(covStack,edge.rf.exp)
plot(map.rf.r1,main="random forest model predictions log soc 0-5cm")