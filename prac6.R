library(rpart)
library(ithir)
library(raster)
data(edgeroi_splineCarbon)
names(edgeroi_splineCarbon)[2:3]<- c("x","y")
edgeroi_splineCarbon$log_cStock0_5 <- log(edgeroi_splineCarbon$X0.5.cm)
data(edgeroiCovariates)
coordinates(edgeroi_splineCarbon)<- ~x+y
covStack <- stack(elevation, twi, radK, landsat_b3, landsat_b4)
DSM_data<- extract(covStack,edgeroi_splineCarbon,sp=1,method="simple")
DSM_data<- as.data.frame(DSM_data)

#decision trees
set.seed(123)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))
edge.rt.exp<- rpart(log_cStock0_5~elevation+twi+radK+landsat_b3+landsat_b4,data=DSM_data[training,],control=rpart.control(minsplit = 50))
summary(edge.rt.exp)
printcp(edge.rt.exp)
plot(edge.rt.exp)
text(edge.rt.exp)

rt.pred.c<- predict(edge.rt.exp,DSM_data[training,])
goof(observed=DSM_data$log_cStock0_5[training],predicted = rt.pred.c)

rt.pred.v<- predict(edge.rt.exp,DSM_data[-training,])
goof(observed=DSM_data$log_cStock0_5[-training],predicted = rt.pred.v)

map.rt.r1<- predict(covStack,edge.rt.exp)
plot(map.rt.r1,main="decision tree plot soc 0-5cm (validation set) ")


#cubist
library(Cubist)
set.seed(123)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))
mdat<- DSM_data[training,]

edge.cub.exp <- cubist(x = mdat[, c("elevation", "twi", "radK", "landsat_b3","landsat_b4")], y = mdat$log_cStock0_5,cubistControl(rules = 5, extrapolation = 5),committees = 1)
summary(edge.cub.exp)

cubist.pred.c<- predict(edge.cub.exp,DSM_data[training,])
goof(observed=DSM_data$log_cStock0_5[training],predicted=cubist.pred.c)

cubist.pred.v<- predict(edge.cub.exp,DSM_data[-training,])
goof(observed=DSM_data$log_cStock0_5[-training],predicted=cubist.pred.v)

map.cubist.r1<- predict(covStack,edge.cub.exp)
plot(map.cubist.r1,main="cubist model predictions")
