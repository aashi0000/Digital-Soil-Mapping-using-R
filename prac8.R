library(rpart)
library(ithir)
library(raster)
library(fBasics)
library(nortest)
library(ggplot2)
library(gstat)
library(randomForest)
library(caret)
library(Cubist)

fit<- train(form=log_cStock0_5~elevation+twi+radK+landsat_b3+landsat_b4,data=DSM_data,method="lm",trControl=trainControl(method="repeatedcv",number=5,repeats=10))
fit
pred_lm<- predict(fit,DSM_data)
pred_lm_r1<- predict(covStack,fit)
plot(pred_lm_r1)

#------------------------------------------------#
set.seed(123)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))
cdat<- DSM_data[training,]
coordinates(cdat)<- ~x+y
crs(cdat) <- "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84
+units=m +no_defs"
crs(covStack) = crs(cdat)
vgm1<- variogram(log_cStock0_5~elevation+twi+radK+landsat_b3+landsat_b4,data=cdat,width=250,cressie=TRUE,cutoff=10000)
mod<- vgm(psill=var(DSM_data$log_cStock0_5),"Exp",range=5000,nugget=0)
model1<- fit.variogram(vgm1,mod)
model1;
guk<- gstat(NULL,"log.carbon",log_cStock0_5~elevation+twi+radK+landsat_b3+landsat_b4,data=cdat,model=model1)

#---------------------------------------------------#
vdat<- DSM_data[-training,]
coordinates(vdat)<- ~x+y
crs(vdat) <- "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84
+units=m +no_defs"
UK.preds.V <- as.data.frame(krige(log_cStock0_5 ~ elevation + twi+ radK + landsat_b3 + landsat_b4, cdat, model = model1,newdata = vdat))
goof(observed=DSM_data$log_cStock0_5[-training],predicted=UK.preds.V[,3])

#----------------------------------------------------#
par(mfrow=c(1,2))
uk_p_map<- interpolate(covStack,guk,xyOnly=FALSE,index=1)
uk_pvar_map<- interpolate(covStack,guk,xyOnly=FALSE,index=2)
plot(uk_p_map,main="universal kriging predictions")
plot(uk_pvar_map,main="universal kriging variance predictions")

#----------------------------------------------------#
set.seed(123)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))
mdat<- DSM_data[training,]
edge.cub.exp<- cubist(x=mdat[,c("elevation","twi","radK","landsat_b3","landsat_b4")],y=mdat$log_cStock0_5,cubistControl(unbiased=TRUE,rules=100,extrapolation=5,sample=0,label="outcome"),committees=1)
mdat$residual<-mdat$log_cStock0_5 - predict(edge.cub.exp,newdata=mdat)
mean(mdat$residual)
coordinates(mdat) <- ~x + y
crs(mdat) <- "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84
+units=m +no_defs"
vgm1 <- variogram(residual ~ 1, mdat, width = 250, cressie = TRUE,cutoff = 10000)
mod <- vgm(psill = var(mdat$residual), "Sph", range = 5000, nugget = 0)
model_1 <- fit.variogram(vgm1,mod)
gRK <- gstat(NULL, "RKresidual", residual ~ 1, mdat,model = model_1)
Cubist.pred.V <- predict(edge.cub.exp, newdata = DSM_data[-training, ])
vdat <- DSM_data[-training, ]
coordinates(vdat) <- ~x + y
crs(vdat) <- "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84
+units=m +no_defs"
RK.preds.V <- as.data.frame(krige(residual ~ 1, mdat, model = model_1,newdata = vdat))
RK.preds.fin <- Cubist.pred.V + RK.preds.V[, 3]
goof(observed = DSM_data$log_cStock0_5[-training],predicted = Cubist.pred.V)
goof(observed = DSM_data$log_cStock0_5[-training],predicted = RK.preds.fin)

#-----------------------------------------------------#
par(mfrow=c(3,1))

map_rk1<- predict(covStack,edge.cub.exp)
plot(map_rk1,main="cubist preds")

map_rk2<- interpolate(covStack,gRK,index=1)
plot(map_rk2,main="kriged residual")

pred_rkstack<- stack(map_rk1,map_rk2)
map_rk3<- calc(pred_rkstack,fun=sum)
plot(map_rk3,main="cubist plus residuals")