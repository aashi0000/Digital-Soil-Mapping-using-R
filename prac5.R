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
# str(DSM_data)
# head(DSM_data)
DSM_data <- DSM_data[, c(2:3, 11:16)]
#x,y,logwala,rasters se extracted wale
DSM_data<- DSM_data[complete.cases(DSM_data),]
edge.MLR.full<- lm(log_cStock0_5~elevation+twi+radK+landsat_b3+landsat_b4,data=DSM_data)
edge.MLR.full
edge.MLR.step<- step(edge.MLR.full,trace=0,direction="both")
edge.MLR.step
summary(edge.MLR.full)
tempd<- data.frame(cellnos=seq(1:ncell(covStack)))
vals<- as.data.frame(getValues(covStack))
tempd<- cbind(tempd,vals)
tempd<- tempd[complete.cases(tempd),]
cellnos<- c(tempd$cellnos)
gxy<- data.frame(xyFromCell(covStack,cellnos,spatial=FALSE))
tempd<- cbind(gxy,tempd)
# str(tempd)
# nrow(tempd)
set.seed(123)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))
edge.mlr.rh<- lm(log_cStock0_5~elevation+landsat_b3,data=DSM_data[training,])
map.mlr<- predict(edge.mlr.rh,newdata=tempd)
map.mlr<- cbind(data.frame(tempd[,c("x","y")]),map.mlr)
map.mlr.r<- rasterFromXYZ(as.data.frame(map.mlr[,1:3]))
plot(map.mlr.r,main="mlr predicted log soc stock for 0-5 cm")
crs(map.mlr.r) <- "+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
writeRaster(map.mlr.r,"cStock_0_5_MLR.tif",format = "GTiff",datatype="FLT4S",overwrite=TRUE)

par(mfrow=c(3,1))
map.mlr.r.low<- predict(covStack,edge.mlr.rh,index=2,interval="prediction",level=0.9)
plot(map.mlr.r.low)
map.mlr.r.pred<- predict(covStack,edge.mlr.rh,index=1,interval="prediction",level=0.9)
plot(map.mlr.r.pred)
map.mlr.r.up<- predict(covStack,edge.mlr.rh,index=3,interval="prediction",level=0.9)
plot(map.mlr.r.up)


# can be done normally
# par(mfrow = c(3, 1))
# predfun <- function(model, data) {
#   v <- predict(model, data, interval = "prediction", level = 0.9)
# }
# map.mlr.r.1ow <- predict(covStack, edge.mlr.rh, "cStock_0_5_MLR_low.tif",fun = predfun, index = 2, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
# plot(map.mlr.r.1ow, main = "MLR predicted log SOC stock (0-5cm) lower limit")
# map.mlr.r.pred <- predict(covStack, edge.mlr.rh, "cStock_0_5_MLR_pred.tif",fun = predfun, index = 1, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
# plot(map.mlr.r.pred, main = "MLR predicted log SOC stock (0-5cm)")
# map.mlr.r.up <- predict(covStack, edge.mlr.rh, "cStock_0_5_MLR_up.tif",fun = predfun, index = 3, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
# plot(map.mlr.r.up, main = "MLR predicted log SOC stock (0-5cm) upper limit")
