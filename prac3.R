library(ithir)
library(raster)
library(fBasics)
library(nortest)
library(ggplot2)
library(gstat)
data(oneProfile)
str(oneProfile)
plot_soilProfile(data = oneProfile, vals = oneProfile$C.kg.m3., depths = oneProfile[,2:3], label= names(oneProfile)[4])
ea_fit<- ea_spline(oneProfile,var.name = "C.kg.m3.",d=t(c(0,5,15,30,60,100,200)),lam=0.1,vlow=0,show.progress = FALSE)
str(ea_fit)
par(mfrow = c(3, 1))
for (i in 1:3) {
  plot_ea_spline(splineOuts = ea_fit, d = t(c(0, 5, 15, 30, 60,100, 200)), maxd = 200, type = i, plot.which = 1,label = "carbon density")
}

par(mfrow=c(1,1));
data(edgeroi_splineCarbon);
data(edgeroiCovariates);
# elevation;
plot(elevation,main="edgeroi el map w points plotted");
coordinates(edgeroi_splineCarbon) <- ~ east + north;
plot(edgeroi_splineCarbon,add=T);
covstack<- stack(elevation,twi,radK,landsat_b3,landsat_b4);
dsm_data<- extract(covstack,edgeroi_splineCarbon,sp=1,method="simple");
newdf<- as.data.frame(dsm_data);
write.table(newdf,"C:\\Users\\aashi\\OneDrive\\Desktop\\project\\prac\\edgeroi.txt",col.names = TRUE,row.names = FALSE,sep=',')
edge.dat<- read.table("C:\\Users\\aashi\\OneDrive\\Desktop\\project\\prac\\edgeroi.txt",header = TRUE,sep=',')
str(edge.dat);
sampleSKEW(edge.dat$X0.5.cm);
sampleKURT(edge.dat$X0.5.cm)
ad.test(edge.dat$X0.5.cm)
par(mfrow = c(1, 2))
hist(edge.dat$X0.5.cm)
qqnorm(edge.dat$X0.5.cm, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(edge.dat$X0.5.cm, col = "red", lwd = 2)
par(mfrow = c(1, 2))
hist(log(edge.dat$X0.5.cm))
qqnorm(log(edge.dat$X0.5.cm), plot.it = TRUE, pch = 4, cex = 0.7)
qqline(log(edge.dat$X0.5.cm), col = "red", lwd = 2)
library(ggplot2)
ggplot(edge.dat, aes(x = east, y = north)) +
  geom_point(aes(size = edge.dat$X0.5.cm))
tempD <- data.frame(cellNos = seq(1:ncell(elevation)))
tempD$vals <- getValues(elevation)
tempD <- tempD[complete.cases(tempD), ]
cellNos <- c(tempD$cellNos)
gXY <- data.frame(xyFromCell(elevation, cellNos, spatial = FALSE))
names(edge.dat)[2:3] <- c("x", "y")
IDW.pred <- idw(log(edge.dat$X0.5.cm) ~ 1, locations = ~x + y, data = edge.dat, newdata = gXY, idp = 2)
IDW.raster.p <- rasterFromXYZ(as.data.frame(IDW.pred[, 1:3]))
plot(IDW.raster.p)
vgm1 <- variogram(log(X0.5.cm) ~ 1, ~x + y, edge.dat, width = 400,cressie = TRUE, cutoff = 10000)
mod <- vgm(psill = var(log(edge.dat$X0.5.cm)),"Exp", range = 5000, nugget = 0)
model_1 <- fit.variogram(vgm1, mod)
model_1
plot(vgm1, model = model_1)


krig.pred <- krige(log(edge.dat$X0.5.cm) ~ 1, locations = ~x + y,data = edge.dat, newdata = gXY, model = model_1)
str(krig.pred)
par(mfrow = c(2, 1))
krig.raster.p <- rasterFromXYZ(as.data.frame(krig.pred[, 1:3]))
krig.raster.var <- rasterFromXYZ(as.data.frame(krig.pred[, c(1:2, 4)]))
plot(krig.raster.p, main = "ordinary kriging predictions")
plot(krig.raster.var, main = "ordinary kriging variance")