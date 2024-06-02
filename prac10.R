library(ithir)
library(raster)
library(ipred)
library(nnet)
library(sp)
library(rasterVis)


data(hvTerronDat)
data(hunterCovariates)

names(hvTerronDat)
coordinates(hvTerronDat)<- ~x+y
covStack<- stack(hunterCovariates$AACN,hunterCovariates$Drainage.Index,hunterCovariates$Light.Insolation,hunterCovariates$TWI,hunterCovariates$Gamma.Total.Count)
DSM_data<- extract(covStack,hvTerronDat,sp=1,method="simple")
DSM_data<- as.data.frame(DSM_data)
str(DSM_data)

#----------------------------------------------#
set.seed(655)
training<- sample(nrow(DSM_data),0.7*nrow(DSM_data))
hv.MNLR<- multinom(terron~AACN+Drainage.Index+Light.Insolation+TWI+Gamma.Total.Count,data=DSM_data[training,])
# summary(hv.MNLR)
probs.hv.MNLR<- fitted(hv.MNLR)
# head(probs.hv.MNLR)
pred.hv.MNLR<- predict(hv.MNLR)
# summary(pred.hv.MNLR)
goofcat(observed=DSM_data$terron[training],predicted=pred.hv.MNLR)
V.pred.hv.MNLR<- predict(hv.MNLR,newdata=DSM_data[-training,])
goofcat(observed=DSM_data$terron[-training],predicted=V.pred.hv.MNLR)
map.MNLR.c<- predict(covStack,hv.MNLR,type="class",index=1)
map.MNLR.p<- predict(covStack,hv.MNLR,type="probs",index=1)


map.MNLR.c <- as.factor(map.MNLR.c)
rat <- levels(map.MNLR.c)[[1]]
rat[["terron"]] <- c("HVT_001", "HVT_002", "HVT_003", "HVT_004", "HVT_005", "HVT_006", "HVT_007", "HVT_008","HVT_009", "HVT_010", "HVT_011", "HVT_012")
levels(map.MNLR.c) <- rat
area_colors <- c("#FF0000", "#38A800", "#73DFFF", "#FFEBAF","#A8A800", "#0070FF", "#FFA77F", "#7AF5CA", "#D7B09E","#CCCCCC", "#B4D79E", "#FFFF00")
levelplot(map.MNLR.c, col.regions = area_colors, xlab = "", ylab = "")
