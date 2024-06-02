library(ithir)
library(MASS)
data(USYD_soil1)
soil.data <- USYD_soil1
mod.data <- na.omit(soil.data[, c("clay", "CEC")])
par(mfrow=c(2,1))
mod.1 <- lm(CEC ~ clay, data = mod.data, y = TRUE, x = TRUE)
mod.1
goof(observed=mod.data$CEC,predicted=mod.1$fitted.values,type="DSM",plot.it = TRUE)
set.seed(123)
training<- sample(nrow(mod.data),0.7*nrow(mod.data))
# training
mod.rh<- lm(CEC~clay,data=mod.data[training,],y=TRUE,x=TRUE)
mod.rh
goof(observed=mod.data$CEC[training],predicted=mod.rh$fitted.values,type="DSM")
mod.rh.v<- predict(mod.rh,mod.data[-training,])
goof(observed= mod.data$CEC[-training],predicted=mod.rh.v,plot.it = TRUE)
locvpred<- numeric(nrow(mod.data))
for(i in 1:nrow(mod.data)){
  locvmodel<-lm(CEC~clay,data=mod.data[-i,],x=TRUE,y=TRUE)
  locvpred[i]<- predict(locvmodel,newdata=mod.data[i,])
}
goof(observed=mod.data$CEC,predicted = locvpred,plot.it = TRUE)