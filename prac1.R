
library(ithir)
data(USYD_soil1)
soil.data <- USYD_soil1

str(soil.data)
head(soil.data)
soil.data<-edit(soil.data)
r=soil.data$Lower.Depth;
mean(r);
mean(na.omit(soil.data$ESP));
soil.data$Upper<- soil.data$Upper.Depth *100;
soil.data$Lower<- soil.data$Lower.Depth *100;
soil.data$Upper.Depth;
soil.data$Upper;
soil.data$Lower;
write.table(soil.data, file = "C:\\Users\\aashi\\OneDrive\\Desktop\\project\\prac\\myfile.txt", col.names = TRUE,row.names = FALSE, sep = "\t");
soil.data.edit<-soil.data[order( soil.data$ESP), ];
head(soil.data.edit);
match(0.0,soil.data$ESP);
soil1<-data.frame(soil=c("soil type 1","soil type 2"),gm=c(1,2));
soil2<-data.frame(soil=c("soil type 3","soil type 4"),gm=c(3,4));
rbind(soil1,soil2);
cbind(soil1,soil2);
soil.data.split <- split(soil.data, soil.data$PROFILE);
length(soil.data.split);
subset(soil.data,ESP>10);
soil.data[order(-soil.data$Upper.Depth), ];
summary(soil.data);
qqnorm(log(soil.data$Total_Carbon),plot.it = TRUE,pch=0,cex=0.2);
qqline(log(soil.data$Total_Carbon),col="red");
plot(soil.data$CEC,soil.data$ExchCa);
summary(lm(CEC~ExchCa,data=soil.data));
pairs(na.omit(soil.data),cex=0.05);
