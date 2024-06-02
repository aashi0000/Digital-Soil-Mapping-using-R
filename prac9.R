library(rpart)
library(ithir)
library(MASS)
library(raster)
library(fBasics)
library(nortest)
library(ggplot2)
library(gstat)
library(randomForest)
library(caret)
library(Cubist)
library(sp)
library(ipred)
library(nnet)

con.mat <- matrix(c(5, 0, 1, 2, 0, 15, 0, 5, 0, 1, 31, 0, 0, 10, 2, 11), nrow = 4, ncol = 4)
rownames(con.mat)<- c("DE", "VE", "CH", "KU")
colnames(con.mat)<-c("DE", "VE", "CH", "KU")
con.mat
rowSums(con.mat)
colSums(con.mat)
ceiling(sum(diag(con.mat))/sum(colSums(con.mat))*100)
ceiling((diag(con.mat))/(colSums(con.mat))*100)
ceiling(diag(con.mat)/rowSums(con.mat) * 100)
goofcat(conf.mat=con.mat, imp = TRUE)



data(hunterCovariates)
data(terron.dat)

