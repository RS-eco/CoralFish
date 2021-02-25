#Run a principal component analysis

# Read data
data_FA <- read.delim("VideoData_FA_MDS.csv", sep=",",header=TRUE)

#Remove first column of data
data_FA_new<-data_FA[,2:38]

# PCA code from R-Book
model<-prcomp(data_FA_new, center=TRUE)
summary(model)
plot(model,main="")
biplot(model)

#Plot explanatory variables against PCA
yv<-predict(model)[,1]
yv2<-predict(model)[,2]
par(mfrow=c(2,2))
plot(pgdata$hay,yv,pch=16,xlab="biomass",ylab="PC 1")
plot(pgdata$pH,yv2,pch=16,xlab="soil pH",ylab="PC 2")