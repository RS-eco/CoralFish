#Histogram code from Johnson

rm(list=ls(all=TRUE)) #removes any previously created lists#
graphics.off()   #this turns off any previously drawn graphics#

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Data")

#Import data
CoralFISH <- read.delim("AllRegions_VS_Data.csv", sep=",",header=T)
BM <- read.delim("Belgica_Zone29N_VS_Data.csv", sep=",",header=T)
HB <- read.delim("Hatton_Zone27N_VS_Data.csv", sep=",",header=T)
RB <- read.delim("Rockall_Zone28N_VS_Data.csv", sep=",",header=T)

library(Hmisc)
library(RColorBrewer)

#Telling R to use "fish"#
attach(BM)            #attachs a certain database to R search path, after this command any following requests will be carried out searching through "fish"
names(BM)             #gets the names of the objects currently loaded. this will show all of the column headings of the data table of "fish"#

windows(width=12,height=12)       #window refers to the graphical output window
par(xpd=T, mar=par()$mar+c(0,0,0,4)) # gives space for legend

barplot (t(figure1[,-c(1,5)]), main="", xlab="Year of publication", ylab="Number of peer-reviewed journal articles", col=c("black","gray","white"),space=0.2,cex.axis=0.8, las=2,names.arg=Year, cex=0.75)
axis(2, at=0, labels="", pos=0, lty=1, col=1, las=2, tck=0.95)                                                                                                           

#################################################

#####    creating points to plot regression line on top of barplot)

A<- c(4.978007,0.00313113)
mA1<-matrix(A,nrow=1,ncol=2)
B<- c(36.11981, 8.422712)
mB1<-matrix(B,nrow=1,ncol=2)
ABmatrix<-rbind(mA1, mB1)
colnames(ABmatrix)<- c("X","Y")
row.names(ABmatrix)<- c("A","B")
ABmatrix

ABpoints<-as.data.frame(ABmatrix)

attach(ABpoints)
points(Y~X, type="l")

#####

#minor.tick(nx=(2), ny=1, tick.ratio=0.7)                                                                                                              

legend(36,8,inset=0.02,c("All", "Adult", "Juvenile"), cex=0.8, fill=c("black","gray","white")) #*** how to put this outside the graphic

new.year=Year-min(Year)+1    #changes years to 1-30 - so that the line is not plotted way of chart over x=1981 onwards

regressionfig1<-(lm(Total~new.year, data=figure1))

#abline(regressionfig1)

summary (regressionfig1)

fig1r2<-round(summary(regressionfig1)$r.squared,3) 
fig1r2

fig1p<-round(summary(regressionfig1)$coefficients[8],6)
fig1p

legendletfig1=bquote(r^2 == .(fig1r2)~","~p==. (fig1p))
legend("topleft",legend=legendletfig1,bty="n",text.col="black",cex=0.9,adj=-0.1)
