library(ggplot2)
library(grid)
library(plyr)
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data <- read.delim("VideoData_FA_FB.csv", sep=",",header=TRUE)

#Summary per Species divided into coral/no coral
summaryLepidionFA <- summarySE(data, measurevar="LepidionFA", groupvars="CoralNoCoral")
summarySynaphobranchusFA <- summarySE(data, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")
summarySigmopsFA <- summarySE(data, measurevar="SigmopsFA", groupvars="CoralNoCoral")
summaryMoraFA <- summarySE(data, measurevar="MoraFA", groupvars="CoralNoCoral")
summaryChimaeraFA <- summarySE(data, measurevar="ChimaeraFA", groupvars="CoralNoCoral")
summaryHelicolenusFA <- summarySE(data, measurevar="HelicolenusFA", groupvars="CoralNoCoral")
summaryLophiusFA <- summarySE(data, measurevar="LophiusFA", groupvars="CoralNoCoral")
summaryMolvaFA <- summarySE(data, measurevar="MolvaFA", groupvars="CoralNoCoral")
summaryBrosmeFA <- summarySE(data, measurevar="BrosmeFA", groupvars="CoralNoCoral")
summaryHoplostethusFA <- summarySE(data, measurevar="HoplostethusFA", groupvars="CoralNoCoral")
summaryPhycisFA <- summarySE(data, measurevar="PhycisFA", groupvars="CoralNoCoral")
summaryPseudotriakisFA <- summarySE(data, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")
summaryRajaFA <- summarySE(data, measurevar="RajaFA", groupvars="CoralNoCoral")

# Summary per Species undivided
summaryLepidionFA <- summarySE(data, measurevar="LepidionFA")
summarySynaphobranchusFA <- summarySE(data, measurevar="SynaphobranchusFA")
summarySigmopsFA <- summarySE(data, measurevar="SigmopsFA")
summaryMoraFA <- summarySE(data, measurevar="MoraFA")
summaryChimaeraFA <- summarySE(data, measurevar="ChimaeraFA")
summaryHelicolenusFA <- summarySE(data, measurevar="HelicolenusFA")
summaryLophiusFA <- summarySE(data, measurevar="LophiusFA")
summaryMolvaFA <- summarySE(data, measurevar="MolvaFA")
summaryBrosmeFA <- summarySE(data, measurevar="BrosmeFA")
summaryHoplostethusFA <- summarySE(data, measurevar="HoplostethusFA")
summaryPhycisFA <- summarySE(data, measurevar="PhycisFA")
summaryPseudotriakisFA <- summarySE(data, measurevar="PseudotriakisFA")
summaryRajaFA <- summarySE(data, measurevar="RajaFA")

#Run a principal component analysis

#data <- read.delim("VS_FA_PCA.csv", sep=",",header=TRUE)

# Read data
data <- read.delim("VS_FA_Multivariate.csv", sep=",", header=TRUE)
# PCA code from R-Book
pcad<-data[,3:14]
model<-prcomp(pcad, center=TRUE)
summary(model)
plot(model,main="")
biplot(model)