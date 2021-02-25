VS_FA_Multivariate <- read.delim("~/VS_FA_Multivariate.txt")
View(VS_FA_Multivariate)

library(ecodist)
library(vegan)
library(permute)
library(BiodiversityR)
library(ellipse)
library(MASS)

#Species accumulative curve
spp.curve <- specaccum(comm = com, method = "random", permutations = 1000)
plot(spp.curve)

#Set working directory
#setwd("C:/Users/Matthias/Documents/Analysis_R")

#Load fish abundance data
data_FA <- read.delim("VideoData_FA_MDS.csv", sep=",",header=T)

#Load fish abundance data
data_FB <- read.delim("VideoData_FB_MDS.csv", sep=",",header=T)

#Remove first column of data
data_FA_new<-data_FA[,2:38]

# Default MDS ordination
data.FA.mds <- metaMDS(data_FA_new)

#View items in the list produced by metaMDS.
names(data.FA.mds)

#View the results of the MDS
data.mds

#Extract sample and variable scores.
variableScores <- data.FA.mds$species
sampleScores <- data.FA.mds$points

#Plot sample and variable scores in same space. 
#Open black circles correspond to samples and red crosses indicate taxa.
plot(data.FA.mds)

#MDS plots can be customized by selecting either sites or species. 
#Also, labels may be displayed instead of symbols by specifying type="t".
plot(data.FA.mds, type="t", display=c("species"))






