#Bathymetry map of Belgica Mound

#Set directory
getwd()
setwd("C:/Users/Matthias/Desktop")

#Read data
VideoData <- read.delim("VS_Data_Matt.csv", sep=",", header=T)
#Wrong data file has no coordinates!

#Packages for Interpolation
library(spam)
library(fields)
library(akima)

#Delete rows with missing data
VideoData_omited <- na.omit(VideoData)

#Data interpolation
FishAbundance_interp <- interp(VideoData_omited$Longitude,VideoData_omited$Latitude,VideoData_omited$AllSpeciesFA, duplicate="mean")
FishBiomass_interp <- interp(VideoData_omited$Longitude,VideoData_omited$Latitude,VideoData_omited$AllSpeciesFB, duplicate="mean")
CFW_interp <- interp(VideoData_omited$Longitude,VideoData_omited$Latitude,VideoData_omited$X.CFW, duplicate="mean")
Depth_interp <- interp(VideoData_omited$Longitude, VideoData_omited$Latitude,VideoData_omited$Depth..m., duplicate="mean")

#Produce plot
image.plot(FishAbundance_interp)
image.plot(FishBiomass_interp)
image.plot(CFW_interp)
image.plot(Depth_interp)

image  (FishAbundance_interp, add=TRUE)
contour(FishAbundance_interp, add=TRUE)
points (bath_BM_interp, pch = 3)
