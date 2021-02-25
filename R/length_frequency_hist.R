#Histogram

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Data")

#Import data
CoralFISH <- read.delim("AllRegions_VS_Data.csv", sep=",",header=T)
BM <- read.delim("Belgica_Zone29N_VS_Data.csv", sep=",",header=T)
HB <- read.delim("Hatton_Zone27N_VS_Data.csv", sep=",",header=T)
RB <- read.delim("Rockall_Zone28N_VS_Data.csv", sep=",",header=T)

par(mfrow = c(3,2), mar = c(4,4,3,2))
#hist(CoralFISH$BROSMELENG, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Brosme brosme"))))
hist(CoralFISH$CHIMLENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Chimaera monstrosa"))))
hist(CoralFISH$HELILENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
#hist(CoralFISH$HOPLOLENGT, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Hoplostethus atlanticus"))))
hist(CoralFISH$LEPLENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))
#hist(CoralFISH$LOPHILENGT, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lophius piscatorius"))))
#hist(CoralFISH$MOLVALENGT, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Molva dypterygia"))))
hist(CoralFISH$MORALENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))
#hist(CoralFISH$PHYCISLENG, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Phycis phycis"))))
#hist(CoralFISH$PSEUDOLENG, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Pseudotriakis microdon"))))
#hist(CoralFISH$RAJALENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Raja fyllae"))))
hist(CoralFISH$SIGLENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Sigmops bathyphilus"))))
hist(CoralFISH$SYNLENGTH, breaks = 50, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Synaphobranchus kaupii"))))

par(mfrow = c(2,2), mar = c(4,4,3,3))
hist(BM$HELILENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
hist(BM$LEPLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))
hist(BM$MORALENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))
hist(BM$SYNLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Synaphobranchus kaupii"))))

hist(HB$LEPLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))

par(mfrow = c(2,1))
hist(RB$HELILENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
hist(RB$LEPLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))

par(mfrow = c(1,3))
hist(BM$LEPLENGTH, breaks = 25, ylab = "Frequency of L. eques", xlab="Total length (cm)", main="Belgica Mound", ylim = c(0,80), xlim = c(0,60))
hist(HB$LEPLENGTH, breaks = 25, ylab = "Frequency of L. eques", xlab="Total length (cm)", main="Hatton Bank", ylim = c(0,80), xlim = c(0,60))
hist(RB$LEPLENGTH, breaks = 25, ylab = "Frequency of L. eques", xlab="Total length (cm)", main="Rockall Bank", ylim = c(0,80), xlim = c(0,60))

#Histogram with Lattice
library(lattice)
histogram( ~ CoralFISH$LEPLENGTH | CoralFISH$CFW_Bin, type = "count",
           xlab = "Total length (cm)",
           ylab = "Frequency",
           nint=30,
           )

## generate some random data
coralLengths <- BM$LEPLENGTH[BM$CFW_Bin == 1]
allLengths <- BM$LEPLENGTH
## calculate the histograms - don't plot yet
histcoral <- hist(coralLengths,plot = FALSE)
histall <- hist(allLengths,plot = FALSE)
## calculate the range of the graph
xlim <- range(histcoral$breaks,histall$breaks)
ylim <- range(0,200)
## plot the first graph
plot(histall,xlim = xlim, ylim = ylim,
     xaxt = 'n', yaxt = 'n', ## don't add axes
     col = rgb(1,0,1), add = TRUE)
## plot the second graph on top of this
plot(histcoral,xlim = xlim, ylim = ylim,
     col = rgb(0,0,0),xlab = 'Lengths',
     main = 'Length-frequency of Lepidion eques')
## add a legend in the corner
legend('topleft',c('Coral','Non-coral'),
       fill = rgb(0:1,0,0:1,1), bty = 'n',
       border = NA)
barplot(rbind(histall, histcoral))

#Stacked histogram
install.packages("ggplot2")
library(ggplot2)
qplot(carat, data = diamonds, binwidth = 0.1)

qplot(LEPLENGTH, data = BM, binwidth = 0.1, aes(x=CFW_Bin, fill=cut))
hist_coral

qplot(rating, data=movies, geom="histogram") 
