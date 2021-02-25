library("lattice")

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Data Exploration")

#Import data
data <- read.delim("CoralFISH_Model+Terrain.csv", sep=",",header=T)

# Write to a file, suppress row names
#write.csv(data2, "VideoData_FA_FB.csv", row.names=FALSE)

#Produce a pairs plot with histogramm, correlation coefficients, and smoothers, see ?pairs for function
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#Pairs plot _ Terrain Analysis
pairs(data[,c(58,61,66,67,68)], lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, 
      main="Bivariate plots with Histograms, Smooth Curves & Correlation coefficients")

#Pairs plot - Video Variable
pairs(data[,c(1,3,7,8,12,15)], lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, 
      main="Bivariate plots with Histograms, Smooth Curves & Correlation coefficients")

#Pairs plot - Fish count & mixed variables
pairs(data[,c(55,12,15,58,61,66)], lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, 
      main="Bivariate plots with Histograms, Smooth Curves & Correlation coefficients")

#Check for Outliers
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(data$AllNo,  ylab = "No of Fish per Min")
dotchart(data$AllNo, xlab = "No of Fish per Min", ylab = "Order of the data")

#Multipanel Cleveland plot
library(lattice)
Z <- cbind(data$AllNo, data$Depth_m,  data$X.CFW, data$Aspect,  data$Depth, data$Roughness)
colnames(Z) <- c("No of Fish", "Depth", "Coral Cover", "Aspect", "Depth_MB", "Roughness")

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

###############################################################
#Check for homogeneity of variance, MUlti-panel conditional boxplots
#Missing second variable!!!

library(lattice)

data$RegionInt <- as.integer(data$Region)
data$fRegion <- factor(data$RegionInt, levels = c(0, 1, 2), labels = c("Belgica Mound", "Hatton Bank", "Rockall Bank"))

bwplot(AllNo ~ | fRegion, data = data,
       strip = strip.custom(bg = 'white'),   subset = Region!=0,
       cex = .5, layout = c(3, 1),
       xlab = "Habitat", ylab = "No of Fish per Min",
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

##############################################################
#Check for normal distribution, histogram

data$RegionInt <- as.integer(data$Region)
data$fRegion <- factor(data$RegionInt, levels = c(1, 2, 3), labels = c("Belgica Mound", "Hatton Bank", "Rockall Bank"))

data$I1 <- data$fRegion =="Belgica Mound" |
  data$fRegion =="Hatton Bank" |
  data$fRegion =="Rockall Bank"

hist(data$LepLength[data$I1],
     xlab = "Length (cm)", breaks = 30,
     main = "", ylab = "Frequency")

library(lattice)
histogram( ~ LepLength | fRegion, type = "count",
           xlab = "Length (cm)",
           ylab = "Frequency",
           nint=30,layout=c(1,3),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = fRegion =="Belgica Mound" | fRegion == "Hatton Bank" |fRegion == "Rockall Bank",
           data = data)


####################################################################
#Figure 9
Z <- as.vector(as.matrix(data[, c("Avgmaxht", "Avgdens",
                                       "ht.thatch", "S.patens", "Distichlis",
                                       "S.alternifloraShort", "S.alternifloraTall", "Juncus",
                                       "Bare", "Other", "Phragmites", "Shrub", "Tallsedge", "Water")]))


#Setup the data in vector format for the xyplot
Y10 <- rep(data$AllNo, ??)

MyNames <- names(data[,c("Avgmaxht", "Avgdens", "ht.thatch",
                              "S.patens", "Distichlis", "S.alternifloraShort",
                              "S.alternifloraTall", "Juncus", "Bare", "Other",
                              "Phragmites", "Shrub", "Tallsedge", "Water")])

ID10 <- rep(MyNames, each = length(data$AllNo))
library(lattice)


ID11 <- factor(ID10, labels = c("% Juncus gerardii",
                                "% Shrub", "Height of thatch", "% Spartina patens",
                                "% Distichlis", "% Bare ground", "% Other vegetation",
                                "% Phragmites australis", "% Tall sedge", "% Water",
                                "% Spartina alterniflora (short)",
                                "% Spartina alterniflora (tall)",
                                "Maximum vegetation height",
                                "Vegetation stem density"),
               levels = c("Juncus", "Shrub", "Avgmaxht", "S.patens",
                          "Distichlis", "Bare", "Other", "Phragmites",
                          "Tallsedge", "Water", "S.alternifloraShort",
                          "S.alternifloraTall", "ht.thatch", "Avgdens"))


xyplot(Y10 ~ Z | ID11, col = 1,
       strip = function(bg='white',...) strip.default(bg='white',...),
       scales = list(alternating = T,
                     x = list(relation = "free"),
                     y = list(relation = "same")),
       xlab = "Covariates",
       par.strip.text = list(cex = 0.8),
       ylab = "Banded",
       panel=function(x, y, subscripts,...){
         panel.grid(h =- 1, v = 2)
         panel.points(x, y, col = 1, pch = 16)
         if(ID10[subscripts][1] != "Tallsedge") {panel.loess(x,y,col=1,lwd=2)}
       })


##################################################################
#Figure 10

Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
source(file = "HighstatLib.R")
MyNames <- c("wing chord", "tarsus length", "head length",
             "culmen length", "nalospi to bill tip", "weightt")
pairs(Sparrows[,c(1, 3, 4, 5, 6, 7)],
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=MyNames)


###################################################################
#Consider interaction, Coplo

#Define Region as categorical variable
data$RegionInt <- as.integer(data$Region)
data$fRegion <- factor(data$RegionInt, levels = c(1, 2, 3), labels = c("Belgica Mound", "Hatton Bank", "Rockall Bank"))

#Run linear model
M1 <- lm(data$AllNo ~ data$Depth*data$X.CFW*data$fRegion)
summary(M1)
anova(M1)

#Make the coplot
coplot(data$AllNo ~ data$Depth | data$X.CFW * data$fRegion, ylab = "Count",
       xlab = "Depth",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


##################################################################
#Figure 12
Waders <- read.table(file = "wader.txt", header = TRUE)

#Define the time axis
Time <- seq(1,25)

par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
plot(Time, Waders$C.fuscicolis, type = "l", xlab = "Time (2 weeks)",
     ylab = "C. fuscicollis abundance")
acf(Waders$C.fuscicolis, main = "C. fuscicollis ACF")

plot(Time, Waders$L.dominicanus, type = "l", xlab = "Time (2 weeks)",
     ylab = "L. dominicanus abundance")
acf(Waders$L.dominicanus, main = "L. dominicanus ACF")

#################################################################

#Produce coplot, design & interaction plot
library(lattice)
coplot(AllSpeciesFA ??? X.CFW | Depth..m., data = data2)

coplot(Richness~NAP | temperature, data = RIKZ,
       panel = panel.smooth)

xyplot(AllSpeciesFA~X.CFW | factor(Depth..m.), data = data2, col = 1)

library(Design)
plot.design(AllSpeciesFA ~ Station + X.CFW + Depth..m., data = data2)

interaction.plot(data2$X.CFW, data2$Depth..m., data2$AllSpeciesFA, xlab="CFW", ylab="")

#Produce bubble maps

#Subset data for Belgica Mound
subbelgica <- subset(data2, Area == "Belgica Mound")

#Subset data for Hatton Bank
subhatton <- subset(data2, Area == "Hatton Bank")

#Subset data for Rockall Bank
subrockall <- subset(data2, Area == "Rockall Bank")

par(mfrow=c(3,1))

plot(subbelgica$Latitude~subbelgica$Longitude, asp=1, type="p", cex=5*subbelgica$AllSpeciesFA/max(subbelgica$AllSpeciesFA), main="Total FA - Belgica Mound", xlab="Longitude", ylab="Latitude")
plot(subbelgica$Latitude~subbelgica$Longitude, asp=1, type="p", cex=5*subbelgica$AllSpeciesFB/max(subbelgica$AllSpeciesFB), main="Total FB - Belgica Mound", xlab="Longitude", ylab="Latitude")
plot(subbelgica$Latitude~subbelgica$Longitude, asp=1, type="p", cex=5*subbelgica$X.CFW/max(subbelgica$X.CFW), main="% CFW - Belgica Mound", xlab="Longitude", ylab="Latitude")

plot(subhatton$Latitude~subhatton$Longitude, asp=1, type="p", cex=5*subhatton$AllSpeciesFA/max(subhatton$AllSpeciesFA), main="Total FA - Hatton Bank", xlab="Longitude", ylab="Latitude")
plot(subhatton$Latitude~subhatton$Longitude, asp=1, type="p", cex=5*subhatton$AllSpeciesFB/max(subhatton$AllSpeciesFB), main="Total FB - Hatton Bank", xlab="Longitude", ylab="Latitude")
plot(subhatton$Latitude~subhatton$Longitude, asp=1, type="p", cex=5*subhatton$X.CFW/max(subhatton$X.CFW), main="% CFW - Hatton Bank", xlab="Longitude", ylab="Latitude")

plot(subrockall$Latitude~subrockall$Longitude, asp=1, type="p", cex=5*subrockall$AllSpeciesFA/max(subrockall$AllSpeciesFA), main="Total FA - Rockall Bank", xlab="Longitude", ylab="Latitude")
plot(subrockall$Latitude~subrockall$Longitude, asp=1, type="p", cex=5*subrockall$AllSpeciesFB/max(subrockall$AllSpeciesFB), main="Total FB - Rockall Bank", xlab="Longitude", ylab="Latitude")
plot(subrockall$Latitude~subrockall$Longitude, asp=1, type="p", cex=5*subrockall$X.CFW/max(subrockall$X.CFW), main="% CFW - Rockall Bank", xlab="Longitude", ylab="Latitude")

#Transform abundances to presence-absence (1-0)
library("vegan")
data_ab_pa<-decostand(data_ab, method="pa")

#Identifying species associations
spe <- data2[,c(12,14,16,17,19,21,23,25,27,29,31,33,34)]

# Kendell's W coefficient of concordance

#Extraction of the most abundant species
sp.sum <- apply(spe, 2, sum)
spe.sorted <- spe[,order(sp.sum, decreasing=TRUE)]
spe.small <- spe.sorted[,1:10]

#Transformation of species data and transposition
spe.small.hel <- decostand(spe.small, "hellinger")
spe.small.std <- decostand(spe.small.hel, "standardize")
spe.small.t <- t(spe.small.std)

#k-means partitioning of species
spe.t.kmeans.casc <- cascadeKM(spe.small.t, inf.gr=2, sup.gr=8, iter=100, criterion="calinski")
plot(spe.t.kmeans.casc, sortg=TRUE)

#The partition into 2 groups is found in column 1 of the object $partition
clusters <- spe.t.kmeans.casc$partition[,1]
clusters

#Run a global Kendell W test on these groups
spe.kendall.global <- kendall.global(spe.small.hel, clusters)
spe.kendall.global

#Run a posteriori test to identify the significantly concordant species
spe.kendall.post <- kendall.post(spe.small.hel, clusters, nperm=9999)
spe.kendall.post