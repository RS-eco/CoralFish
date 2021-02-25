#Corrgram
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/GLM")
CoralFISH <- read.delim("CoralFISH_Model.csv", sep=",", header=T)
attach(CoralFISH)

#These are all the species
AllS <- c("LepNo", "SynNo", "SigNo", "MoraNo", "ChimNo", "HeliNo", "LophiNo", "MolvaNo", "BrosmeNo", "HoploNo", "PhycisNo", "PseudoNo", "RajaNo")

#Determine species richness
Richness <- colSums(CoralFISH[,AllS] > 0, na.rm = TRUE)

#Remove all covariates
FISH  <- CoralFISH[,AllS]

#To reduce the of variables in the figure, we only used the
N <- ncol(FISH)


AllNames <- names(FISH)
A <- matrix(nrow = N, ncol = N)

for (i in 1:N){
  for (j in 1:N){
    A[i,j] <- sum(CoralFISH[,AllS[i]]==0  & CoralFISH[,AllS[j]]==0, na.rm=TRUE)
  }}


A1 <- A/2167
print(A1, digits = 2)
rownames(A1) <- AllNames
colnames(A1) <- AllNames


library(lattice)

panel.corrgram.2 <- function(x, y, z, subscripts, at = pretty(z), scale = 0.8, ...)
{
  require("grid", quietly = TRUE)
  x <- as.numeric(x)[subscripts]
  y <- as.numeric(y)[subscripts]
  z <- as.numeric(z)[subscripts]
  zcol <- level.colors(z, at = at, ...)
  for (i in seq(along = z))
  {
    lims <- range(0, z[i])
    tval <- 2 * base::pi *
      seq(from = lims[1], to = lims[2], by = 0.01)
    grid.polygon(x = x[i] + .5 * scale * c(0, sin(tval)),
                 y = y[i] + .5 * scale * c(0, cos(tval)),
                 default.units = "native",
                 gp = gpar(fill = zcol[i]))
    grid.circle(x = x[i], y = y[i], r = .5 * scale,
                default.units = "native")
  }
}




levelplot(A1,xlab=NULL,ylab=NULL,
          at=do.breaks(c(0.5,1.01),101),
          panel=panel.corrgram.2,
          scales=list(x=list(rot=90)),
          colorkey=list(space="top"),
          col.regions=colorRampPalette(c("red","white","blue")))


#Grey colours
levelplot(A1,xlab=NULL,ylab=NULL,
          at=do.breaks(c(0.5,1.01),101),
          panel=panel.corrgram.2,
          scales=list(x=list(rot=90)),
          colorkey=list(space="top"),
          col.regions=colorRampPalette(c(grey(0.8),grey(0.5),grey(0.2))))