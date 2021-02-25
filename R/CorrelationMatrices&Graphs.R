#Correlation matrix of 1000 random points from Terrain Analysis

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/Data/RandomPoints")

#Load dataset
BM <- read.delim("Belgica_Zone29N_Random_Pnts_1000.csv", sep=",", header=T)
HB <- read.delim("Hatton_Zone27N_Random_Pnts_1000.csv", sep=",", header=T)
RB <- read.delim("Rockall_Zone28N_Random_Pnts_1000.csv", sep=",", header=T)

#Rename (copy) covariables
BM$X <- BM$XCoord
BM$Y <- BM$YCoord
BM$Depth <- BM$BELGICA_29N_50M_MASK
BM$Aspect <- BM$BELGICA_ZONE29N_50M_MASK_ASPECT
BM$BPI_Broad <- BM$BELGICA_ZONE29N_50M_MASK_BPI_BROAD
BM$BPI_Fine <- BM$BELGICA_ZONE29N_50M_MASK_BPI_FINE
BM$Plan_Curve <- BM$BELGICA_ZONE29N_50M_MASK_PLAN_CURVATURE
BM$Prof_Curve <- BM$BELGICA_ZONE29N_50M_MASK_PROFILE_CURVATURE
BM$Roughness <- BM$BELGICA_ZONE29N_50M_MASK_ROUGHNESS
BM$Rugosity <- BM$BELGICA_ZONE29N_50M_MASK_RUGOSITY
BM$Slope <- BM$BELGICA_ZONE29N_50M_MASK_SLOPE_DEG
BM$Tang_Curve <- BM$BELGICA_ZONE29N_50M_MASK_TANGENTIAL_CURVATURE
BM$TPI <- BM$BELGICA_ZONE29N_50M_MASK_TPI
BM$TRI <- BM$BELGICA_ZONE29N_50M_MASK_TRI
BM$Northings <- BM$BELGICA_ZONE29N_50M_MASK_NORTHINGS 
BM$Eastings <- BM$ROCKALL_ZONE28N_50M_EASTINGS

HB$X <- HB$XCoord
HB$Y <- HB$YCoord
HB$Depth <- HB$HATTON_ZONE27N_50M_MASK
HB$Aspect <- HB$HATTON_ZONE27N_50M_MASK_ASPECT
HB$BPI_Broad <- HB$HATTON_ZONE27N_50M_MASK_BPI_BROAD
HB$BPI_Fine <- HB$HATTON_ZONE27N_50M_MASK_BPI_FINE
HB$Plan_Curve <- HB$HATTON_ZONE27N_50M_PLAN_CURVE
HB$Prof_Curve <- HB$HATTON_ZONE27N_50M_PROFILE_CURVE
HB$Roughness <- HB$HATTON_ZONE27N_50M_MASK_ROUGHNESS
HB$Rugosity <- HB$HATTON_ZONE27N_50M_MASK_RUGOSITY
HB$Slope <- HB$HATTON_ZONE27N_50M_MASK_SLOPE_DEG
HB$Tang_Curve <- HB$HATTON_ZONE27N_50M_TANGENTIAL_CURVE
HB$TPI <- HB$HATTON_ZONE27N_50M_MASK_TPI
HB$TRI <- HB$HATTON_ZONE27N_50M_MASK_TRI
HB$Northings <- HB$HATTON_ZONE27N_50M_MASK_NORTHINGS 
HB$Eastings <- HB$HATTON_ZONE27N_50M_MASK_EASTINGS

RB$X <- RB$XCoord
RB$Y <- RB$YCoord
RB$Depth <- RB$ROCKALL_ZONE28N_50M
RB$Aspect <- RB$ROCKALL_ZONE28N_50M_ASPECT
RB$BPI_Broad <- RB$ROCKALL_ZONE28N_50M_BPI_BROAD
RB$BPI_Fine <- RB$ROCKALL_ZONE28N_50M_BPI_FINE
RB$Plan_Curve <- RB$ROCKALL_ZONE28N_50M_PLAN
RB$Prof_Curve <- RB$ROCKALL_ZONE28N_50M_PROFILE_CURVATURE
RB$Roughness <- RB$ROCKALL_ZONE28N_50M_ROUGHNESS
RB$Rugosity <- RB$ROCKALL_ZONE28N_50M_RUGOSITY
RB$Slope <- RB$ROCKALL_ZONE28N_50M_SLOPE_DEG
RB$Tang_Curve <- RB$ROCKALL_ZONE28N_50M_TANGENTIAL_CURVATURE
RB$TPI <- RB$ROCKALL_ZONE28N_50M_TPI
RB$TRI <- RB$ROCKALL_ZONE28N_50M_TRI
RB$Northings <- RB$ROCKALL_ZONE28N_50M_NORTHINGS 
RB$Eastings <- RB$ROCKALL_ZONE28N_50M_EASTINGS

#################################################

#Collinearity of covariates

#Look at the variance inflation factors (VIFs) of the variables to assess the extent of any remaining collinearity
HB_Var<-cbind(HB$Depth, HB$Rugosity, HB$Roughness, HB$Slope, HB$BPI_Fine, 
              HB$BPI_Broad, HB$Tang_Curve, HB$Prof_Curve, HB$Plan_Curve, 
              HB$Aspect, HB$Northings, HB$Eastings, HB$TPI, HB$TRI)

RB_Var<-cbind(RB$Depth, RB$Rugosity, RB$Roughness, RB$Slope, RB$BPI_Fine, 
              RB$BPI_Broad, RB$Tang_Curve, RB$Prof_Curve, RB$Plan_Curve, 
              RB$Aspect, RB$Northings, RB$Eastings, RB$TPI, RB$TRI)

BM_Var<-cbind(BM$Depth, BM$Rugosity, BM$Roughness, BM$Slope, BM$BPI_Fine, 
              BM$BPI_Broad, BM$Tang_Curve, BM$Prof_Curve, BM$Plan_Curve, 
              BM$Aspect, BM$Northings, BM$Eastings, BM$TPI, BM$TRI)

HB_Var2<-data.frame(Depth = c(HB$Depth), Rugosity = c(HB$Rugosity), BPI_Broad = c(HB$BPI_Broad), 
                    Tang_Curve = c(HB$Tang_Curve), Northings = c(HB$Northings), Eastings = c(HB$Eastings))

RB_Var2<-data.frame(Depth = c(RB$Depth), Rugosity = c(RB$Rugosity), BPI_Broad = c(RB$BPI_Broad), 
                    Tang_Curve = c(RB$Tang_Curve), Northings = c(RB$Northings), Eastings = c(RB$Eastings))
#RB_Var2<-cbind(RB$Depth, RB$Rugosity, RB$BPI_Broad, RB$Tang_Curve, RB$Prof_Curve, RB$Aspect, RB$Northings, RB$Eastings)
BM_Var2<-data.frame(Depth = c(BM$Depth), Rugosity = c(BM$Rugosity), BPI_Broad = c(BM$BPI_Broad), 
                    Tang_Curve = c(BM$Tang_Curve), Northings = c(BM$Northings), Eastings = c(BM$Eastings))
#BM_Var<-cbind(BM$Depth, BM$Rugosity, BM$BPI_Broad, BM$Tang_Curve, BM$Prof_Curve, BM$Northings, BM$Eastings)

library(AED)
corvif(HB_Var)
corvif(RB_Var)
corvif(BM_Var)

corvif(HB_Var2)
corvif(RB_Var2)
corvif(BM_Var2)

source(file="HighstatLib.R")

#Pairplots (multi-panel scatterplot)
pairs(BM_Var2, upper.panel = panel.smooth, lower.panel = panel.cor)

pairs(HB_Var2, upper.panel = panel.smooth, lower.panel = panel.cor)
#BPI_Broad & Tang_Curve correlate with 0.7 (Pearson correlation)

pairs(RB_Var2, upper.panel = panel.smooth, lower.panel = panel.cor)

#Pairplots only captures 2-way relationships, whereas VIFs detect high-dimensional collinearity. 
#However, use pair plots to check for non-linear collinearity.


#################################################

#Pearson Correlation
Pearson_CorMat_Belgica <- cor(BM[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
                                     "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
                              y = NULL, use = "pairwise.complete.obs", method = c("pearson"))

Pearson_CorMat_Hatton <- cor(HB[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
                                   "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
                             y = NULL, use = "pairwise.complete.obs", method = c("pearson"))

Pearson_CorMat_Rockall <- cor(RB[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
                                    "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
                              y = NULL, use = "pairwise.complete.obs", method = c("pearson"))

Pearson_CorMat_AllRegions <- cor(AR[,c("Depth", "Eastings", "Northings", "Rugosity",
                                       "BPI_Fine", "Tang_Curve", "TPI")],
                                 y = NULL, use = "pairwise.complete.obs", method = c("pearson"))

#Save correlation matrices to file
write.csv(Pearson_CorMat_Belgica, file = "Belgica_PearsonCorrelation_RandomPoints.csv")
write.csv(Pearson_CorMat_Hatton, file = "Hatton_PearsonCorrelation_RandomPoints.csv")
write.csv(Pearson_CorMat_Rockall, file = "Rockall_PearsonCorrelation_RandomPoints.csv")
write.csv(Pearson_CorMat_AllRegions, file = "AllRegions_PearsonCorrelation_RandomPoints.csv")

#################################################

#Corrgram
library(corrgram)
corrgram(RB[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
               "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
         order = FALSE , lower.panel=panel.pts, upper.panel=panel.pie, text.panel=panel.txt)

corrgram(HB[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
               "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
         order = FALSE , lower.panel=panel.pts, upper.panel=panel.pie, text.panel=panel.txt)

corrgram(BM[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
               "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
         order = FALSE , lower.panel=panel.pts, upper.panel=panel.pie, text.panel=panel.txt)

corrgram(AR[,c("Depth", "Aspect", "Eastings", "Northings", "Roughness", "Rugosity", "Slope",
               "BPI_Broad", "BPI_Fine", "Plan_Curve", "Prof_Curve", "Tang_Curve", "TPI", "TRI")], 
         order = FALSE , lower.panel=panel.pts, upper.panel=panel.pie, text.panel=panel.txt)

#################################################
