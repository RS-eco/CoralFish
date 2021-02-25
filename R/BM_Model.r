#GLM on CoralFISH based on Chapter 21 of Mixed Effects Models and Extensions in Ecology with R (2009)

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Data")

#Import data
BM <- read.delim("Belgica_Zone29N_VS_Data.csv", sep=",",header=T)

#################################################

#Names of variables
#str(BM)

#table()

#Rename (copy) covariables
BM$AREA <- BM$SURVEYAREA_M2
BM$X <- BM$XCoord
BM$Y <- BM$YCoord
BM$Depth <- BM$BELGICA_ZONE29N_50M_MASK
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
BM$Eastings <- BM$BELGICA_ZONE29N_50M_EASTINGS

#Transform variables into Factor
BM$fStation <- factor(BM$STATION)

#################################################

library(lattice)

#Position of all survey sites
xyplot(X ~ Y, xlab="Longitude", ylab="Latitude", col=1, aspect="iso", pch=16, data=BM)

#Position of all survey sites per station
xyplot(X ~ Y | fStation, xlab="Longitude", ylab="Latitude", col=1, aspect="iso", pch=16, data=BM)

#Position of sites
xyplot(Y ~ X, aspect="iso", col = 1, pch=16, data=BM)

MyCex <- sqrt(BM$ALL_FA_HA / max(BM$ALL_FA_HA))
xyplot(Y ~ X, aspect="iso", pch=16, cex = MyCex, col = 1, data=BM)

#################################################

#Transform Non-integer to Integer values
BM$Depth <- as.integer(BM$Depth)
#BM$ALL_FA_HA <- as.integer(BM$ALL_FA_HA)
BM$LEP_FA_HA <- as.integer(BM$LEP_FA_HA)
BM$SYN_FA_HA <- as.integer(BM$SYN_FA_HA)
BM$MORA_FA_HA <- as.integer(BM$MORA_FA_HA)

#Removes the missing values (which are present in the response variable)
I=is.na(BM$fStation) | is.na(BM$Depth) | is.na(BM$ALLNO) | is.na(BM$TPI) | is.na(BM$Rugosity) | is.na(BM$Northings) | is.na(BM$Eastings) | is.na(BM$CFW)
BM <- BM[!I,]


#Frequency plot for all fish data
plot(table(BM$ALL_FA_HA), type="h", ylab="Frequency", xlab="Observed values")

plot(table(BM$LEP_FA_HA), type="h", ylab="Frequency", xlab="Observed values - L. eques")
plot(table(BM$SYN_FA_HA), type="h", ylab="Frequency", xlab="Observed values - S. kaupii")
plot(table(BM$MORA_FA_HA), type="h", ylab="Frequency", xlab="Observed values - M. mora")

#Percent of zero observations, length and number of non-zero observations
ZEROS <- 100 * sum(BM$ALL_FA_HA == 0) / length(BM$ALL_FA_HA)
LENGTH <- length(BM$ALL_FA_HA)
Z <- cbind(ZEROS, LENGTH, (100 - ZEROS)*LENGTH/100)
colnames(Z) <- c("%0", "Length", "#non-0")
rownames(Z) <- c("All Fish Species")
Z

#################################################

#Look at covariates to check for outliers, continuous vs. categorical

#library(lattice)

MyCovariates <- c("fStation", "fDepth", "X", "Y", "Aspect", "BPI_Broad", "BPI_Fine", 
                  "Plan_Curve", "Prof_Curve", "Roughness", "Rugosity", "Slope", 
                  "Tang_Curve", "TPI", "TRI", "Northings", "Eastings", "CFW")
BM.X <- BM[, MyCovariates]
dotplot(as.matrix(BM.X), groups=FALSE, col=1, cex=0.5, 
        strip=strip.custom(bg = 'white', 
                           par.strip.text = list(cex=1.0)), 
        scales=list(x=list(relation = "free"), 
                    y = list(relation = "free"), draw=FALSE), 
        xlab="Value of the variable", pch=16, 
        ylab = "Order of the data from text file")

dotplot(as.matrix(BM$CFW), col = 1, cex = 1, 
        strip=strip.custom(bg = 'white', 
              par.strip.text = list(cex=1.0)),
        scales=list(x=list(relation = "free"), 
                    y = list(relation = "free"), draw=FALSE), 
        xlab="Value of variable", pch = 16, ylab="Order of the data from text file")

#################################################

#Converts count data into absence/presence
#DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
#DeerEcervi$Ecervi.01[DeerEcervi$Ecervi >0 ] <- 1

#################################################

#Collinearity of covariates

Col_All<- BM[,c("fStation", "Depth",  "CFW", "X", "Y", 
                "BPI_Broad", "BPI_Fine", "TPI", "TRI",
                "Tang_Curve", "Plan_Curve", "Prof_Curve", 
                "Roughness", "Rugosity", "Slope", 
                "Aspect", "Northings", "Eastings")]

Col_Con<-BM[,c("Depth",  "CFW",
               "BPI_Broad", "BPI_Fine", "TPI", "TRI",
               "Tang_Curve", "Plan_Curve", "Prof_Curve", 
               "Roughness", "Rugosity", "Slope", 
               "Aspect", "Northings", "Eastings")]

NoCol<-BM[,c("Depth",  "CFW", "TPI", "Rugosity", "Northings", "Eastings")]

source(file="HighstatLib.R")

#Pairplots (multi-panel scatterplot)
pairs(Col_Con, upper.panel= panel.smooth, lower.panel = panel.cor)

#Variance inflation factors(VIF)
#library(AED)
corvif(NoCol)

#Pairplots only capturs 2-way relationships, whereas VIFs detect high-dimensional collinearity.

#Boxplot to assess collinearity between continuouss and categorical variables
bwplot(CFW ~ fStation, strip = strip.custom(bg = 'white'), cex = 1,
data=BM, xlab="Station", ylab="% CFW")

#################################################

#Removes the missing values (which are present in the response variable)
I=is.na(BM$fStation) | is.na(BM$Depth) | is.na(BM$ALL_FA_HA) | is.na(BM$TPI) | is.na(BM$Rugosity) | is.na(BM$Northings) | is.na(BM$Eastings) | is.na(BM$CFW)
BM <- BM[!I,]

#Visualisation of relationships between fish and covariates

Fish <- rep(BM$ALL_FA_HA, 6)
Fish01 <- Fish
Fish01[Fish > 0] <- 1
XNames <- rep(c("Depth",  "CFW", "TPI", "Rugosity", "Northings", "Eastings"))
AllID <- rep(XNames, each = length(BM$ALL_FA_HA))
AllX <- c("Depth",  "CFW", "TPI", "Rugosity", "Northings", "Eastings")
library(lattice)
library(mgcv)
xyplot(Fish01 ~ AllX | AllID, col = 1,
       strip = function(bg = 'white', ...)
             strip.default(bg = 'white', ...),
       scales = list(alternating = T, x = list(relation = "free"), 
                   y = list(relation = "same")), 
       xlab="Covariates", ylab="Presence/absence of All Fish",
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.points(jitter(x), abs(jitter(y)), col = 1, cex = 0.2)
         tmp <- gam(y ~ s(x), family=negbin)
         DF1 <- data.frame(x = seq(min(x, na.rm = TRUE),
                                   max(x, na.rm = TRUE), length = 100))
         P1 <- predict(tmp, newdata = DF1, type = "response")
         panel.lines(DF1$x, P1, col = 1, lwd = 6)})


BMNon0 <- BM[BM$ALL_FA_HA > 0,]
FishNon0 <- rep(BMNon0$ALL_FA_HA, 6)
XNames <- rep(c("Depth", "CFW", "TPI", "Rugosity", "Northings", "Eastings"))
AllID <- rep(XNames, each = length(BM$ALL_FA_HA))
AllX <- c("Depth", "CFW", "TPI", "Rugosity", "Northings", "Eastings")
#library(lattice)
library(mgcv)
xyplot(FishNon0 ~ AllX | AllID, col = 1,
       strip=function(bg = 'white', ...)
       strip.default(bg = 'white', ...),
       scales=list(alternating = T, 
                   x=list(relation = "free"), 
                   y=list(relation = "same")), 
       xlab="Covariates", ylab="Positive total fish abundance",
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1, cex = 0.5)
         tmp <- gam(y ~ s(x), family=negbin)
         DF1 <- data.frame(x = seq(min(x, na.rm = TRUE),
                                   max(x, na.rm = TRUE), length = 100))
         P1 <- predict(tmp, newdata = DF1, type = "response")
         panel.lines(DF1$x, P1, col = 1, lwd = 6)})


#Removes the missing values (which are present in the response variable)
I=is.na(BM$CFW) | is.na(BM$Depth) | is.na(BM$ALL_FA_HA) | is.na(BM$TPI) | is.na(BM$Rugosity) | is.na(BM$Northings) | is.na(BM$Eastings)
BM <- BM[!I,]

BM[BM$AllNO > 0] <- 1
MyX <- c("Eastings", "Northings", "Depth", "CFW", "TPI", "Rugosity")
AllX <- as.vector(as.matrix(BM[,MyX]))
ID <- rep(MyX, each = nrow(BM))
AllY <- rep(BM$ALLNO, length(MyX))
library(lattice)
xyplot(AllY ~ AllX | ID, col=1, xlab="Explanatory variables", ylab="Positive counts of all fish species",
       scales=list(alternating = T, 
                   x=list(relation = "free"), 
                   y=list(relation = "same")),
       panel=function(x,y) {
         panel.grid(h=-1, v=2)
         panel.points(x, y, col=1)
         panel.loess(x, y, col=1, lwd=2)
       })

#################################################

#Transect effects

par(mar = c(8,4,3,3))
boxplot(log(ALL_FA_HA) ~ fStation, varwidth = TRUE,
        cex = 0.5, las = 3, data=BMNon0, xlab="Station", ylab="Positive Total fish abundance")

#################################################

#Model fish abundance

BM$LogArea <- log(BM$SURVEYAREA_M2)
#Doesn't work with log transformed area!

M1 <- glm(ALLNO ~ Depth + CFW + TPI + Rugosity + Northings + Eastings + offset(AREA), family=poisson, data = BM)
#Shows warning message when offset is included

library(MASS)
M2 <- glm.nb(ALLNO ~ Depth + CFW + TPI + Rugosity + Northings + Eastings + offset(AREA), data = BM)
#Shows warning message when offset is included

library(pscl)
M3 <- zeroinfl(ALLNO ~ Depth + CFW + TPI + Rugosity + Northings + Eastings + offset(AREA), dist = "poisson", link = "logit", data = BM)
#Shows warning messages when offset is included

M4 <- zeroinfl(ALLNO ~ Depth + CFW + TPI + Rugosity + Northings + Eastings, dist = "negbin", link = "logit", data = BM)
#Shows warning messages when offset is included

#Check for overdispersion
sum(resid(M1, type = "pearson")^2)/M1$df.res

summary(M1)
anova(M1)



#################################################
#################################################
#################################################
#################################################

#ZIP GAM

#To avoid conflict between mgcv and VGAM packages it is best to close R, restart it and reload the data

library(VGAM)
stats::resid

ZI_GAM_1 <- vgam(ALL_FA_HA ~ s(Depth) + s(CFW) + s(TPI) + s(Rugosity) + s(Northings) + s(Eastings), data = BM,
             family= zipoisson, control = vgam.control(maxit = 100, epsilon = 1e-4))

#Non-convergence problem => Intercept only model for the logistic part of the ZIP.

#Supossedly solved by the following command, but didn't work!
constraints(GAM3)
m.SecondSmoother <- rbind(c(0,0), c(0,1))
GAM3.clist <- list("Intercept" = diag(2), 
                   "s(CFW)" = m.SecondSmoother,
                   "s(Depth)" = m.SecondSmoother,
                   "s(Prof_Curve)" = m.SecondSmoother,
                   "s(Tang_Curve)" = m.SecondSmoother,
                   "s(Rugosity)" = m.SecondSmoother,
                   "s(Northings)" = m.SecondSmoother,
                   "s(Eastings)" = m.SecondSmoother)
GAM3_Cor <- vgam(ALL_FA_HA ~ s(CFW) + s(Depth) + s(Prof_Curve) + s(Tang_Curve) + s(Rugosity) + s(Northings) + s(Eastings), 
                 family= zipoisson, constraints = GAM3.clist, data = BM, control = vgam.control(maxit = 100, epsilon = 1e-4))

summary(GAM3_Cor)

#Check for overdispersion
my.logit.inv <- function(x) {(1 / (1 + exp(-x)))}
GAM3.prd <- predict(GAM3)
GAM3.prd.pi <- my.logit.inv(GAM3.prd[,1])
GAM3.prd.mu <- exp(GAM3.prd[,2])
GAM3.E <- GAM3.prd.mu * (1 - GAM3.prd.pi)
GAM3.Var <- (1 - GAM3.prd.pi) * (GAM3.prd.mu + GAM3.prd.pi * GAM3.prd.mu^2)
GAM3.res <- (BM$ALL_FA_HA - GAM3.E)/sqrt(GAM3.Var)
sum(GAM3.res^2) / (nrow(BM)-3-3 -2)

#################################################

#ZINB GAM
ZI_GAM2 <- vgam(ALL_FA_HA ~ s(Depth) + s(CFW) + s(TPI) + s(Rugosity) + s(Northings) + s(Eastings), data = BM,
             family= zinegbinomial, control = vgam.control(maxit = 100, epsilon = 1e-4))

coef(GAM4, matrix=TRUE)
summary(GAM4)

#Check for overdispersion
my.logit.inv <- function(x) {(1 / (1 + exp(-x)))}
GAM4.prd <- predict(GAM4)
GAM4.prd.pi <- my.logit.inv(GAM4.prd[,1])
GAM4.prd.mu <- exp(GAM4.prd[,2])
GAM4.prd.k <- exp(GAM4.prd[,3])
GAM4.E <- GAM4.prd.mu * (1 - GAM4.prd.pi)
GAM4.Var <- (1 - GAM4.prd.pi) * GAM4.prd.mu * (1 + GAM4.prd.pi * GAM4.prd.mu + GAM4.prd.mu/GAM4.prd.k)
GAM4.res <- (BM$ALL_FA_HA - GAM4.E)/sqrt(GAM4.Var)
#Problem with calculating residual
sum(GAM4.res^2) / (nrow(BM)-3-3 -3)

#################################################