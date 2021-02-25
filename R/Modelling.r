#GLM on CoralFISH based on Chapter 21 of Mixed Effects Models and Extensions in Ecology with R (2009)

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Data")

#Import data
AR <- read.delim("AllRegions_VS_Data.csv", sep=",", header=T)
BM <- read.delim("Belgica_Zone29N_VS_Data.csv", sep=",",header=T)
HB <- read.delim("Hatton_Zone27N_VS_Data.csv", sep=",",header=T)
RB <- read.delim("Rockall_Zone28N_VS_Data.csv", sep=",",header=T)

#Rename (copy) covariables

AR$AREA <- AR$SURVEYAREA
AR$Depth <- AR$X50M_MASK
AR$Aspect <- AR$X50M_ASPECT
AR$BPI_Broad <- AR$X50M_BPI_BROAD
AR$BPI_Fine <- AR$X50M_BPI_FINE
AR$Plan_Curve <- AR$X50M_PLAN_CURVE
AR$Prof_Curve <- AR$X50M_PROFILE_CURVATURE
AR$Roughness <- AR$X50M_ROUGHNESS
AR$Rugosity <- AR$X50M_RUGOSITY
AR$Slope <- AR$X50M_SLOPE_DEG
AR$Tang_Curve <- AR$X50M_TANGENTIAL_CURVATURE
AR$TPI <- AR$X50M_TPI
AR$TRI <- AR$X50M_TRI
AR$Northings <- AR$X50M_NORTHINGS 
AR$Eastings <- AR$X50M_EASTINGS

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
BM$Slope <- BM$BELGICA_ZONE29NN_50M_MASK_SLOPE_DEG
BM$Tang_Curve <- BM$BELGICA_ZONE29N_50M_MASK_TANGENTIAL_CURVATURE
BM$TPI <- BM$BELGICA_ZONE29N_50M_MASK_TPI
BM$TRI <- BM$BELGICA_ZONE29N_50M_MASK_TRI
BM$Northings <- BM$BELGICA_ZONE29N_50M_MASK_NORTHINGS 
BM$Eastings <- BM$BELGICA_ZONE29N_50M_MASK_EASTINGS

HB$AREA <- HB$SURVEYAREA
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

RB$AREA <- RB$SURVEYAREA
RB$X <- RB$XCoord
RB$Y <- RB$YCoord
RB$Depth <- RB$ROCKALL_ZONE28N_50M
RB$Aspect <- RB$ROCKALL_ZONE28N_50M_ASPECT
RB$BPI_Broad <- RB$ROCKALL_ZONE28N_50M_BPI_BROAD
RB$BPI_Fine <- RB$ROCKALL_ZONE28N_50M_BPI_FINE
RB$Plan_Curve <- RB$ROCKALL_ZONE28N_50M_PLAN_CURVATURE
RB$Prof_Curve <- RB$ROCKALL_ZONE28N_50M_PROFILE_CURVATURE
RB$Roughness <- RB$ROCKALL_ZONE28N_50M_ROUGHNESS
RB$Rugosity <- RB$ROCKALL_ZONE28N_50M_RUGOSITY
RB$Slope <- RB$ROCKALL_ZONE28N_50M_SLOPE_DEG
RB$Tang_Curve <- RB$ROCKALL_ZONE28N_50M_TANGENTIAL_CURVATURE
RB$TPI <- RB$ROCKALL_ZONE28N_50M_TPI
RB$TRI <- RB$ROCKALL_ZONE28N_50M_TRI
RB$Northings <- RB$ROCKALL_ZONE28N_50M_NORTHINGS 
RB$Eastings <- RB$ROCKALL_ZONE28N_50M_EASTINGS

#Converts count data into absence/presence
#DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
#DeerEcervi$Ecervi.01[DeerEcervi$Ecervi >0 ] <- 1

#Transform STATION into Factor
BM$Station <- factor(BM$STATION)

#Transform Non-integer to Integer values
#BM$Depth <- as.integer(BM$Depth_m)
BM$ALL_FA_HA <- as.integer(BM$ALL_FA_HA)

#names(BM)
str(BM)

#table()

#Removes the missing values (which are present in the response variable)
#I=is.na(BM$fRegion) | is.na(BM$Station) | is.na(BM$Depth_Int) | is.na(BM$CFW_Bin) | is.na(BM$Longitude) | is.na(BM$Latitude)
#BM2 <- BM[!I,]

#################################################

#Plot of the frequencies for the response variable Intensity from cod parasite data.
plot(table(BM$ALL_FA_HA),ylab="Frequency", xlab="Number of fish per ha/Observed values")
table(BM$ALL_FA_HA)
summary(table(BM$ALLFA_HA))
summary(BM$ALL_FA_HA)

#Length frequency histogram
hist(CoralFISH$LepLength, xlab = "Weight (kg)", breaks = 50, main = "", ylab = "Frequency")

#Histogram with Lattice
library(lattice)
histogram( ~ wt | fMonth, type = "count",
           xlab = "Weight (g)",
           ylab = "Frequency",
           nint=30,layout=c(1,3),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = fMonth =="June" | fMonth == "July" |fMonth == "August",
           data = Sparrows)


#################################################

#Look at the variance inflation factors (VIFs) of the variables to assess the extent of any remaining collinearity
VIF<-cbind(BM$AREA, BM$Station, BM$XCoord, BM$YCoord, BM$BELGICA_ZONE29N_50M_MASK, BM$BELGICA_ZONE29N_50M_MASK_ASPECT, 
         BM$BELGICA_ZONE29N_50M_MASK_BPI_BROAD, BM$BELGICA_ZONE29N_50M_MASK_BPI_FINE, BM$BELGICA_ZONE29N_50M_MASK_PLAN_CURVATURE, 
         BM$BELGICA_ZONE29N_50M_MASK_PROFILE_CURVATURE, BM$BELGICA_ZONE29N_50M_MASK_ROUGHNESS, BM$BELGICA_ZONE29N_50M_MASK_RUGOSITY, 
         BM$BELGICA_ZONE29N_50M_MASK_SLOPE_DEG, BM$BELGICA_ZONE29N_50M_MASK_TANGENTIAL_CURVATURE, BM$BELGICA_ZONE29N_50M_MASK_TPI, 
         BM$BELGICA_ZONE29N_50M_MASK_TRI, BM$BELGICA_ZONE29N_50M_MASK_NORTHINGS, BM$BELGICA_ZONE29N_50M_EASTINGS)

VIF_below5<-cbind(BM$Station, BM$YCoord, BM$BELGICA_ZONE29N_50M_MASK, BM$BELGICA_ZONE29N_50M_MASK_ASPECT, 
                BM$BELGICA_ZONE29N_50M_MASK_BPI_BROAD, BM$BELGICA_ZONE29N_50M_MASK_PROFILE_CURVATURE, 
                BM$BELGICA_ZONE29N_50M_MASK_RUGOSITY, BM$BELGICA_ZONE29N_50M_MASK_TANGENTIAL_CURVATURE, 
                BM$BELGICA_ZONE29N_50M_MASK_NORTHINGS, BM$BELGICA_ZONE29N_50M_EASTINGS)

VIF_below3<-cbind(BM$AREA, BM$Station, BM$YCoord, BM$BELGICA_ZONE29N_50M_MASK, BM$BELGICA_ZONE29N_50M_MASK_PROFILE_CURVATURE, 
                BM$BELGICA_ZONE29N_50M_MASK_RUGOSITY, BM$BELGICA_ZONE29N_50M_MASK_TANGENTIAL_CURVATURE, 
                BM$BELGICA_ZONE29N_50M_MASK_NORTHINGS, BM$BELGICA_ZONE29N_50M_EASTINGS)

library(AED)
corvif(VIF_below3)

#################################################

#GLM with Poisson distribution
GLM1 <- glm(ALLNO ~ CFW + Depth + Prof_Curve + Tang_Curve + Rugosity + Northings + Eastings, family=poisson, data = BM)

summary(GLM1)

#Check for overdispersion
E1 <- resid(GLM1, type = "pearson")
sum(E1^2) /GLM1$df.res
#Model is overdispersed if the sum of squared Pearson residuals to the residual degress of freedom is higher than 1.

#################################################

#NB GLM
library(MASS)
GLM2 <- glm.nb(ALLNO ~ CFW + Depth + Prof_Curve + Tang_Curve + Rugosity + Northings + Eastings, link="log", data=BM)

summary(GLM2)

#Check for overdispersion
E2 <- resid(GLM2, type = "pearson")
sum(E2^2)/GLM2$df.res

#Apply smoothing model to check for model 
library(mgcv)
 <- gam(E2 ~ s(), data= BM)
anova()
#A significant smoother would indicate model misspecification for the NB GLM

#################################################

#ZINB GLM
library(VGAM)
stats::resid
GLM3 <- vglm(ALLNO ~ CFW + Depth + Prof_Curve + Tang_Curve + Rugosity + Northings + Eastings, family = zinegbinomial, 
             control = vglm.control(maxit = 100), data = BM)

summary(GLM3)

#Check for overdispersion
E3 <- resid(GLM3, type = "pearson")
sum(E3^2) / (nrow(BM)-3-3 -2)

#################################################

#ZIP GLM
library(pscl)
GLM4 <- zeroinfl(ALLNO ~ CFW + Depth + Prof_Curve + Tang_Curve + Rugosity + Northings + Eastings, 
        dist = "poisson", link="logit", data=BM)

summary(GLM4)

#Check for overdispersion
E4 <- resid(GLM4, type = "pearson")
sum(E4^2)/GLM5$df.res

#################################################

#ZINB GLM
library(pscl)
GLM5 <- zeroinfl(ALLNO ~ CFW + Depth + Prof_Curve + Tang_Curve + Rugosity + Northings + Eastings, 
                 dist = "negbin", link="logit", data=BM)

summary(GLM5)

#Check for overdispersion
E5 <- resid(GLM5, type = "pearson")
sum(E5^2)/GLM5$df.res

#################################################

#ZAP GLM
library(pscl)
GLM6 <- hurdle(ALL_FA_HA ~ Depth + CFW + Rugosity + AREA + Northings + Prof_Curve,  dist = "poisson", link = "logit", 
            data = BM)

#Check for overdispersion
E6 <- resid(GLM6, type = "pearson")
sum(E6^2)/GLM6$df.res

#################################################

#ZANB
GLM7 <- hurdle(ALL_FA_HA ~ Depth + CFW + Rugosity + AREA + Northings + Prof_Curve, dist = "negbin", link = "logit",
        data = BM)

#Check for overdispersion
E7 <- resid(GLM7, type = "pearson")
sum(E7^2)/GLM7$df.res

#################################################

#GAM with Poisson distribution
library(mgcv)
GAM1 <- gam(ALLNO ~ s(CFW) + s(Depth) + s(Prof_Curve) + s(Tang_Curve) + s(Rugosity) + s(Northings) + s(Eastings), family= poisson, data = BM)

summary(GAM1)

#Check for overdispersion
sum(resid(GAM1, type = "pearson")^2) /GAM1$df.res

#################################################

#GAM with a negative binomial distribution
library(mgcv)
GAM2 <- gam(ALLNO ~ s(CFW) + s(Depth) + s(Prof_Curve) + s(Tang_Curve) + s(Rugosity) + s(Northings) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)

summary(GAM2)

#Check for overdispersion
sum(resid(GAM2, type = "pearson")^2)/GAM2$df.res

#################################################

#ZIP GAM

#To avoid conflict between mgcv and VGAM packages it is best to close R, restart it and reload the data

library(VGAM)
stats::resid

GAM3 <- vgam(ALLNO ~ s(CFW) + s(Depth) + s(Prof_Curve) + s(Tang_Curve) + s(Rugosity) + s(Northings) + s(Eastings), data = BM,
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
GAM4 <- vgam(ALLNO ~ s(CFW) + s(Depth) + Prof_Curve + Tang_Curve + s(Rugosity) + Northings + Eastings, data = BM,
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

#Compare all estimated models
Z <- cbind(c(deviance(GLM1), deviance(GLM2), deviance(GAM1), deviance(GAM2)),
           c(deviance(GLM1)/GLM1$df.res, deviance(GLM2)/GLM2$df.res, deviance(GAM1)/GAM1$df.res, deviance(GAM2)/GAM2$df.res),
           c(AIC(GLM1), AIC(GLM2), AIC(GAM1), AIC(GAM2)), c(logLik(GLM1), logLik(GLM2), logLik(GAM1), logLik(GAM2)))
colnames(Z) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(Z) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM")
Z


#################################################

#GAM with a negative binomial distribution
library(mgcv)

NB_GAM1 <- gam(ALLNO ~ s(Depth), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM2 <- gam(ALLNO ~ s(CFW), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM3 <- gam(ALLNO ~ s(Rugosity), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM4 <- gam(ALLNO ~ s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM5 <- gam(ALLNO ~ s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM6 <- gam(ALLNO ~ s(AREA), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM7 <- gam(ALLNO ~ s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM8 <- gam(ALLNO ~ s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM9 <- gam(ALLNO ~ s(Depth) + s(CFW), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM10 <- gam(ALLNO ~ s(Depth) + s(Rugosity), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM11 <- gam(ALLNO ~ s(Depth) + s(AREA), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM12 <- gam(ALLNO ~ s(Depth) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM13 <- gam(ALLNO ~ s(Depth) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM14 <- gam(ALLNO ~ s(Depth) + s(Northings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM15 <- gam(ALLNO ~ s(Depth) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM16 <- gam(ALLNO ~ s(Depth) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM17 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM18 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(AREA), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM19 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM20 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Northings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM21 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM22 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM23 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM24 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM25 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM26 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM27 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM28 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM29 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM30 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM31 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM32 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM33 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM34 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM35 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM36 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM37 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM38 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM39 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM40 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM41 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM42 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(Eastings), family=negbin (c(0.1,10), link=log), data = BM)
NB_GAM43 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)

NB_GAM44 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(Eastings) + s(YCoord), family=negbin (c(0.1,10), link=log), data = BM)

AIC(NB_GAM1, NB_GAM2, NB_GAM3, NB_GAM4, NB_GAM5, NB_GAM6, NB_GAM7, NB_GAM8)
AIC(NB_GAM9, NB_GAM10, NB_GAM11, NB_GAM12, NB_GAM13, NB_GAM14, NB_GAM15, NB_GAM16)
AIC(NB_GAM17, NB_GAM18, NB_GAM19, NB_GAM20, NB_GAM21, NB_GAM22, NB_GAM23)
AIC(NB_GAM24, NB_GAM25, NB_GAM26, NB_GAM27, NB_GAM28, NB_GAM29)
AIC(NB_GAM30, NB_GAM31, NB_GAM32, NB_GAM33, NB_GAM34)
AIC(NB_GAM35, NB_GAM36, NB_GAM37, NB_GAM38)
AIC(NB_GAM39, NB_GAM40, NB_GAM41)
AIC(NB_GAM42, NB_GAM43, NB_GAM44)
AIC(NB_GAM44)

summary(NB_GAM40)
anova(NB_GAM40, NB_GAM44)

#################################################

#GAM with a negative binomial distribution
library(mgcv)

NB_GAM1_HB <- gam(ALLNO ~ s(Depth), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM2_HB <- gam(ALLNO ~ s(CFW), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM3_HB <- gam(ALLNO ~ s(Rugosity), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM4_HB <- gam(ALLNO ~ s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM5_HB <- gam(ALLNO ~ s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM6_HB <- gam(ALLNO ~ s(AREA), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM7_HB <- gam(ALLNO ~ s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM8_HB <- gam(ALLNO ~ s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM9_HB <- gam(ALLNO ~ s(Depth) + s(CFW), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM10_HB <- gam(ALLNO ~ s(Depth) + s(Rugosity), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM11_HB <- gam(ALLNO ~ s(Depth) + s(AREA), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM12_HB <- gam(ALLNO ~ s(Depth) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM13_HB <- gam(ALLNO ~ s(Depth) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM14_HB <- gam(ALLNO ~ s(Depth) + s(Northings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM15_HB <- gam(ALLNO ~ s(Depth) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM16_HB <- gam(ALLNO ~ s(Depth) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM17_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM18_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(AREA), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM19_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM20_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Northings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM21_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM22_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM23_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM24_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM25_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM26_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM27_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM28_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM29_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM30_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM31_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM32_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM33_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM34_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM35_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM36_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM37_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM38_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM39_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM40_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM41_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM42_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(Eastings), family=negbin (c(0.1,10), link=log), data = HB)
NB_GAM43_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)

NB_GAM44_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(Eastings) + s(YCoord), family=negbin (c(0.1,10), link=log), data = HB)

AIC(NB_GAM1_HB, NB_GAM2_HB, NB_GAM3_HB, NB_GAM4_HB, NB_GAM5_HB, NB_GAM6_HB, NB_GAM7_HB, NB_GAM8_HB)
AIC(NB_GAM9_HB, NB_GAM10_HB, NB_GAM11_HB, NB_GAM12_HB, NB_GAM13_HB, NB_GAM14_HB, NB_GAM15_HB, NB_GAM16_HB)
AIC(NB_GAM17_HB, NB_GAM18_HB, NB_GAM19_HB, NB_GAM20_HB, NB_GAM21_HB, NB_GAM22_HB, NB_GAM23_HB)
AIC(NB_GAM24_HB, NB_GAM25_HB, NB_GAM26_HB, NB_GAM27_HB, NB_GAM28_HB, NB_GAM29_HB)
AIC(NB_GAM30_HB, NB_GAM31_HB, NB_GAM32_HB, NB_GAM33_HB, NB_GAM34_HB)
AIC(NB_GAM35_HB, NB_GAM36_HB, NB_GAM37_HB, NB_GAM38_HB)
AIC(NB_GAM39_HB, NB_GAM40_HB, NB_GAM41_HB)
AIC(NB_GAM42_HB, NB_GAM43_HB, NB_GAM44_HB)

summary(NB_GAM40_HB)
anova(NB_GAM40, NB_GAM44)

#################################################

#GAM with a negative binomial distribution
library(mgcv)

NB_GAM1_RB <- gam(ALLNO ~ s(Depth), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM2_RB <- gam(ALLNO ~ s(CFW), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM3_RB <- gam(ALLNO ~ s(Rugosity), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM4_RB <- gam(ALLNO ~ s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM5_RB <- gam(ALLNO ~ s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM6_RB <- gam(ALLNO ~ s(AREA), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM7_RB <- gam(ALLNO ~ s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM8_RB <- gam(ALLNO ~ s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM9_RB <- gam(ALLNO ~ s(Depth) + s(CFW), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM10_RB <- gam(ALLNO ~ s(Depth) + s(Rugosity), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM11_RB <- gam(ALLNO ~ s(Depth) + s(AREA), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM12_RB <- gam(ALLNO ~ s(Depth) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM13_RB <- gam(ALLNO ~ s(Depth) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM14_RB <- gam(ALLNO ~ s(Depth) + s(Northings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM15_RB <- gam(ALLNO ~ s(Depth) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM16_RB <- gam(ALLNO ~ s(Depth) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM17_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM18_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(AREA), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM19_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM20_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Northings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM21_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM22_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM23_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM24_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM25_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM26_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM27_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM28_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM29_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM30_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM31_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM32_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM33_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM34_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM35_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM36_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM37_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM38_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM39_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM40_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM41_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM42_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(Eastings), family=negbin (c(0.1,10), link=log), data = RB)
NB_GAM43_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)

NB_GAM44_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(AREA) + s(Northings) + s(Prof_Curve) + s(Tang_Curve) + s(Eastings) + s(YCoord), family=negbin (c(0.1,10), link=log), data = RB)

AIC(NB_GAM1_RB, NB_GAM2_RB, NB_GAM3_RB, NB_GAM4_RB, NB_GAM5_RB, NB_GAM6_RB, NB_GAM7_RB, NB_GAM8_RB)
AIC(NB_GAM9_RB, NB_GAM10_RB, NB_GAM11_RB, NB_GAM12_RB, NB_GAM13_RB, NB_GAM14_RB, NB_GAM15_RB, NB_GAM16_RB)
AIC(NB_GAM17_RB, NB_GAM18_RB, NB_GAM19_RB, NB_GAM20_RB, NB_GAM21_RB, NB_GAM22_RB, NB_GAM23_RB)
AIC(NB_GAM24_RB, NB_GAM25_RB, NB_GAM26_RB, NB_GAM27_RB, NB_GAM28_RB, NB_GAM29_RB)
AIC(NB_GAM30_RB, NB_GAM31_RB, NB_GAM32_RB, NB_GAM33_RB, NB_GAM34_RB)
AIC(NB_GAM35_RB, NB_GAM36_RB, NB_GAM37_RB, NB_GAM38_RB)
AIC(NB_GAM39_RB, NB_GAM40_RB, NB_GAM41_RB)
AIC(NB_GAM42_RB, NB_GAM43_RB, NB_GAM44_RB)

summary(NB_GAM40_RB)
anova(NB_GAM40, NB_GAM44)

#################################################

#Compare all estimated models
T <- cbind(c(deviance(GLM1), deviance(GLM2), deviance(GAM1), deviance(GAM2)),
           ,
           c(AIC(NB_GAM1), AIC(NB_GAM2), AIC(NB_GAM3, AIC(NB_GAM4), AIC(NB_GAM5), AIC(NB_GAM6), AIC(NB_GAM9), AIC(NB_GAM10)), 
           c(logLik(NB_GAM1), logLik(NB_GAM2), logLik(NB_GAM3), logLik(NB_GAM4)))
colnames(T) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(T) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM")

T

#################################################

summary(GAM2)
logLik(GAM2)
anova(GAM2)

#Likelihood-ratio test for nested Models
library(lmtest)
lrtest(zinb_hb_9,zinb_hb_6)

#################################################

plot(M11$gam)
E <- resid(M11$lme, type = "normalized")
F <- fitted(M11$lme)
plot(x = F, y = E, xlab = "Fitted values", ylab = "Residuals", cex = 0.3)

#Model validation
op <- par(mfrow = c(5, 2), mar = c(5, 4, 1, 2))
plot(M1, add.smooth = FALSE)
E <- resid(M1)
hist(E, xlab = "Residuals", main = "")
plot(CF$fRegion, E, xlab = "Region", ylab = "Residuals")
plot(CF$Station, E, xlab = "Station", ylab = "Residuals")
plot(CF$Depth_Int, E, xlab = "Depth", ylab = "Residuals")
plot(CF$Longitude, E, xlab = "Longitude", ylab = "Residuals")
plot(CF$Latitude, E, xlab = "Latitude", ylab ="Residuals")
par(op)