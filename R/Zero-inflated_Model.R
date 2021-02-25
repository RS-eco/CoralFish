#GLM on CoralFISH based on Chapter 21 of Mixed Effects Models and Extensions in Ecology with R (2009)

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Data")

#Import data
AR <- read.delim("AllRegions_VS_Data.csv", sep=",", header=T)

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

#names(BM)
str(AR)

#table()

#summary(AR)

#Removes the missing values (which are present in the response variable)
#I = is.na(AR$Depth) | is.na(AR$CFW) | is.na(AR$Rugosity) | is.na(AR$Aspect) | is.na(AR$TPI)
#AR <- AR[!I,]

#subset of dataset
subbelgica <- subset(AR, REGION == "Belgica Mound")
subhatton <- subset(AR, REGION == "Hatton Bank")
subrockall <- subset(AR, REGION == "Rockall Bank")

#################################################

#Look at the variance inflation factors (VIFs) of the variables to assess the extent of any remaining collinearity
AR_Var<-cbind(AR$Depth, AR$CFW, AR$Rugosity, AR$BPI_Fine, AR$Tang_Curve, AR$Northings, AR$Eastings)

library(AED)
corvif(AR_Var)

#Pearson Correlation
Pearson_CorMat_CoralCover <- cor(AR[,c("LCF", "DCF", "CFW", "CR", "SEDIMENT", "SEDIMENTCR")], 
                              y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
Pearson_CorMat_CoralCover

#Pearson_CorMat_FishAb&Bio
cor(AR[,c("LEPNO", "LEPWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("SYNNO", "SYNWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("MORANO", "MORAWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("CHIMNO", "CHIMWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("HELINO", "HELIWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("LOPHINO", "LOPHIWEIGH")], y = NULL, use = "pairwise.complete.obs", method = c("pearson")) #correlated (0.8818)
cor(AR[,c("MOLVANO", "MOLVAWEIGH")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("BROSMENO", "BROSMEWEIG")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("HOPLONO", "HOPLOWEIGH")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("PHYCISNO", "PHYCISWEIG")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("ALLNO", "ALLWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))

cor(AR[,c("LEPLENGTH", "LEPWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("SYNLENGTH", "SYNWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("MORALENGTH", "MORAWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson")) #correlated (0.8798)
cor(AR[,c("CHIMLENGTH", "CHIMWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson")) #correlated (0.8686)
cor(AR[,c("HELILENGTH", "HELIWEIGHT")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("LOPHILENGT", "LOPHIWEIGH")], y = NULL, use = "pairwise.complete.obs", method = c("pearson")) 
cor(AR[,c("MOLVALENGT", "MOLVAWEIGH")], y = NULL, use = "pairwise.complete.obs", method = c("pearson")) #correlated (0.9588)
cor(AR[,c("BROSMELENG", "BROSMEWEIG")], y = NULL, use = "pairwise.complete.obs", method = c("pearson")) #correlated (0.9711)
cor(AR[,c("HOPLOLENGT", "HOPLOWEIGH")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))
cor(AR[,c("PHYCISLENG", "PHYCISWEIG")], y = NULL, use = "pairwise.complete.obs", method = c("pearson"))

#################################################

AR$lArea <- log(AR$AREA)

#################################################

GLM1 <- glm(ALLNO ~ Depth + CFW + Rugosity + Northings + Eastings + TPI, family = poisson, offset(AR$AREA), data = AR)

#Check for overdispersion
sum(resid(GLM1, type = "pearson")^2) / GLM1$df.res

#Overdispersion = 80.13434

#################################################

library(MASS)
GLM2 <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + Northings + Eastings + TPI, data = AR, offset(AR$AREA), link = log)

#Check for overdispersion
sum(resid(GLM2, type = "pearson")^2) / GLM2$df.res

#Overdispersion  = 56.75877

#################################################

library(mgcv)

GAM1 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings) + s(Eastings) + s(TPI), offset(AR$AREA), family = poisson, data = AR)

GAM2 <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings) + s(Eastings) + s(TPI), offset(AR$AREA), family = negbin(c(0.1, 10)), data = AR)

#Check for overdispersion

sum(resid(GAM1, type = "pearson")^2) / GAM1$df.res
#Overdispersion = 68.86619

sum(resid(GAM2, type = "pearson")^2) / GAM2$df.res
#Overdispersion = 55.58357

#################################################

library(pscl)

ZIP1 <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + Northings + Eastings + BPI_Fine + Tang_Curve | Depth + CFW + Rugosity + Northings + Eastings + BPI_Fine + Tang_Curve , offset(AREA),
                 dist = c("poisson"), link = c("logit"), data = AR)

ZINB1 <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + Northings + Eastings + BPI_Fine + Tang_Curve | Depth + CFW + Rugosity + Northings + Eastings + BPI_Fine + Tang_Curve, offset(AREA),
                 dist = c("negbin"), link = c("logit"), data = AR)

#Check for overdispersion
summary(ZIP1)
sum(resid(ZIP1, type="pearson")^2) / (nrow(AR)-16)
#Dispersion = 1.045948

#Model validation
EP <- resid(M5, type = "pearson")
> ED <- resid(M5, type = "deviance")
> mu <- predict(M5, type = "response")
> E <- RK$TOT.N - mu
> EP2 <- E / sqrt(7.630148 * mu)
> op <- par(mfrow = c(2, 2))
> plot(x = mu, y = E, main = "Response residuals")
> plot(x = mu, y = EP, main = "Pearson residuals")
> plot(x = mu, y = EP2,
       main = "Pearson residuals scaled")
> plot(x = mu, y = ED, main = "Deviance residuals")
> par(op)


plot(resid(ZIP1, type="pearson"), AR$Depth)

summary(ZINB1)
sum(resid(ZINB1, type="pearson")^2) / (nrow(AR)-17)
#Dispersion = 0.9964592

#################################################

library(VGAM)
stats::resid

#ZIP GLM
ZIP2 <- vglm(ALLNO ~ CFW + Depth + Rugosity + Northings + Eastings + TPI, offset(AR$lArea), family = zipoisson, data = AR)

#ZINB GLM
ZINB2 <- vglm(ALLNO ~ CFW + Depth + Rugosity + Northings + Eastings + TPI, offset(AR$lArea), family = zinegbinomial, data = AR)

#Check for overdispersion
E3 <- resid(GLM1, type = "pearson")
sum(E3^2) / (nrow(AR)-3-3 -2)

#################################################

#ZIP GAM
ZIP3 <- vgam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings) + s(Eastings) + s(TPI), offset(AR$lArea), family = zipoisson, data = AR)

#ZINB GAM
ZINB3 <- vgam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(Northings) + s(Eastings) + s(TPI), family = zinegbinomial, offset(AR$lArea), data = AR)

coef(GAM1, matrix=TRUE)
summary(GAM1)

#Check for overdispersion
my.logit.inv <- function(x) {(1 / (1 + exp(-x)))}
ZIP3.prd <- predict(ZIP3)
ZIP3.prd.pi <- my.logit.inv(ZIP3.prd[,1])
ZIP3.prd.mu <- exp(ZIP3.prd[,2])
ZIP3.E <- ZIP3.prd.mu * (1 - ZIP3.prd.pi)
ZIP3.Var <- (1 - ZIP3.prd.pi) * (ZIP3.prd.mu + ZIP3.prd.pi * ZIP3.prd.mu^2)
ZIP3.res <- (AR$ALLNO - ZIP3.E)/sqrt(ZIP3.Var)

#Problem with calculating residual? (Change -2!!!)
Dis_ZIP3 <- sum(ZIP3.res^2) / (nrow(AR)-3-3 -2)

my.logit.inv <- function(x) {(1 / (1 + exp(-x)))}
ZINB3.prd <- predict(ZINB3)
ZINB3.prd.pi <- my.logit.inv(ZINB3.prd[,1])
ZINB3.prd.mu <- exp(ZINB3.prd[,2])
ZINB3.prd.k <- exp(ZINB3.prd[,3])
ZINB3.E <- ZINB3.prd.mu * (1 - ZINB3.prd.pi)
ZINB3.Var <- (1 - ZINB3.prd.pi) * ZINB3.prd.mu * (1 + ZINB3.prd.pi * ZINB3.prd.mu + ZINB3.prd.mu / ZINB3.prd.k)
ZINB3.res <- (AR$ALLNO - ZINB3.E)/sqrt(ZINB3.Var)

#Problem with calculating residual? (Change -2!!!)
Dis_ZINB3 <- sum(ZINB3.res^2) / (nrow(AR)-3-3 -2)

#################################################

#Compare all estimated models
Z <- cbind(c(deviance(GLM1), deviance(GLM2), deviance(GAM1), deviance(GAM2), deviance(ZIP1), deviance(ZINB1)),
           c(deviance(GLM1)/GLM1$df.res, deviance(GLM2)/GLM2$df.res, deviance(GAM1)/GAM1$df.res, deviance(GAM2)/GAM2$df.res, 
             sum(residuals(ZIP1, type = "pearson")^2) / ZIP1$df.res, sum(residuals(ZINB1, type = "pearson")^2) / ZINB1$df.res),
           c(AIC(GLM1), AIC(GLM2), AIC(GAM1), AIC(GAM2), AIC(ZIP1), AIC(ZINB1)), 
           c(logLik(GLM1), logLik(GLM2), logLik(GAM1), logLik(GAM2), logLik(ZIP1), logLik(ZINB1)))
colnames(Z) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(Z) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM", "ZIP GLM", "ZINB GLM")
Z

#################################################

#GAM with a negative binomial distribution

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