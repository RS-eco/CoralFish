#Calculations for Paper

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/Data")

#Import data
VS_perMin <- read.delim("CoralFISH_1min.csv", sep=",", header=T)
VS_perStation <- read.delim("VS_perStation.csv", sep=",", header=T)
VS_perRegion <- read.delim("VS_perRegion.csv", sep=",", header=T)

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

#General Summary
summaryDepth <- summarySE(VS_perMin, measurevar="AvDepth_m", groupvars="Region")
summaryArea <- summarySE(VS_perMin, measurevar="SurveyArea_m")
summaryWidth <- summarySE(VS_perMin, measurevar="Width_m")

summaryMin<- summarySE(VS_perStation, measurevar="Minutes")
summaryLength <- summarySE(VS_perStation, measurevar="TransectLength_m")
summaryAreaPerStation <- summarySE(VS_perStation, measurevar="SurveyArea_m")

#Coral data
summaryLCF <- summarySE(VS_perMin, measurevar="X.LCF", groupvars="Region")
summaryDCF <- summarySE(VS_perMin, measurevar="X.DCF", groupvars="Region")
summaryCFW <- summarySE(VS_perMin, measurevar="X.CFW", groupvars="Region")
summaryCR <- summarySE(VS_perMin, measurevar="X.CR", groupvars="Region")
summarySEDIMENT <- summarySE(VS_perMin, measurevar="X.SEDIMENT", groupvars="Region")
summarySEDIMENTCR <- summarySE(VS_perMin, measurevar="X.SEDIMENTCR", groupvars="Region")

#Fish data
#Abundance per Species
VS_perStation$Area_ha <- VS_perStation$SurveyArea_m/10000

VS_perStation$LEPFA <- VS_perStation$LEPNO/VS_perStation$Area_ha
summaryLEPFA <- summarySE(VS_perStation, measurevar="LEPFA", groupvars="Region")

VS_perStation$SYNFA <- VS_perStation$SYNNO/VS_perStation$Area_ha
summarySYNFA <- summarySE(VS_perStation, measurevar="SYNFA")

VS_perStation$SIGFA <- VS_perStation$SIGNO/VS_perStation$Area_ha
summarySIGFA <- summarySE(VS_perStation, measurevar="SIGFA")

VS_perStation$MORAFA <- VS_perStation$MORANO/VS_perStation$Area_ha
summaryMORAFA <- summarySE(VS_perStation, measurevar="MORAFA")

VS_perStation$CHIMFA <- VS_perStation$CHIMNO/VS_perStation$Area_ha
summaryCHIMFA <- summarySE(VS_perStation, measurevar="CHIMFA")

VS_perStation$HELIFA <- VS_perStation$HELINO/VS_perStation$Area_ha
summaryHELIFA <- summarySE(VS_perStation, measurevar="HELIFA")

VS_perStation$LOPHIFA <- VS_perStation$LOPHINO/VS_perStation$Area_ha
summaryLOPHIFA <- summarySE(VS_perStation, measurevar="LOPHIFA", groupvars="Region")

VS_perStation$MOLVAFA <- VS_perStation$MOLVANO/VS_perStation$Area_ha
summaryMOLVAFA <- summarySE(VS_perStation, measurevar="MOLVAFA", groupvars="Region")

VS_perStation$BROSMEFA <- VS_perStation$BROSMENO/VS_perStation$Area_ha
summaryBROSMEFA <- summarySE(VS_perStation, measurevar="BROSMEFA", groupvars="Region")

VS_perStation$HOPLOFA <- VS_perStation$HOPLONO/VS_perStation$Area_ha
summaryHOPLOFA <- summarySE(VS_perStation, measurevar="HOPLOFA", groupvars="Region")

VS_perStation$PHYCISFA <- VS_perStation$PHYCISNO/VS_perStation$Area_ha
summaryPHYCISFA <- summarySE(VS_perStation, measurevar="PHYCISFA", groupvars="Region")

VS_perStation$PSEUDOFA <- VS_perStation$PSEUDONO/VS_perStation$Area_ha
summaryPSEUDOFA <- summarySE(VS_perStation, measurevar="PSEUDOFA", groupvars="Region")

VS_perStation$RAJAFA <- VS_perStation$RAJANO/VS_perStation$Area_ha
summaryRAJAFA <- summarySE(VS_perStation, measurevar="RAJAFA", groupvars="Region")


#Biomass per species

VS_perStation$LEPFB <- VS_perStation$LEPWEIGHT/VS_perStation$Area_ha
summaryLEPFB <- summarySE(VS_perStation, measurevar="LEPFB")

VS_perStation$SYNFB <- VS_perStation$SYNWEIGHT/VS_perStation$Area_ha
summarySYNFB <- summarySE(VS_perStation, measurevar="SYNFB", groupvars="Region")

VS_perStation$MORAFB <- VS_perStation$MORAWEIGHT/VS_perStation$Area_ha
summaryMORAFB <- summarySE(VS_perStation, measurevar="MORAFB")

VS_perStation$CHIMFB <- VS_perStation$CHIMWEIGHT/VS_perStation$Area_ha
summaryCHIMFB <- summarySE(VS_perStation, measurevar="CHIMFB", groupvars="Region")

VS_perStation$HELIFB <- VS_perStation$HELIWEIGHT/VS_perStation$Area_ha
summaryHELIFB <- summarySE(VS_perStation, measurevar="HELIFB")

VS_perStation$LOPHIFB <- VS_perStation$LOPHIWEIGH/VS_perStation$Area_ha
summaryLOPHIFB <- summarySE(VS_perStation, measurevar="LOPHIFB")

VS_perStation$MOLVAFB <- VS_perStation$MOLVAWEIGH/VS_perStation$Area_ha
summaryMOLVAFB <- summarySE(VS_perStation, measurevar="MOLVAFB", groupvars="Region")

VS_perStation$BROSMEFB <- VS_perStation$BROSMEWEIG/VS_perStation$Area_ha
summaryBROSMEFB <- summarySE(VS_perStation, measurevar="BROSMEFB", groupvars="Region")

VS_perStation$HOPLOFB <- VS_perStation$HOPLOWEIGH/VS_perStation$Area_ha
summaryHOPLOFB <- summarySE(VS_perStation, measurevar="HOPLOFB", groupvars="Region")

VS_perStation$PHYCISFB <- VS_perStation$PHYCISWEIG/VS_perStation$Area_ha
summaryPHYCISFB <- summarySE(VS_perStation, measurevar="PHYCISFB", groupvars="Region")

#Abundance and biomass per Region
VS_perStation$Area_ha <- VS_perStation$SurveyArea_m/10000
VS_perStation$ALLFA <- VS_perStation$ALLNO/VS_perStation$Area_ha
summaryALLFA <- summarySE(VS_perStation, measurevar="ALLFA", groupvars="Region")

VS_perStation$ALLFB <- VS_perStation$ALLWEIGHT/VS_perStation$Area_ha
summaryALLFB <- summarySE(VS_perStation, measurevar="ALLFB", groupvars="Region")

