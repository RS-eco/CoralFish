#R-code for figures and tables of Biber et al. 2013 - Fish coral associations in the deep Northeast Atlantic

#Load required packages
library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
setwd("/home/mabi/CoralFish")

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

##############################################################################

#Figure 1

#Fig. 1. (a) Map of the study area in the North-east Atlantic with the three 683 sampling sites indicated. 
#(b) Close-up of Rockall Bank with tracks of video surveys made on Haas Mound and non-coral areas. 
#(c) Close-up of Hatton Bank with video tracks indicated. 
#(d) Close-up of Belgica Mound Province with video tracks on mounds and off-mound area indicated.

#See ArcGIS File/Andy

##############################################################################

#Table 1

#Table 1. Details of each video survey for all three study areas. 
#RB = Rockall Bank, HB = Hatton Bank, BMP = Belgica Mound Province. 
#% CFW is the mean percentage of coral frame work coverage calculated from every frame (1min interval).

#See Excel File with Tables

##############################################################################

#Figure 2

#Fig. 2. (a) The digital video camera system used during video surveys in 2006, 2008 and 2010. 
#(b) The same camera system used in 2009 mounted on a drop frame. 
#(c) The HD camera system with optical cable as used during video surveys in 2011.

#Pictures from Cruise

##############################################################################

#Table 2

#Table 2. List of all fish species encountered and their length-weight relationship if available.
#Abbreviations: a = intercept, b = slope. Values represent calculated means from FishBase records.

#See Excel File with Tables

##############################################################################

#Figure 3

#Fig. 3. Total fish abundance (ind. ha-1) and biomass (kg ha-1) for coral and non-coral framework patches; 
#Error bars indicate ? 1 S.E.; * = p < 0.05 (Kruskal-Wallis Test).

#Read data
data <- read.delim("CoralFISH_BarGraph.csv", sep=",",header=T)

#Creates custom theme ... White with black lines, no bar fill, font sizes set
theme_update(panel.grid.minor= theme_blank(),
             panel.grid.major= theme_blank(),
             panel.background= theme_blank(),
             axis.title.x= theme_text( family= "serif", angle= 0 , size= 12),
             axis.text.x= theme_text( family= "serif", angle= 0 , size= 12),
             axis.text.y= theme_text( family= "serif" , size= 12),
             axis.title.y= theme_text( family= "serif", angle= 90 , size= 12))
theme_map <- theme_get()
theme_set(theme_bw())

#Fish abundance
summaryAllSpeciesFA <- summarySE(data, measurevar="AllFA", groupvars="Coral")

plotAllSpeciesFA <- ggplot(summaryAllSpeciesFA, aes(x=Coral, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("Fish abundance (ind. ha-1)") +
  ylim(0,250) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Fish biomass
summaryAllSpeciesFB <- summarySE(data, measurevar="AllFB", groupvars="Coral")

plotAllSpeciesFB <- ggplot(summaryAllSpeciesFB, aes(x=Coral, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("Fish biomass (kg ha-1)") +
  ylim(0,100) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFA~Coral, data=data)
kruskal.test(AllFB~Coral, data=data)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

print(plotAllSpeciesFA, vp = vplayout(1, 1))
print(plotAllSpeciesFB, vp = vplayout(1, 2))

##############################################################################

#Figure 4

#Fig. 4. Fish abundance (ind. ha-1) of several species for coral and non-coral framework patches of the three study sites; 
#Error bars indicate ? 1 S.E.; * = p < 0.05 (Kruskal-Wallis Test).

#Read data for Figure 4 & 5
data <- read.delim("CoralFISH_BarGraph.csv", sep=",",header=T)

#Subset data in regions
subbelgica <- subset(data, Region == "BMP")
subhatton <- subset(data, Region == "HB")
subrockall <- subset(data, Region == "RB")

#Belgica Mound

#Plot LepidionFA for Belgica Mound
summarybelgicaLepidionFA <- summarySE(subbelgica, measurevar="LepFA", groupvars="Coral")

plotbelgicaLepidionFA <- ggplot(summarybelgicaLepidionFA, aes(x=Coral, y=LepFA)) +
  geom_bar(colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepFA-se, ymax=LepFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.0)) +  
  xlab("") +
  ylab("") +
  ylim(0,250) +
  geom_hline(yintercept=0, width=.5)  + #add horizontal 0 line
  theme_map


#Plot SynaphobranchusFA for Belgica Mound
summarybelgicaSynaphobranchusFA <- summarySE(subbelgica, measurevar="SynFA", groupvars="Coral")

plotbelgicaSynaphobranchusFA <- ggplot(summarybelgicaSynaphobranchusFA, aes(x=Coral, y=SynFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFA-se, ymax=SynFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("") +
  ylab("") +
  ylim(0,80) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot HelicolenusFA for Belgica Mound
summarybelgicaHelicolenusFA <- summarySE(subbelgica, measurevar="HeliFA", groupvars="Coral")

plotbelgicaHelicolenusFA <- ggplot(summarybelgicaHelicolenusFA, aes(x=Coral, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  opts(title="Belgica Mound Province") +
  xlab("") +
  ylab("") +
  ylim(0,120) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot AllSpeciesFA for Belgica Mound
summarybelgicaAllSpeciesFA <- summarySE(subbelgica, measurevar="AllFA", groupvars="Coral")

plotbelgicaAllSpeciesFA <- ggplot(summarybelgicaAllSpeciesFA, aes(x=Coral, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("") +
  ylim(0,300) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(LepFA~Coral, data=subbelgica)
kruskal.test(SynFA~Coral, data=subbelgica)
kruskal.test(HeliFA~Coral, data=subbelgica)
kruskal.test(AllFA~Coral, data=subbelgica)


#Hatton Bank

#Plot LepidionFA for Hatton Bank
summaryhattonLepidionFA <- summarySE(subhatton, measurevar="LepFA", groupvars="Coral")

plothattonLepidionFA <- ggplot(summaryhattonLepidionFA, aes(x=Coral, y=LepFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepFA-se, ymax=LepFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +     
  xlab("") +
  ylab("") +
  ylim(0,250) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot SynaphobranchusFA for Hatton Bank
summaryhattonSynaphobranchusFA <- summarySE(subhatton, measurevar="SynFA", groupvars="Coral")

plothattonSynaphobranchusFA <- ggplot(summaryhattonSynaphobranchusFA, aes(x=Coral, y=SynFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFA-se, ymax=SynFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("") +
  ylab("") +
  ylim(0,80) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot HelicolenusFA for Hatton Bank
summaryhattonHelicolenusFA <- summarySE(subhatton, measurevar="HeliFA", groupvars="Coral")

plothattonHelicolenusFA <- ggplot(summaryhattonHelicolenusFA, aes(x=Coral, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  opts(title="Hatton Bank") +
  xlab("") +
  ylab("") +
  ylim(0,120) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot AllSpeciesFA for Hatton Bank
summaryhattonAllSpeciesFA <- summarySE(subhatton, measurevar="AllFA", groupvars="Coral")

plothattonAllSpeciesFA <- ggplot(summaryhattonAllSpeciesFA, aes(x=Coral, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("") +
  ylim(0,300) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(LepFA~Coral, data=subhatton)
kruskal.test(SynFA~Coral, data=subhatton)
kruskal.test(HeliFA~Coral, data=subhatton)
kruskal.test(AllFA~Coral, data=subhatton)


#Rockall Bank

#Plot LepidionFA for Rockall Bank
summaryrockallLepidionFA <- summarySE(subrockall, measurevar="LepFA", groupvars="Coral")

plotrockallLepidionFA <- ggplot(summaryrockallLepidionFA, aes(x=Coral, y=LepFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepFA-se, ymax=LepFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("")+
  ylab(expression(paste(italic("L. eques")))) +
  ylim(0,250) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot SynaphobranchusFA for Rockall Bank
summaryrockallSynaphobranchusFA <- summarySE(subrockall, measurevar="SynFA", groupvars="Coral")

plotrockallSynaphobranchusFA <- ggplot(summaryrockallSynaphobranchusFA, aes(x=Coral, y=SynFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFA-se, ymax=SynFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("") +
  ylab(expression(paste(italic("S. kaupii")))) +
  ylim(0,80) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map


#Plot HelicolenusFA for Rockall Bank
summaryrockallHelicolenusFA <- summarySE(subrockall, measurevar="HeliFA", groupvars="Coral")

plotrockallHelicolenusFA <- ggplot(summaryrockallHelicolenusFA, aes(x=Coral, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  opts(title="Rockall Bank") +
  xlab("") +
  ylab(expression(paste(italic("H. dactylopterus")))) +
  ylim(0,120) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot AllSpeciesFA for Rockall Bank
summaryrockallAllSpeciesFA <- summarySE(subrockall, measurevar="AllFA", groupvars="Coral")

plotrockallAllSpeciesFA <- ggplot(summaryrockallAllSpeciesFA, aes(x=Coral, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("Total") +
  ylim(0,300) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(LepFA~Coral, data=subrockall)
kruskal.test(SynFA~Coral, data=subrockall)
kruskal.test(HeliFA~Coral, data=subrockall)
kruskal.test(AllFA~Coral, data=subrockall)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 3)))

print(plotbelgicaHelicolenusFA, vp = vplayout(1, 3))
print(plotbelgicaLepidionFA, vp = vplayout(2, 3))
print(plotbelgicaSynaphobranchusFA, vp = vplayout(3, 3))
print(plotbelgicaAllSpeciesFA, vp = vplayout(4, 3))

print(plothattonHelicolenusFA, vp = vplayout(1, 2))
print(plothattonLepidionFA, vp = vplayout(2, 2))
print(plothattonSynaphobranchusFA, vp = vplayout(3, 2))
print(plothattonAllSpeciesFA, vp = vplayout(4, 2))

print(plotrockallHelicolenusFA, vp = vplayout(1, 1))
print(plotrockallLepidionFA, vp = vplayout(2, 1))
print(plotrockallSynaphobranchusFA, vp = vplayout(3, 1))
print(plotrockallAllSpeciesFA, vp = vplayout(4, 1))

##############################################################################

#Figure 5

#Fig. 5. Fish biomass (kg ha-1) of several species for coral and non-coral framework patches of the three study sites; 
#Error bars indicate ? 1 S.E.; * = p < 0.05 (Kruskal-Wallis Test).

#Belgica Mound

#Plot LepidionFB for Belgica Mound
summarybelgicaLepidionFB <- summarySE(subbelgica, measurevar="LepFB", groupvars="Coral")

plotbelgicaLepidionFB <- ggplot(summarybelgicaLepidionFB, aes(x=Coral, y=LepFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepFB-se, ymax=LepFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +                
  xlab("") +
  ylab("") +
  ylim(0,30) +
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot MoraFB for Belgica Mound
summarybelgicaMoraFB <- summarySE(subbelgica, measurevar="MoraFB", groupvars="Coral")

plotbelgicaMoraFB <- ggplot(summarybelgicaMoraFB, aes(x=Coral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("") +
  ylab("") +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot HelicolenusFB for Belgica Mound
summarybelgicaHelicolenusFB <- summarySE(subbelgica, measurevar="HeliFB", groupvars="Coral")

plotbelgicaHelicolenusFB <- ggplot(summarybelgicaHelicolenusFB, aes(x=Coral, y=HeliFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  opts(title="Belgica Mound Province") +
  xlab("") +
  ylab("") +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot AllSpeciesFB for Belgica Mound
summarybelgicaAllSpeciesFB <- summarySE(subbelgica, measurevar="AllFB", groupvars="Coral")

plotbelgicaAllSpeciesFB <- ggplot(summarybelgicaAllSpeciesFB, aes(x=Coral, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("") +
  ylim(0,200) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("Absent","Present")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(LepFB~Coral, data=subbelgica)
kruskal.test(MoraFB~Coral, data=subbelgica)
kruskal.test(HeliFB~Coral, data=subbelgica)
kruskal.test(AllFB~Coral, data=subbelgica)


#Hatton Bank

#Plot LepidionFB for Hatton Bank
summaryhattonLepidionFB <- summarySE(subhatton, measurevar="LepFB", groupvars="Coral")

plothattonLepidionFB <- ggplot(summaryhattonLepidionFB, aes(x=Coral, y=LepFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepFB-se, ymax=LepFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +               
  xlab("") +
  ylab("") +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot MoraFA for Hatton Bank
summaryhattonMoraFB <- summarySE(subhatton, measurevar="MoraFB", groupvars="Coral")

plothattonMoraFB <- ggplot(summaryhattonMoraFB, aes(x=Coral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("") +
  ylab("") +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot HelicolenusFB for Hatton Bank
summaryhattonHelicolenusFB <- summarySE(subhatton, measurevar="HeliFB", groupvars="Coral")

plothattonHelicolenusFB <- ggplot(summaryhattonHelicolenusFB, aes(x=Coral, y=HeliFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  opts(title="Hatton Bank") +
  xlab("") +
  ylab("") +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot AllSpeciesFB for Hatton Bank
summaryhattonAllSpeciesFB <- summarySE(subhatton, measurevar="AllFB", groupvars="Coral")

plothattonAllSpeciesFB <- ggplot(summaryhattonAllSpeciesFB, aes(x=Coral, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("") +
  ylim(0,200) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(LepFB~Coral, data=subhatton)
kruskal.test(MoraFB~Coral, data=subhatton)
kruskal.test(HeliFB~Coral, data=subhatton)
kruskal.test(AllFB~Coral, data=subhatton)


#Rockall Bank

#Plot LepidionFB for Rockall Bank
summaryrockallLepidionFB <- summarySE(subrockall, measurevar="LepFB", groupvars="Coral")

plotrockallLepidionFB <- ggplot(summaryrockallLepidionFB, aes(x=Coral, y=LepFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepFB-se, ymax=LepFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("")+
  ylab(expression(paste(italic("L. eques")))) +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot MoraFB for Rockall Bank
summaryrockallMoraFB <- summarySE(subrockall, measurevar="MoraFB", groupvars="Coral")

plotrockallMoraFB <- ggplot(summaryrockallMoraFB, aes(x=Coral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("") +
  ylab(expression(paste(italic("M. moro")))) +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot HelicolenusFB for Rockall Bank
summaryrockallHelicolenusFB <- summarySE(subrockall, measurevar="HeliFB", groupvars="Coral")

plotrockallHelicolenusFB <- ggplot(summaryrockallHelicolenusFB, aes(x=Coral, y=HeliFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  opts(title="Rockall Bank") +
  xlab("") +
  ylab(expression(paste(italic("H. dactylopterus")))) +
  ylim(0,30) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

#Plot AllSpeciesFB for Rockall Bank
summaryrockallAllSpeciesFB <- summarySE(subrockall, measurevar="AllFB", groupvars="Coral")

plotrockallAllSpeciesFB <- ggplot(summaryrockallAllSpeciesFB, aes(x=Coral, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Coral framework") +
  ylab("Total") +
  ylim(0,200) +
  #scale_y_continuous(breaks=-1:20000*1000) +
  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map

# Kruskal-Wallis Test
kruskal.test(LepFB~Coral, data=subrockall)
kruskal.test(MoraFB~Coral, data=subrockall)
kruskal.test(HeliFB~Coral, data=subrockall)
kruskal.test(AllFB~Coral, data=subrockall)


# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 3)))

print(plotbelgicaHelicolenusFB, vp = vplayout(1, 3))
print(plotbelgicaLepidionFB, vp = vplayout(2, 3))
print(plotbelgicaMoraFB, vp = vplayout(3, 3))
print(plotbelgicaAllSpeciesFB, vp = vplayout(4, 3))

print(plothattonHelicolenusFB, vp = vplayout(1, 2))
print(plothattonLepidionFB, vp = vplayout(2, 2))
print(plothattonMoraFB, vp = vplayout(3, 2))
print(plothattonAllSpeciesFB, vp = vplayout(4, 2))

print(plotrockallHelicolenusFB, vp = vplayout(1, 1))
print(plotrockallLepidionFB, vp = vplayout(2, 1))
print(plotrockallMoraFB, vp = vplayout(3, 1))
print(plotrockallAllSpeciesFB, vp = vplayout(4, 1))

##############################################################################

#Table 3

#Table 3. Variance inflation factor (VIF) values for all terrain variables and VIF values for
#remaining variables after co-linearity has been removed by sequentially deleting the highest
#VIF value until all VIFs were below 5. Terrain variables were obtained from 1000 random
#points of each study site extracted from the multibeam data. The meaning of different
#variables is explained in the text.

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

##############################################################################

#Table 4

#Table 4. Model components of the different models compared.
#The logarithm of the study area was used as offset for all fitted models.

#See Excel File with Tables

##############################################################################

#Table 5

#Table 5. Fish abundance for each fish species (ind. ha-1) at the three study sites. 
#Mean ? S.E. and range in (parentheses).

VS_perStation <- read.delim("VS_perStation.csv", sep=",", header=T)

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

VS_perStation$ALLFA <- VS_perStation$ALLNO/VS_perStation$Area_ha
summaryALLFA <- summarySE(VS_perStation, measurevar="ALLFA", groupvars="Region")

##############################################################################

#Table 6

#Table 6. Fish biomass for all fish species (kg ha-1), where length-weight relationship available, at the three study sites. 
#Mean ? S.E. and range in (parentheses).

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

VS_perStation$ALLFB <- VS_perStation$ALLWEIGHT/VS_perStation$Area_ha
summaryALLFB <- summarySE(VS_perStation, measurevar="ALLFB", groupvars="Region")

##############################################################################

#Table 7

#Table 7. Absence and presence of all fish species within coral and non-coral framework patches at the three study sites.

#See Excel File with Tables

##############################################################################

#Table 8

#Table 8. Comparison of all estimated distributions from the full model (M15). 
#Dispersion parameters and AIC values.

#Import data
HB_1min <- read.delim("CoralFish_1_min_Start_27N.csv", sep=",", header=T)
RB_1min <- read.delim("CoralFish_1_min_Start_28N.csv", sep=",", header=T)
BM_1min <- read.delim("CoralFish_1_min_Start_29N.csv", sep=",", header=T)

HB_5min <- read.delim("CoralFish_5_min_Start_27N.csv", sep=",", header=T)
RB_5min <- read.delim("CoralFish_5_min_Start_28N.csv", sep=",", header=T)
BM_5min <- read.delim("CoralFish_5_min_Start_29N.csv", sep=",", header=T)

HB_10min <- read.delim("CoralFish_10_min_Start_27N.csv", sep=",", header=T)
RB_10min <- read.delim("CoralFish_10_min_Start_28N.csv", sep=",", header=T)
BM_10min <- read.delim("CoralFish_10_min_Start_29N.csv", sep=",", header=T)

#################################################

#Plot of the frequencies for the response variable.

par(mfrow = c(1,3), mar = c(4,4,4,1))

plot(table(RB_1min$ALLNO),ylab="Frequency", xlab="Observed values", type = "h", main = "HB")
plot(table(HB_1min$ALLNO),ylab="Frequency", xlab="Observed values", type = "h", main = "RB")
plot(table(BM_1min$ALLNO),ylab="Frequency", xlab="Observed values", type = "h", main = "BM")


par(mfrow = c(3,3), mar = c(4,4,2,1))

hist(RB_1min$HELILENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
hist(HB_1min$HELILENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
hist(BM_1min$HELILENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))

hist(RB_1min$LEPLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))
hist(HB_1min$LEPLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))
hist(BM_1min$LEPLENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))

hist(RB_1min$MORALENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))
hist(HB_1min$MORALENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))
hist(BM_1min$MORALENGTH, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))

par(mfrow = c(3,3), mar = c(4,4,2,1))

hist(RB_1min$HELIWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
hist(HB_1min$HELIWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))
hist(BM_1min$HELIWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Helicolenus dactylopterus"))))

hist(RB_1min$LEPWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))
hist(HB_1min$LEPWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))
hist(BM_1min$LEPWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Lepidion eques"))))

hist(RB_1min$MORAWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))
hist(HB_1min$MORAWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))
hist(BM_1min$MORAWEIGHT, breaks = 20, ylab = "Frequency", xlab="Total length (cm)", main=expression(paste(italic("Mora moro"))))


#################################################

RB_1min$lArea <- log(RB_1min$SURVEYAREA_M)
HB_1min$lArea <- log(HB_1min$SURVEYAREA_M)
BM_1min$lArea <- log(BM_1min$SURVEYAREA_M)

RB_5min$lArea <- log(RB_5min$SURVEYAREA_M)
HB_5min$lArea <- log(HB_5min$SURVEYAREA_M)
BM_5min$lArea <- log(BM_5min$SURVEYAREA_M)

RB_10min$lArea <- log(RB_10min$SURVEYAREA_M)
HB_10min$lArea <- log(HB_10min$SURVEYAREA_M)
BM_10min$lArea <- log(BM_10min$SURVEYAREA_M)

#Save to file
write.csv(HB_1min, file = "CoralFish_1_min_Start_27N.csv")
write.csv(RB_1min, file = "CoralFish_1_min_Start_28N.csv")
write.csv(BM_1min, file = "CoralFish_1_min_Start_29N.csv")

write.csv(HB_5min, file = "CoralFish_5_min_Start_27N_lA.csv")
write.csv(RB_5min, file = "CoralFish_5_min_Start_28N_lA.csv")
write.csv(BM_5min, file = "CoralFish_5_min_Start_29N_lA.csv")

write.csv(HB_10min, file = "CoralFish_10_min_Start_27N_lA.csv")
write.csv(RB_10min, file = "CoralFish_10_min_Start_28N_lA.csv")
write.csv(BM_10min, file = "CoralFish_10_min_Start_29N_lA.csv")

#################################################

#1 min

HB_1min<-data.frame(ALLNO = c(HB_1min$ALLNO), CFW = c(HB_1min$CFW), Depth = c(HB_1min$Depth), Rugosity = c(HB_1min$Rugosity),
                    BPI_Broad = c(HB_1min$BPI_Broad), Tang_Curve = c(HB_1min$Tang_Curve), Northings = c(HB_1min$Northings), 
                    Eastings = c(HB_1min$Eastings), lArea = c(HB_1min$lArea))

RB_1min<-data.frame(ALLNO = c(RB_1min$ALLNO), CFW = c(RB_1min$CFW), Depth = c(RB_1min$Depth), Rugosity = c(RB_1min$Rugosity),
                    BPI_Broad = c(RB_1min$BPI_Broad), Tang_Curve = c(RB_1min$Tang_Curve), Northings = c(RB_1min$Northings), 
                    Eastings = c(RB_1min$Eastings), lArea = c(RB_1min$lArea))

BM_1min<-data.frame(ALLNO = c(BM_1min$ALLNO), CFW = c(BM_1min$CFW), Depth = c(BM_1min$Depth), Rugosity = c(BM_1min$Rugosity),
                    BPI_Broad = c(BM_1min$BPI_Broad), Tang_Curve = c(BM_1min$Tang_Curve), Northings = c(BM_1min$Northings), 
                    Eastings = c(BM_1min$Eastings), lArea = c(BM_1min$lArea))

#5 min

HB_5min<-data.frame(ALLNO = c(HB_5min$ALLNO), CFW = c(HB_5min$CFW), Depth = c(HB_5min$Depth), Rugosity = c(HB_5min$Rugosity),
                    BPI_Broad = c(HB_5min$BPI_Broad), Tang_Curve = c(HB_5min$Tang_Curve), Northings = c(HB_5min$Northings), 
                    Eastings = c(HB_5min$Eastings), lArea = c(HB_5min$lArea))

RB_5min<-data.frame(ALLNO = c(RB_5min$ALLNO), CFW = c(RB_5min$CFW), Depth = c(RB_5min$Depth), Rugosity = c(RB_5min$Rugosity),
                    BPI_Broad = c(RB_5min$BPI_Broad), Tang_Curve = c(RB_5min$Tang_Curve), Northings = c(RB_5min$Northings), 
                    Eastings = c(RB_5min$Eastings), lArea = c(RB_5min$lArea))

BM_5min<-data.frame(ALLNO = c(BM_5min$ALLNO), CFW = c(BM_5min$CFW), Depth = c(BM_5min$Depth), Rugosity = c(BM_5min$Rugosity),
                    BPI_Broad = c(BM_5min$BPI_Broad), Tang_Curve = c(BM_5min$Tang_Curve), Northings = c(BM_5min$Northings), 
                    Eastings = c(BM_5min$Eastings), lArea = c(BM_5min$lArea))

#10 min

HB_10min<-data.frame(ALLNO = c(HB_10min$ALLNO), CFW = c(HB_10min$CFW), Depth = c(HB_10min$Depth), Rugosity = c(HB_10min$Rugosity),
                     BPI_Broad = c(HB_10min$BPI_Broad), Tang_Curve = c(HB_10min$Tang_Curve), Northings = c(HB_10min$Northings), 
                     Eastings = c(HB_10min$Eastings), lArea = c(HB_10min$lArea))

RB_10min<-data.frame(ALLNO = c(RB_10min$ALLNO), CFW = c(RB_10min$CFW), Depth = c(RB_10min$Depth), Rugosity = c(RB_10min$Rugosity),
                     BPI_Broad = c(RB_10min$BPI_Broad), Tang_Curve = c(RB_10min$Tang_Curve), Northings = c(RB_10min$Northings), 
                     Eastings = c(RB_10min$Eastings), lArea = c(RB_10min$lArea))

BM_10min<-data.frame(ALLNO = c(BM_10min$ALLNO), CFW = c(BM_10min$CFW), Depth = c(BM_10min$Depth), Rugosity = c(BM_10min$Rugosity),
                     BPI_Broad = c(BM_10min$BPI_Broad), Tang_Curve = c(BM_10min$Tang_Curve), Northings = c(BM_10min$Northings), 
                     Eastings = c(BM_10min$Eastings), lArea = c(BM_10min$lArea))

#Removes the missing values (which are present in the response variable)
RB_NA <- is.na(RB_1min$Depth) | is.na(RB_1min$CFW) | is.na(RB_1min$Rugosity) | is.na(RB_1min$BPI_Broad) | is.na(RB_1min$Tang_Curve) | is.na(RB_1min$Northings) | is.na(RB_1min$Eastings)
RB_1min <- RB_1min[!RB_NA,]
HB_NA <- is.na(HB_1min$Depth) | is.na(HB_1min$CFW) | is.na(HB_1min$Rugosity) | is.na(HB_1min$BPI_Broad) | is.na(HB_1min$Tang_Curve) | is.na(HB_1min$Northings) | is.na(HB_1min$Eastings)
HB_1min <- HB_1min[!HB_NA,]
BM_NA <- is.na(BM_1min$Depth) | is.na(BM_1min$CFW) | is.na(BM_1min$Rugosity) | is.na(BM_1min$BPI_Broad) | is.na(BM_1min$Tang_Curve) | is.na(BM_1min$Northings) | is.na(BM_1min$Eastings)
BM_1min <- BM_1min[!BM_NA,]

RB_NA5 <- is.na(RB_1min$Depth) | is.na(RB_1min$CFW) | is.na(RB_1min$Rugosity) | is.na(RB_1min$BPI_Broad) | is.na(RB_1min$Tang_Curve) | is.na(RB_1min$Northings) | is.na(RB_1min$Eastings)
RB_1min <- RB_1min[!RB_NA5,]
HB_NA5 <- is.na(HB_1min$Depth) | is.na(HB_1min$CFW) | is.na(HB_1min$Rugosity) | is.na(HB_1min$BPI_Broad) | is.na(HB_1min$Tang_Curve) | is.na(HB_1min$Northings) | is.na(HB_1min$Eastings)
HB_1min <- HB_1min[!HB_NA5,]
BM_NA5 <- is.na(BM_1min$Depth) | is.na(BM_1min$CFW) | is.na(BM_1min$Rugosity) | is.na(BM_1min$BPI_Broad) | is.na(BM_1min$Tang_Curve) | is.na(BM_1min$Northings) | is.na(BM_1min$Eastings)
BM_1min <- BM_1min[!BM_NA5,]

RB_NA10 <- is.na(RB_1min$Depth) | is.na(RB_1min$CFW) | is.na(RB_1min$Rugosity) | is.na(RB_1min$BPI_Broad) | is.na(RB_1min$Tang_Curve) | is.na(RB_1min$Northings) | is.na(RB_1min$Eastings)
RB_1min <- RB_1min[!RB_NA10,]
HB_NA10 <- is.na(HB_1min$Depth) | is.na(HB_1min$CFW) | is.na(HB_1min$Rugosity) | is.na(HB_1min$BPI_Broad) | is.na(HB_1min$Tang_Curve) | is.na(HB_1min$Northings) | is.na(HB_1min$Eastings)
HB_1min <- HB_1min[!HB_NA10,]
BM_NA10 <- is.na(BM_1min$Depth) | is.na(BM_1min$CFW) | is.na(BM_1min$Rugosity) | is.na(BM_1min$BPI_Broad) | is.na(BM_1min$Tang_Curve) | is.na(BM_1min$Northings) | is.na(BM_1min$Eastings)
BM_1min <- BM_1min[!BM_NA10,]

#################################################

#Poisson GLM

GLM1_RB <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = RB_1min)
GLM1_HB <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = HB_1min)
GLM1_BM <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = BM_1min)

GLM1_RB_5min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = RB_5min)
GLM1_HB_5min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = HB_5min)
GLM1_BM_5min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = BM_5min)

GLM1_RB_10min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = RB_10min)
GLM1_HB_10min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = HB_10min)
GLM1_BM_10min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = poisson, offset(lArea), data = BM_10min)

#################################################

#Negative binomial GLM

library(MASS)

GLM2_RB <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = RB_1min, offset(lArea), link = log)
GLM2_HB <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = HB_1min, offset(lArea), link = log)
#The fit did not meet the default convergence limits
GLM2_BM <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = BM_1min, offset(lArea), link = log)

GLM2_RB_5min <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = RB_5min, offset(lArea), link = log)
GLM2_HB_5min <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = HB_5min, offset(lArea), link = log)
GLM2_BM_5min <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = BM_5min, offset(lArea), link = log)

GLM2_RB_10min <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = RB_10min, offset(lArea), link = log)
GLM2_HB_10min <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = HB_10min, offset(lArea), link = log)
GLM2_BM_10min <- glm.nb(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, data = BM_10min, offset(lArea), link = log)

#################################################

#Quassi-Poisson GLM
GLM3_RB <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = RB_1min)
GLM3_HB <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = HB_1min)
GLM3_BM <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = BM_1min)

GLM3_RB_5min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = RB_5min)
GLM3_HB_5min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = HB_5min)
GLM3_BM_5min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = BM_5min)

GLM3_RB_10min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = RB_10min)
GLM3_HB_10min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = HB_10min)
GLM3_BM_10min <- glm(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings, family = quasipoisson, offset(lArea), data = BM_10min)

#################################################

#Poisson GAM

library(mgcv)

GAM1_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = RB_1min)
GAM1_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = HB_1min)
GAM1_BM <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = BM_1min)

GAM1_RB_5min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = RB_5min)
GAM1_HB_5min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = HB_5min)
GAM1_BM_5min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = BM_5min)

GAM1_RB_10min <- gam(ALLNO ~ s(Depth, k=7) + s(CFW, k=7) + s(Rugosity, k=7) + s(BPI_Broad, k=7) + s(Tang_Curve, k=7) + s(Northings, k=7) + s(Eastings, k=7), offset(lArea), family = poisson, data = RB_10min)
#Model has more coefficients than data => k=7
GAM1_HB_10min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = HB_10min)
GAM1_BM_10min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = poisson, data = BM_10min)

#################################################

#Negative binomial GAM

GAM2_RB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = RB_1min)
GAM2_HB <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = HB_1min)
GAM2_BM <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = BM_1min)

GAM2_RB_5min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = RB_5min)
GAM2_HB_5min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = HB_5min)
GAM2_BM_5min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = BM_5min)

GAM2_RB_10min <- gam(ALLNO ~ s(Depth, k=7) + s(CFW, k=7) + s(Rugosity, k=7) + s(BPI_Broad, k=7) + s(Tang_Curve, k=7) + s(Northings, k=7) + s(Eastings, k=7), offset(lArea), family = negbin(c(0.1, 10)), data = RB_10min)
#Model has more coefficients than data =< k=7
GAM2_HB_10min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = HB_10min)
GAM2_BM_10min <- gam(ALLNO ~ s(Depth) + s(CFW) + s(Rugosity) + s(BPI_Broad) + s(Tang_Curve) + s(Northings) + s(Eastings), offset(lArea), family = negbin(c(0.1, 10)), data = BM_10min)

#################################################

#Zero-inflated Poisson GLM

library(pscl)

ZIP1_RB <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist = "poisson", link = "logit", data = RB_1min)
ZIP1_HB <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist = "poisson", link = "logit", data = HB_1min)
ZIP1_BM <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist = "poisson", link = "logit", data = BM_1min)

ZIP1_BM_5min <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist = "poisson", link = "logit", data = BM_5min)
#Error message, but model produced (likely to be perfect fit)

#################################################   

#Zero-inflated NB GLM

ZINB1_RB <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist ="negbin", link = "logit", data = RB_1min)
ZINB1_HB <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist = "negbin", link = "logit", data = HB_1min)
#Warning message: NaNs produced
ZINB1_BM <- zeroinfl(ALLNO ~ Depth + CFW + Rugosity + BPI_Broad + Tang_Curve + Northings + Eastings + offset(lArea), dist = "negbin", link = "logit", data = BM_1min)
#Warning message: NaNs produced

#################################################
#Table 8 

#1 min

RB_1min_T <- cbind(c(deviance(GLM1_RB), deviance(GLM2_RB), deviance(GLM3_RB), deviance(GAM1_RB), deviance(GAM2_RB), deviance(ZIP1_RB), deviance(ZINB1_RB)),
                   c(deviance(GLM1_RB)/GLM1_RB$df.res, deviance(GLM2_RB)/GLM2_RB$df.res, deviance(GLM3_RB)/GLM3_RB$df.res, deviance(GAM1_RB)/GAM1_RB$df.res, deviance(GAM2_RB)/GAM2_RB$df.res, 
                     sum(resid(ZIP1_RB, type="pearson")^2) / ZIP1_RB$df.res, sum(resid(ZINB1_RB, type="pearson")^2) / ZINB1_RB$df.res),
                   c(AIC(GLM1_RB), AIC(GLM2_RB), AIC(GLM3_RB), AIC(GAM1_RB), AIC(GAM2_RB), AIC(ZIP1_RB), AIC(ZINB1_RB)), 
                   c(logLik(GLM1_RB), logLik(GLM2_RB), logLik(GLM3_RB), logLik(GAM1_RB), logLik(GAM2_RB), logLik(ZIP1_RB), logLik(ZINB1_RB)))
colnames(RB_1min_T) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(RB_1min_T) <- c("Poisson GLM", "NB GLM", "Quasi-Poisson GLM", "Poisson GAM", "NB GAM", "ZIP GLM", "ZINB GLM")
RB_1min_T

HB_1min_T <- cbind(c(deviance(GLM1_HB), deviance(GLM2_HB), deviance(GLM3_HB), deviance(GAM1_HB), deviance(GAM2_HB), deviance(ZIP1_HB), deviance(ZINB1_HB)),
                   c(deviance(GLM1_HB)/GLM1_HB$df.res, deviance(GLM2_HB)/GLM2_HB$df.res, deviance(GLM3_HB)/GLM3_HB$df.res, deviance(GAM1_HB)/GAM1_HB$df.res, deviance(GAM2_HB)/GAM2_HB$df.res, 
                     sum(resid(ZIP1_HB, type="pearson")^2) / ZIP1_HB$df.res, sum(resid(ZINB1_HB, type="pearson")^2) / ZINB1_HB$df.res),
                   c(AIC(GLM1_HB), AIC(GLM2_HB), AIC(GLM3_HB), AIC(GAM1_HB), AIC(GAM2_HB), AIC(ZIP1_HB), AIC(ZINB1_HB)), 
                   c(logLik(GLM1_HB), logLik(GLM2_HB), logLik(GLM3_HB), logLik(GAM1_HB), logLik(GAM2_HB), logLik(ZIP1_HB), logLik(ZINB1_HB)))
colnames(HB_1min_T) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(HB_1min_T) <- c("Poisson GLM", "NB GLM", "Quasi-Poisson GLM", "Poisson GAM", "NB GAM", "ZIP GLM", "ZINB GLM")
HB_1min_T

BM_1min_T <- cbind(c(deviance(GLM1_BM), deviance(GLM2_BM), deviance(GLM3_BM), deviance(GAM1_BM), deviance(GAM2_BM), deviance(ZIP1_BM), deviance(ZINB1_BM)),
                   c(deviance(GLM1_BM)/GLM1_BM$df.res, deviance(GLM2_BM)/GLM2_BM$df.res, deviance(GLM3_BM)/GLM3_BM$df.res, deviance(GAM1_BM)/GAM1_BM$df.res, deviance(GAM2_BM)/GAM2_BM$df.res, 
                     sum(resid(ZIP1_BM, type="pearson")^2) / ZIP1_BM$df.res, sum(resid(ZINB1_BM, type="pearson")^2) / ZINB1_BM$df.res),
                   c(AIC(GLM1_BM), AIC(GLM2_BM), AIC(GLM3_BM), AIC(GAM1_BM), AIC(GAM2_BM), AIC(ZIP1_BM), AIC(ZINB1_BM)), 
                   c(logLik(GLM1_BM), logLik(GLM2_BM), logLik(GLM3_BM), logLik(GAM1_BM), logLik(GAM2_BM), logLik(ZIP1_BM), logLik(ZINB1_BM)))
colnames(BM_1min_T) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(BM_1min_T) <- c("Poisson GLM", "NB GLM", "Quasi-Poisson GLM", "Poisson GAM", "NB GAM", "ZIP GLM", "ZINB GLM")
BM_1min_T

#5 min

RB_5min <- cbind(c(deviance(GLM1_RB_5min), deviance(GLM2_RB_5min), deviance(GLM3_RB_5min), deviance(GAM1_RB_5min), deviance(GAM2_RB_5min)),
                 c(deviance(GLM1_RB_5min)/GLM1_RB_5min$df.res, deviance(GLM2_RB_5min)/GLM2_RB_5min$df.res, deviance(GLM3_RB_5min)/GLM3_RB_5min$df.res,deviance(GAM1_RB_5min)/GAM1_RB_5min$df.res, deviance(GAM2_RB)/GAM2_RB$df.res),
                 c(AIC(GLM1_RB_5min), AIC(GLM2_RB_5min), AIC(GLM3_RB_5min), AIC(GAM1_RB_5min), AIC(GAM2_RB_5min)), 
                 c(logLik(GLM1_RB_5min), logLik(GLM2_RB_5min), logLik(GLM3_RB_5min), logLik(GAM1_RB_5min), logLik(GAM2_RB_5min)))
colnames(RB_5min) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(RB_5min) <- c("Poisson GLM", "NB GLM", "Quasi-Poisson GLM", "Poisson GAM", "NB GAM")
RB_5min

HB_5min <- cbind(c(deviance(GLM1_HB_5min), deviance(GLM2_HB_5min), deviance(GAM1_HB_5min), deviance(GAM2_HB_5min)),
                 c(deviance(GLM1_HB_5min)/GLM1_HB_5min$df.res, deviance(GLM2_HB_5min)/GLM2_HB_5min$df.res, deviance(GAM1_HB_5min)/GAM1_HB_5min$df.res, deviance(GAM2_HB)/GAM2_HB$df.res),
                 c(AIC(GLM1_HB_5min), AIC(GLM2_HB_5min), AIC(GAM1_HB_5min), AIC(GAM2_HB_5min)), 
                 c(logLik(GLM1_HB_5min), logLik(GLM2_HB_5min), logLik(GAM1_HB_5min), logLik(GAM2_HB_5min)))
colnames(HB_5min) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(HB_5min) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM")
HB_5min

BM_5min <- cbind(c(deviance(GLM1_BM_5min), deviance(GLM2_BM_5min), deviance(GAM1_BM_5min), deviance(GAM2_BM_5min), deviance(ZIP1_BM_5min)),
                 c(deviance(GLM1_BM_5min)/GLM1_BM_5min$df.res, deviance(GLM2_BM_5min)/GLM2_BM_5min$df.res, deviance(GAM1_BM_5min)/GAM1_BM_5min$df.res, deviance(GAM2_BM_5min)/GAM2_BM_5min$df.res, 
                   sum(resid(ZIP1_BM_5min, type="pearson")^2) / ZIP1_BM_5min$df.res),
                 c(AIC(GLM1_BM_5min), AIC(GLM2_BM_5min), AIC(GAM1_BM_5min), AIC(GAM2_BM_5min), AIC(ZIP1_BM_5min)), 
                 c(logLik(GLM1_BM_5min), logLik(GLM2_BM_5min), logLik(GAM1_BM_5min), logLik(GAM2_BM_5min), logLik(ZIP1_BM_5min)))
colnames(BM_5min) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(BM_5min) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM", "ZIP GLM")
BM_5min

#10 min

RB_10min <- cbind(c(deviance(GLM1_RB_10min), deviance(GLM2_RB_10min), deviance(GAM1_RB_10min), deviance(GAM2_RB_10min)),
                  c(deviance(GLM1_RB_10min)/GLM1_RB_10min$df.res, deviance(GLM2_RB_10min)/GLM2_RB_10min$df.res, deviance(GAM1_RB_10min)/GAM1_RB_10min$df.res, deviance(GAM2_RB_10min)/GAM2_RB_10min$df.res),
                  c(AIC(GLM1_RB_10min), AIC(GLM2_RB_10min), AIC(GAM1_RB_10min), AIC(GAM2_RB_10min)), 
                  c(logLik(GLM1_RB_10min), logLik(GLM2_RB_10min), logLik(GAM1_RB_10min), logLik(GAM2_RB_10min)))
colnames(RB_10min) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(RB_10min) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM")
RB_10min

HB_10min <- cbind(c(deviance(GLM1_HB_10min), deviance(GLM2_HB_10min), deviance(GAM1_HB_10min), deviance(GAM2_HB_10min)),
                  c(deviance(GLM1_HB_10min)/GLM1_HB_10min$df.res, deviance(GLM2_HB_10min)/GLM2_HB_10min$df.res, deviance(GAM1_HB_10min)/GAM1_HB_10min$df.res, deviance(GAM2_HB_10min)/GAM2_HB_10min$df.res),
                  c(AIC(GLM1_HB_10min), AIC(GLM2_HB_10min), AIC(GAM1_HB_10min), AIC(GAM2_HB_10min)), 
                  c(logLik(GLM1_HB_10min), logLik(GLM2_HB_10min), logLik(GAM1_HB_10min), logLik(GAM2_HB_10min)))
colnames(HB_10min) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(HB_10min) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM")
HB_10min

BM_10min <- cbind(c(deviance(GLM1_BM_10min), deviance(GLM2_BM_10min), deviance(GAM1_BM_10min), deviance(GAM2_BM_10min)),
                  c(deviance(GLM1_BM_10min)/GLM1_BM_10min$df.res, deviance(GLM2_BM_10min)/GLM2_BM_10min$df.res, deviance(GAM1_BM_10min)/GAM1_BM_10min$df.res, deviance(GAM2_BM_10min)/GAM2_BM_10min$df.res),
                  c(AIC(GLM1_BM_10min), AIC(GLM2_BM_10min), AIC(GAM1_BM_10min), AIC(GAM2_BM_10min)), 
                  c(logLik(GLM1_BM_10min), logLik(GLM2_BM_10min), logLik(GAM1_BM_10min), logLik(GAM2_BM_10min)))
colnames(BM_10min) <- c("Deviance", "Dispersion", "AIC", "LogLik")
rownames(BM_10min) <- c("Poisson GLM", "NB GLM", "Poisson GAM", "NB GAM")
BM_10min

##############################################################################