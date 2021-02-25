library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
#setwd("C:/Users/MBiber/Documents/Analysis_R")

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data <- read.delim("VideoData_FA_FB_perStation.csv", sep=",",header=T)

#Create a new theme just for Andy... White with black lines, no bar fill, font sizes set
theme_update( panel.grid.minor= theme_blank(),
              panel.grid.major= theme_blank(),
              panel.background= theme_blank(),
              axis.title.x= theme_text( family= "serif", angle= 0 , size= 12),
              axis.text.x= theme_text( family= "serif", angle= 0 , size= 11),
              axis.text.y= theme_text( family= "serif" , size= 11),
              axis.title.y= theme_text( family= "serif", angle= 90 , size= 12))
theme_map <- theme_get()
theme_set( theme_bw())

# Belgica Mound

#Subset data for Belgica Mound
subbelgica <- subset(data, Region == "Belgica Mound")

#Plot LepidionFA for Belgica Mound
summarybelgicaLepidionFA <- summarySE(subbelgica, measurevar="LepiFA", groupvars="Category")

plotbelgicaLepidionFA <- ggplot(summarybelgicaLepidionFA, aes(x=Category, y=LepiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFA-se, ymax=LepiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +                
                  xlab("") +
                  ylab(expression(paste(italic("Lepidion eques")))) +
                  ylim(0,250) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=subbelgica)

#Plot SynaphobranchusFA for Belgica Mound
summarybelgicaSynaphobranchusFA <- summarySE(subbelgica, measurevar="SynFA", groupvars="Category")

plotbelgicaSynaphobranchusFA <- ggplot(summarybelgicaSynaphobranchusFA, aes(x=Category, y=SynFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFA-se, ymax=SynFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Synaphobranchus kaupii")))) +
                  ylim(0,80) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=subbelgica)


#Plot SigmopsFA for Belgica Mound
summarybelgicaSigmopsFA <- summarySE(subbelgica, measurevar="SigFA", groupvars="Category")

plotbelgicaSigmopsFA <- ggplot(summarybelgicaSigmopsFA, aes(x=Category, y=SigFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SigFA-se, ymax=SigFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Sigmops bathyphilus")))) +
                  ylim(0,40) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=subbelgica)

#Plot MoraFA for Belgica Mound
summarybelgicaMoraFA <- summarySE(subbelgica, measurevar="MoraFA", groupvars="Category")

plotbelgicaMoraFA <- ggplot(summarybelgicaMoraFA, aes(x=Category, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Mora moro")))) +
                  ylim(0,20) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=subbelgica)

#Plot ChimaeraFA for Belgica Mound
summarybelgicaChimaeraFA <- summarySE(subbelgica, measurevar="ChimFA", groupvars="Category")

plotbelgicaChimaeraFA <- ggplot(summarybelgicaChimaeraFA, aes(x=Category, y=ChimFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFA-se, ymax=ChimFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Chimaera monstrosa")))) +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=subbelgica)

#Plot HelicolenusFA for Belgica Mound
summarybelgicaHelicolenusFA <- summarySE(subbelgica, measurevar="HeliFA", groupvars="Category")

plotbelgicaHelicolenusFA <- ggplot(summarybelgicaHelicolenusFA, aes(x=Category, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Helicolenus dactylopterus")))) +
                  ylim(0,100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=subbelgica)

#Plot LophiusFA for Belgica Mound
summarybelgicaLophiusFA <- summarySE(subbelgica, measurevar="LophiFA", groupvars="Category")

plotbelgicaLophiusFA <- ggplot(summarybelgicaLophiusFA, aes(x=Category, y=LophiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFA-se, ymax=LophiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=subbelgica)

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFA <- summarySE(subbelgica, measurevar="MolvaFA", groupvars="Category")

plotbelgicaMolvaFA <- ggplot(summarybelgicaMolvaFA, aes(x=Category, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=subbelgica)

#Plot BrosmeFA for Belgica Mound
summarybelgicaBrosmeFA <- summarySE(subbelgica, measurevar="BrosmeFA", groupvars="Category")

plotbelgicaBrosmeFA <- ggplot(summarybelgicaBrosmeFA, aes(x=Category, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Brosme brosme")))) +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=subbelgica)

#Plot HoplostethusFA for Belgica Mound
summarybelgicaHoplostethusFA <- summarySE(subbelgica, measurevar="HoploFA", groupvars="Category")

plotbelgicaHoplostethusFA <- ggplot(summarybelgicaHoplostethusFA, aes(x=Category, y=HoploFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFA-se, ymax=HoploFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Hoplostethus atlanticus")))) +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFA~Category, data=subbelgica)

#Plot PhycisFA for Belgica Mound
summarybelgicaPhycisFA <- summarySE(subbelgica, measurevar="PhycisFA", groupvars="Category")

plotbelgicaPhycisFA <- ggplot(summarybelgicaPhycisFA, aes(x=Category, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Phycis phycis")))) +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFA~Category, data=subbelgica)

#Plot PseudotriakisFA for Belgica Mound
summarybelgicaPseudotriakisFA <- summarySE(subbelgica, measurevar="PseudoFA", groupvars="Category")

plotbelgicaPseudotriakisFA <- ggplot(summarybelgicaPseudotriakisFA, aes(x=Category, y=PseudoFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudoFA-se, ymax=PseudoFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Pseudotriakis microdon")))) +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PseudoFA~Category, data=subbelgica)

#Plot RajaFA for Belgica Mound
summarybelgicaRajaFA <- summarySE(subbelgica, measurevar="RajaFA", groupvars="Category")

plotbelgicaRajaFA <- ggplot(summarybelgicaRajaFA, aes(x=Category, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Raja fyllae")))) +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(RajaFA~Category, data=subbelgica)

#Plot AllSpeciesFA for Belgica Mound
summarybelgicaAllSpeciesFA <- summarySE(subbelgica, measurevar="AllFA", groupvars="Category")

plotbelgicaAllSpeciesFA <- ggplot(summarybelgicaAllSpeciesFA, aes(x=Category, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Total")))) +
                  ylim(0,300) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFA~Category, data=subbelgica)


#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Region == "Hatton Bank")

#Plot LepidionFA for Hatton Bank
summaryhattonLepidionFA <- summarySE(subhatton, measurevar="LepiFA", groupvars="Category")

plothattonLepidionFA <- ggplot(summaryhattonLepidionFA, aes(x=Category, y=LepiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFA-se, ymax=LepiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +               
                  xlab("") +
                  ylab("") +
                  ylim(0,250) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=subhatton)

#Plot SynaphobranchusFA for Hatton Bank
summaryhattonSynaphobranchusFA <- summarySE(subhatton, measurevar="SynFA", groupvars="Category")

plothattonSynaphobranchusFA <- ggplot(summaryhattonSynaphobranchusFA, aes(x=Category, y=SynFA)) +
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

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=subhatton)

#Plot SigmopsFA for Hatton Bank
summaryhattonSigmopsFA <- summarySE(subhatton, measurevar="SigFA", groupvars="Category")

plothattonSigmopsFA <- ggplot(summaryhattonSigmopsFA, aes(x=Category, y=SigFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SigFA-se, ymax=SigFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,40) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=subhatton)

#Plot MoraFA for Hatton Bank
summaryhattonMoraFA <- summarySE(subhatton, measurevar="MoraFA", groupvars="Category")

plothattonMoraFA <- ggplot(summaryhattonMoraFA, aes(x=Category, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,20) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=subhatton)

#Plot ChimaeraFA for Hatton Bank
summaryhattonChimaeraFA <- summarySE(subhatton, measurevar="ChimFA", groupvars="Category")

plothattonChimaeraFA <- ggplot(summaryhattonChimaeraFA, aes(x=Category, y=ChimFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFA-se, ymax=ChimFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=subhatton)

#Plot HelicolenusFA for Hatton Bank
summaryhattonHelicolenusFA <- summarySE(subhatton, measurevar="HeliFA", groupvars="Category")

plothattonHelicolenusFA <- ggplot(summaryhattonHelicolenusFA, aes(x=Category, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=subhatton)

#Plot LophiusFA for Hatton Bank
summaryhattonLophiusFA <- summarySE(subhatton, measurevar="LophiFA", groupvars="Category")

plothattonLophiusFA <- ggplot(summaryhattonLophiusFA, aes(x=Category, y=LophiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFA-se, ymax=LophiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=subhatton)

#Plot MolvaFA for Hatton Bank
summaryhattonMolvaFA <- summarySE(subhatton, measurevar="MolvaFA", groupvars="Category")

plothattonMolvaFA <- ggplot(summaryhattonMolvaFA, aes(x=Category, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=subhatton)

#Plot BrosmeFA for Hatton Bank
summaryhattonBrosmeFA <- summarySE(subhatton, measurevar="BrosmeFA", groupvars="Category")

plothattonBrosmeFA <- ggplot(summaryhattonBrosmeFA, aes(x=Category, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=subhatton)

#Plot HoplostethusFA for Hatton Bank
summaryhattonHoplostethusFA <- summarySE(subhatton, measurevar="HoploFA", groupvars="Category")

plothattonHoplostethusFA <- ggplot(summaryhattonHoplostethusFA, aes(x=Category, y=HoploFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFA-se, ymax=HoploFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFA~Category, data=subhatton)

#Plot PhycisFA for Hatton Bank
summaryhattonPhycisFA <- summarySE(subhatton, measurevar="PhycisFA", groupvars="Category")

plothattonPhycisFA <- ggplot(summaryhattonPhycisFA, aes(x=Category, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFA~Category, data=subhatton)

#Plot PseudotriakisFA for Hatton Bank
summaryhattonPseudotriakisFA <- summarySE(subhatton, measurevar="PseudoFA", groupvars="Category")

plothattonPseudotriakisFA <- ggplot(summaryhattonPseudotriakisFA, aes(x=Category, y=PseudoFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudoFA-se, ymax=PseudoFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PseudoFA~Category, data=subhatton)

#Plot RajaFA for Hatton Bank
summaryhattonRajaFA <- summarySE(subhatton, measurevar="RajaFA", groupvars="Category")

plothattonRajaFA <- ggplot(summaryhattonRajaFA, aes(x=Category, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(RajaFA~Category, data=subhatton)

#Plot AllSpeciesFA for Hatton Bank
summaryhattonAllSpeciesFA <- summarySE(subhatton, measurevar="AllFA", groupvars="Category")

plothattonAllSpeciesFA <- ggplot(summaryhattonAllSpeciesFA, aes(x=Category, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,300) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFA~Category, data=subhatton)


#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Region == "Rockall Bank")

#Plot LepidionFA for Rockall Bank
summaryrockallLepidionFA <- summarySE(subrockall, measurevar="LepiFA", groupvars="Category")

plotrockallLepidionFA <- ggplot(summaryrockallLepidionFA, aes(x=Category, y=LepiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFA-se, ymax=LepiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("")+
                  ylab("") +
                  ylim(0,250) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=subrockall)

#Plot SynaphobranchusFA for Rockall Bank
summaryrockallSynaphobranchusFA <- summarySE(subrockall, measurevar="SynFA", groupvars="Category")

plotrockallSynaphobranchusFA <- ggplot(summaryrockallSynaphobranchusFA, aes(x=Category, y=SynFA)) +
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

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=subrockall)

#Plot SigmopsFA for Rockall Bank
summaryrockallSigmopsFA <- summarySE(subrockall, measurevar="SigFA", groupvars="Category")

plotrockallSigmopsFA <- ggplot(summaryrockallSigmopsFA, aes(x=Category, y=SigFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SigFA-se, ymax=SigFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,40) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=subrockall)

#Plot MoraFA for Rockall Bank
summaryrockallMoraFA <- summarySE(subrockall, measurevar="MoraFA", groupvars="Category")

plotrockallMoraFA <- ggplot(summaryrockallMoraFA, aes(x=Category, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,20) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=subrockall)

#Plot ChimaeraFA for Rockall Bank
summaryrockallChimaeraFA <- summarySE(subrockall, measurevar="ChimFA", groupvars="Category")

plotrockallChimaeraFA <- ggplot(summaryrockallChimaeraFA, aes(x=Category, y=ChimFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFA-se, ymax=ChimFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=subrockall)

#Plot HelicolenusFA for Rockall Bank
summaryrockallHelicolenusFA <- summarySE(subrockall, measurevar="HeliFA", groupvars="Category")

plotrockallHelicolenusFA <- ggplot(summaryrockallHelicolenusFA, aes(x=Category, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=subrockall)

#Plot LophiusFA for Rockall Bank
summaryrockallLophiusFA <- summarySE(subrockall, measurevar="LophiFA", groupvars="Category")

plotrockallLophiusFA <- ggplot(summaryrockallLophiusFA, aes(x=Category, y=LophiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFA-se, ymax=LophiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=subrockall)

#Plot MolvaFA for Rockall Bank
summaryrockallMolvaFA <- summarySE(subrockall, measurevar="MolvaFA", groupvars="Category")

plotrockallMolvaFA <- ggplot(summaryrockallMolvaFA, aes(x=Category, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=subrockall)

#Plot BrosmeFA for Rockall Bank
summaryrockallBrosmeFA <- summarySE(subrockall, measurevar="BrosmeFA", groupvars="Category")

plotrockallBrosmeFA <- ggplot(summaryrockallBrosmeFA, aes(x=Category, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=subrockall)

#Plot HoplostethusFA for Rockall Bank
summaryrockallHoplostethusFA <- summarySE(subrockall, measurevar="HoploFA", groupvars="Category")

plotrockallHoplostethusFA <- ggplot(summaryrockallHoplostethusFA, aes(x=Category, y=HoploFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFA-se, ymax=HoploFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFA~Category, data=subrockall)

#Plot PhycisFA for Rockall Bank
summaryrockallPhycisFA <- summarySE(subrockall, measurevar="PhycisFA", groupvars="Category")

plotrockallPhycisFA <- ggplot(summaryrockallPhycisFA, aes(x=Category, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFA~Category, data=subrockall)

#Plot PseudotriakisFA for Rockall Bank
summaryrockallPseudotriakisFA <- summarySE(subrockall, measurevar="PseudoFA", groupvars="Category")

plotrockallPseudotriakisFA <- ggplot(summaryrockallPseudotriakisFA, aes(x=Category, y=PseudoFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudoFA-se, ymax=PseudoFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PseudoFA~Category, data=subrockall)

#Plot RajaFA for Rockall Bank
summaryrockallRajaFA <- summarySE(subrockall, measurevar="RajaFA", groupvars="Category")

plotrockallRajaFA <- ggplot(summaryrockallRajaFA, aes(x=Category, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(RajaFA~Category, data=subrockall)

#Plot AllSpeciesFA for Rockall Bank
summaryrockallAllSpeciesFA <- summarySE(subrockall, measurevar="AllFA", groupvars="Category")

plotrockallAllSpeciesFA <- ggplot(summaryrockallAllSpeciesFA, aes(x=Category, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,300) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFA~Category, data=subrockall)

#All Regions

#Plot LepidionFA for All Regions
summaryLepidionFA <- summarySE(data, measurevar="LepiFA", groupvars="Category")

plotLepidionFA <- ggplot(summaryLepidionFA, aes(x=Category, y=LepiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFA-se, ymax=LepiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("")+
                  ylab("") +
                  ylim(0,250) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=data)

#Plot SynaphobranchusFA for All Regions
summarySynaphobranchusFA <- summarySE(data, measurevar="SynFA", groupvars="Category")

plotSynaphobranchusFA <- ggplot(summarySynaphobranchusFA, aes(x=Category, y=SynFA)) +
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

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=data)

#Plot SigmopsFA for All Regions
summarySigmopsFA <- summarySE(data, measurevar="SigFA", groupvars="Category")

plotSigmopsFA <- ggplot(summarySigmopsFA, aes(x=Category, y=SigFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SigFA-se, ymax=SigFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,40) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=data)

#Plot MoraFA for Rockall Bank
summaryMoraFA <- summarySE(data, measurevar="MoraFA", groupvars="Category")

plotMoraFA <- ggplot(summaryMoraFA, aes(x=Category, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,20) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=data)

#Plot ChimaeraFA for All Regions
summaryChimaeraFA <- summarySE(data, measurevar="ChimFA", groupvars="Category")

plotChimaeraFA <- ggplot(summaryChimaeraFA, aes(x=Category, y=ChimFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFA-se, ymax=ChimFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=data)

#Plot HelicolenusFA for All Regions
summaryHelicolenusFA <- summarySE(data, measurevar="HeliFA", groupvars="Category")

plotHelicolenusFA <- ggplot(summaryHelicolenusFA, aes(x=Category, y=HeliFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFA-se, ymax=HeliFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=data)

#Plot LophiusFA for All Regions
summaryLophiusFA <- summarySE(data, measurevar="LophiFA", groupvars="Category")

plotLophiusFA <- ggplot(summaryLophiusFA, aes(x=Category, y=LophiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFA-se, ymax=LophiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=data)

#Plot MolvaFA for All Regions
summaryMolvaFA <- summarySE(data, measurevar="MolvaFA", groupvars="Category")

plotMolvaFA <- ggplot(summaryMolvaFA, aes(x=Category, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=data)

#Plot BrosmeFA for All Regions
summaryBrosmeFA <- summarySE(data, measurevar="BrosmeFA", groupvars="Category")

plotBrosmeFA <- ggplot(summaryBrosmeFA, aes(x=Category, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=data)

#Plot HoplostethusFA for All Regions
summaryHoplostethusFA <- summarySE(data, measurevar="HoploFA", groupvars="Category")

plotHoplostethusFA <- ggplot(summaryHoplostethusFA, aes(x=Category, y=HoploFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFA-se, ymax=HoploFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFA~Category, data=data)

#Plot PhycisFA for All Regions
summaryPhycisFA <- summarySE(data, measurevar="PhycisFA", groupvars="Category")

plotPhycisFA <- ggplot(summaryPhycisFA, aes(x=Category, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFA~Category, data=data)

#Plot PseudotriakisFA for All Regions
summaryPseudotriakisFA <- summarySE(data, measurevar="PseudoFA", groupvars="Category")

plotPseudotriakisFA <- ggplot(summaryPseudotriakisFA, aes(x=Category, y=PseudoFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudoFA-se, ymax=PseudoFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PseudoFA~Category, data=data)

#Plot RajaFA for All Regions
summaryRajaFA <- summarySE(data, measurevar="RajaFA", groupvars="Category")

plotRajaFA <- ggplot(summaryRajaFA, aes(x=Category, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(RajaFA~Category, data=data)

#Plot AllSpeciesFA for Rockall Bank
summaryAllSpeciesFA <- summarySE(data, measurevar="AllFA", groupvars="Category")

plotAllSpeciesFA <- ggplot(summaryAllSpeciesFA, aes(x=Category, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,300) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFA~Category, data=data)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(7, 4)))

print(plotbelgicaLepidionFA, vp = vplayout(1, 1))
print(plotbelgicaSynaphobranchusFA, vp = vplayout(2, 1))
print(plotbelgicaSigmopsFA, vp = vplayout(3, 1))
print(plotbelgicaMoraFA, vp = vplayout(4, 1))
print(plotbelgicaChimaeraFA, vp = vplayout(5, 1))
print(plotbelgicaHelicolenusFA, vp = vplayout(6, 1))
print(plotbelgicaLophiusFA, vp = vplayout(7, 1))

print(plothattonLepidionFA, vp = vplayout(1, 2))
print(plothattonSynaphobranchusFA, vp = vplayout(2, 2))
print(plothattonSigmopsFA, vp = vplayout(3, 2))
print(plothattonMoraFA, vp = vplayout(4, 2))
print(plothattonChimaeraFA, vp = vplayout(5, 2))
print(plothattonHelicolenusFA, vp = vplayout(6, 2))
print(plothattonLophiusFA, vp = vplayout(7, 2))

print(plotrockallLepidionFA, vp = vplayout(1, 3))
print(plotrockallSynaphobranchusFA, vp = vplayout(2, 3))
print(plotrockallSigmopsFA, vp = vplayout(3, 3))
print(plotrockallMoraFA, vp = vplayout(4, 3))
print(plotrockallChimaeraFA, vp = vplayout(5, 3))
print(plotrockallHelicolenusFA, vp = vplayout(6, 3))
print(plotrockallLophiusFA, vp = vplayout(7, 3))

print(plotLepidionFA, vp = vplayout(1, 4))
print(plotSynaphobranchusFA, vp = vplayout(2, 4))
print(plotSigmopsFA, vp = vplayout(3, 4))
print(plotMoraFA, vp = vplayout(4, 4))
print(plotChimaeraFA, vp = vplayout(5, 4))
print(plotHelicolenusFA, vp = vplayout(6, 4))
print(plotLophiusFA, vp = vplayout(7, 4))


# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(7, 4)))

print(plotbelgicaMolvaFA, vp = vplayout(1, 1))
print(plotbelgicaBrosmeFA, vp = vplayout(2, 1))
print(plotbelgicaHoplostethusFA, vp = vplayout(3, 1))
print(plotbelgicaPhycisFA, vp = vplayout(4, 1))
print(plotbelgicaPseudotriakisFA, vp = vplayout(5, 1))
print(plotbelgicaRajaFA, vp = vplayout(6, 1))
print(plotbelgicaAllSpeciesFA, vp = vplayout(7, 1))

print(plothattonMolvaFA, vp = vplayout(1, 2))
print(plothattonBrosmeFA, vp = vplayout(2, 2))
print(plothattonHoplostethusFA, vp = vplayout(3, 2))
print(plothattonPhycisFA, vp = vplayout(4, 2))
print(plothattonPseudotriakisFA, vp = vplayout(5, 2))
print(plothattonRajaFA, vp = vplayout(6, 2))
print(plothattonAllSpeciesFA, vp = vplayout(7, 2))

print(plotrockallMolvaFA, vp = vplayout(1, 3))
print(plotrockallBrosmeFA, vp = vplayout(2, 3))
print(plotrockallHoplostethusFA, vp = vplayout(3, 3))
print(plotrockallPhycisFA, vp = vplayout(4, 3))
print(plotrockallPseudotriakisFA, vp = vplayout(5, 3))
print(plotrockallRajaFA, vp = vplayout(6, 3))
print(plotrockallAllSpeciesFA, vp = vplayout(7, 3))

print(plotMolvaFA, vp = vplayout(1, 4))
print(plotBrosmeFA, vp = vplayout(2, 4))
print(plotHoplostethusFA, vp = vplayout(3, 4))
print(plotPhycisFA, vp = vplayout(4, 4))
print(plotPseudotriakisFA, vp = vplayout(5, 4))
print(plotRajaFA, vp = vplayout(6, 4))
print(plotAllSpeciesFA, vp = vplayout(7, 4))


# Fish biomass

#Plot LepidionFB for Belgica Mound
summarybelgicaLepidionFB <- summarySE(subbelgica, measurevar="LepiFB", groupvars="Category")

plotbelgicaLepidionFB <- ggplot(summarybelgicaLepidionFB, aes(x=Category, y=LepiFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFB-se, ymax=LepiFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +                
                  xlab("") +
                  ylab(expression(paste(italic("Lepidion eques")))) +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFB~Category, data=subbelgica)

#Plot SynaphobranchusFB for Belgica Mound
summarybelgicaSynaphobranchusFB <- summarySE(subbelgica, measurevar="SynFB", groupvars="Category")

plotbelgicaSynaphobranchusFB <- ggplot(summarybelgicaSynaphobranchusFB, aes(x=Category, y=SynFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFB-se, ymax=SynFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Synaphobranchus kaupii")))) +
                  ylim(0,1) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SynFB~Category, data=subbelgica)


#Plot MoraFB for Belgica Mound
summarybelgicaMoraFB <- summarySE(subbelgica, measurevar="MoraFB", groupvars="Category")

plotbelgicaMoraFB <- ggplot(summarybelgicaMoraFB, aes(x=Category, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Mora moro")))) +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFB~Category, data=subbelgica)

#Plot ChimaeraFA for Belgica Mound
summarybelgicaChimaeraFB <- summarySE(subbelgica, measurevar="ChimFB", groupvars="Category")

plotbelgicaChimaeraFB <- ggplot(summarybelgicaChimaeraFB, aes(x=Category, y=ChimFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFB-se, ymax=ChimFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Chimaera monstrosa")))) +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFB~Category, data=subbelgica)

#Plot HelicolenusFB for Belgica Mound
summarybelgicaHelicolenusFB <- summarySE(subbelgica, measurevar="HeliFB", groupvars="Category")

plotbelgicaHelicolenusFB <- ggplot(summarybelgicaHelicolenusFB, aes(x=Category, y=HeliFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Helicolenus dactylopterus")))) +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFB~Category, data=subbelgica)

#Plot LophiusFB for Belgica Mound
summarybelgicaLophiusFB <- summarySE(subbelgica, measurevar="LophiFB", groupvars="Category")

plotbelgicaLophiusFB <- ggplot(summarybelgicaLophiusFB, aes(x=Category, y=LophiFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFB-se, ymax=LophiFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0,120) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFB~Category, data=subbelgica)

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFB <- summarySE(subbelgica, measurevar="MolvaFB", groupvars="Category")

plotbelgicaMolvaFB <- ggplot(summarybelgicaMolvaFB, aes(x=Category, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFB~Category, data=subbelgica)

#Plot BrosmeFB for Belgica Mound
summarybelgicaBrosmeFB <- summarySE(subbelgica, measurevar="BrosmeFB", groupvars="Category")

plotbelgicaBrosmeFB <- ggplot(summarybelgicaBrosmeFB, aes(x=Category, y=BrosmeFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFB-se, ymax=BrosmeFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Brosme brosme")))) +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFB~Category, data=subbelgica)

#Plot HoplostethusFB for Belgica Mound
summarybelgicaHoplostethusFB <- summarySE(subbelgica, measurevar="HoploFB", groupvars="Category")

plotbelgicaHoplostethusFB <- ggplot(summarybelgicaHoplostethusFB, aes(x=Category, y=HoploFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFB-se, ymax=HoploFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Hoplostethus atlanticus")))) +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFB~Category, data=subbelgica)

#Plot PhycisFA for Belgica Mound
summarybelgicaPhycisFB <- summarySE(subbelgica, measurevar="PhycisFB", groupvars="Category")

plotbelgicaPhycisFB <- ggplot(summarybelgicaPhycisFB, aes(x=Category, y=PhycisFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFB-se, ymax=PhycisFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Phycis phycis")))) +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFB~Category, data=subbelgica)


#Plot AllSpeciesFB for Belgica Mound
summarybelgicaAllSpeciesFB <- summarySE(subbelgica, measurevar="AllFB", groupvars="Category")

plotbelgicaAllSpeciesFB <- ggplot(summarybelgicaAllSpeciesFB, aes(x=Category, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Total")))) +
                  ylim(0,150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFB~Category, data=subbelgica)


#Hatton Bank

#Plot LepidionFB for Hatton Bank
summaryhattonLepidionFB <- summarySE(subhatton, measurevar="LepiFB", groupvars="Category")

plothattonLepidionFB <- ggplot(summaryhattonLepidionFB, aes(x=Category, y=LepiFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFB-se, ymax=LepiFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +               
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFB~Category, data=subhatton)

#Plot SynaphobranchusFB for Hatton Bank
summaryhattonSynaphobranchusFB <- summarySE(subhatton, measurevar="SynFB", groupvars="Category")

plothattonSynaphobranchusFB <- ggplot(summaryhattonSynaphobranchusFB, aes(x=Category, y=SynFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFB-se, ymax=SynFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,1) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SynFB~Category, data=subhatton)


#Plot MoraFA for Hatton Bank
summaryhattonMoraFB <- summarySE(subhatton, measurevar="MoraFB", groupvars="Category")

plothattonMoraFB <- ggplot(summaryhattonMoraFB, aes(x=Category, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFB~Category, data=subhatton)

#Plot ChimaeraFA for Hatton Bank
summaryhattonChimaeraFB <- summarySE(subhatton, measurevar="ChimFB", groupvars="Category")

plothattonChimaeraFB <- ggplot(summaryhattonChimaeraFB, aes(x=Category, y=ChimFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFB-se, ymax=ChimFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFB~Category, data=subhatton)

#Plot HelicolenusFB for Hatton Bank
summaryhattonHelicolenusFB <- summarySE(subhatton, measurevar="HeliFB", groupvars="Category")

plothattonHelicolenusFB <- ggplot(summaryhattonHelicolenusFB, aes(x=Category, y=HeliFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFB~Category, data=subhatton)

#Plot LophiusFB for Hatton Bank
summaryhattonLophiusFB <- summarySE(subhatton, measurevar="LophiFB", groupvars="Category")

plothattonLophiusFB<- ggplot(summaryhattonLophiusFB, aes(x=Category, y=LophiFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFB-se, ymax=LophiFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,120) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFB~Category, data=subhatton)

#Plot MolvaFB for Hatton Bank
summaryhattonMolvaFB <- summarySE(subhatton, measurevar="MolvaFB", groupvars="Category")

plothattonMolvaFB <- ggplot(summaryhattonMolvaFB, aes(x=Category, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFB~Category, data=subhatton)

#Plot BrosmeFB for Hatton Bank
summaryhattonBrosmeFB <- summarySE(subhatton, measurevar="BrosmeFB", groupvars="Category")

plothattonBrosmeFB <- ggplot(summaryhattonBrosmeFB, aes(x=Category, y=BrosmeFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFB-se, ymax=BrosmeFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFB~Category, data=subhatton)

#Plot HoplostethusFB for Hatton Bank
summaryhattonHoplostethusFB <- summarySE(subhatton, measurevar="HoploFB", groupvars="Category")

plothattonHoplostethusFB <- ggplot(summaryhattonHoplostethusFB, aes(x=Category, y=HoploFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFB-se, ymax=HoploFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFB~Category, data=subhatton)

#Plot PhycisFA for Hatton Bank
summaryhattonPhycisFB <- summarySE(subhatton, measurevar="PhycisFB", groupvars="Category")

plothattonPhycisFB <- ggplot(summaryhattonPhycisFB, aes(x=Category, y=PhycisFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFB-se, ymax=PhycisFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFB~Category, data=subhatton)


#Plot AllSpeciesFB for Hatton Bank
summaryhattonAllSpeciesFB <- summarySE(subhatton, measurevar="AllFB", groupvars="Category")

plothattonAllSpeciesFB <- ggplot(summaryhattonAllSpeciesFB, aes(x=Category, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFB~Category, data=subhatton)


#Rockall Bank

#Plot LepidionFB for Rockall Bank
summaryrockallLepidionFB <- summarySE(subrockall, measurevar="LepiFB", groupvars="Category")

plotrockallLepidionFB <- ggplot(summaryrockallLepidionFB, aes(x=Category, y=LepiFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFB-se, ymax=LepiFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("")+
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFB~Category, data=subrockall)

#Plot SynaphobranchusFB for Rockall Bank
summaryrockallSynaphobranchusFB <- summarySE(subrockall, measurevar="SynFB", groupvars="Category")

plotrockallSynaphobranchusFB <- ggplot(summaryrockallSynaphobranchusFB, aes(x=Category, y=SynFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SynFB-se, ymax=SynFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,1) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(SynFB~Category, data=subrockall)


#Plot MoraFB for Rockall Bank
summaryrockallMoraFB <- summarySE(subrockall, measurevar="MoraFB", groupvars="Category")

plotrockallMoraFB <- ggplot(summaryrockallMoraFB, aes(x=Category, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MoraFB~Category, data=subrockall)

#Plot ChimaeraFB for Rockall Bank
summaryrockallChimaeraFB <- summarySE(subrockall, measurevar="ChimFB", groupvars="Category")

plotrockallChimaeraFB <- ggplot(summaryrockallChimaeraFB, aes(x=Category, y=ChimFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimFB-se, ymax=ChimFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(ChimFB~Category, data=subrockall)

#Plot HelicolenusFB for Rockall Bank
summaryrockallHelicolenusFB <- summarySE(subrockall, measurevar="HeliFB", groupvars="Category")

plotrockallHelicolenusFB <- ggplot(summaryrockallHelicolenusFB, aes(x=Category, y=HeliFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HeliFB~Category, data=subrockall)

#Plot LophiusFB for Rockall Bank
summaryrockallLophiusFB <- summarySE(subrockall, measurevar="LophiFB", groupvars="Category")

plotrockallLophiusFB <- ggplot(summaryrockallLophiusFB, aes(x=Category, y=LophiFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFB-se, ymax=LophiFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,120) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LophiFB~Category, data=subrockall)

#Plot MolvaFB for Rockall Bank
summaryrockallMolvaFB <- summarySE(subrockall, measurevar="MolvaFB", groupvars="Category")

plotrockallMolvaFB <- ggplot(summaryrockallMolvaFB, aes(x=Category, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(MolvaFB~Category, data=subrockall)

#Plot BrosmeFB for Rockall Bank
summaryrockallBrosmeFB <- summarySE(subrockall, measurevar="BrosmeFB", groupvars="Category")

plotrockallBrosmeFB <- ggplot(summaryrockallBrosmeFB, aes(x=Category, y=BrosmeFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFB-se, ymax=BrosmeFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(BrosmeFB~Category, data=subrockall)

#Plot HoplostethusFB for Rockall Bank
summaryrockallHoplostethusFB <- summarySE(subrockall, measurevar="HoploFB", groupvars="Category")

plotrockallHoplostethusFB <- ggplot(summaryrockallHoplostethusFB, aes(x=Category, y=HoploFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFB-se, ymax=HoploFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,15) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(HoploFB~Category, data=subrockall)

#Plot PhycisFB for Rockall Bank
summaryrockallPhycisFB <- summarySE(subrockall, measurevar="PhycisFB", groupvars="Category")

plotrockallPhycisFB <- ggplot(summaryrockallPhycisFB, aes(x=Category, y=PhycisFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFB-se, ymax=PhycisFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,0.5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(PhycisFB~Category, data=subrockall)


#Plot AllSpeciesFB for Rockall Bank
summaryrockallAllSpeciesFB <- summarySE(subrockall, measurevar="AllFB", groupvars="Category")

plotrockallAllSpeciesFB <- ggplot(summaryrockallAllSpeciesFB, aes(x=Category, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFB~Category, data=subrockall)

             #All Regions
             
             #Plot LepidionFB for All Regions
             summaryLepidionFB <- summarySE(data, measurevar="LepiFB", groupvars="Category")
             
             plotLepidionFB <- ggplot(summaryLepidionFB, aes(x=Category, y=LepiFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=LepiFB-se, ymax=LepiFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("")+
                               ylab("") +
                               ylim(0,25) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(LepiFB~Category, data=data)
             
             #Plot SynaphobranchusFB for All Regions
             summarySynaphobranchusFB <- summarySE(data, measurevar="SynFB", groupvars="Category")
             
             plotSynaphobranchusFB <- ggplot(summarySynaphobranchusFB, aes(x=Category, y=SynFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=SynFB-se, ymax=SynFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,1) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(SynFB~Category, data=data)
             
             
             #Plot MoraFB for All Regions
             summaryMoraFB <- summarySE(data, measurevar="MoraFB", groupvars="Category")
             
             plotMoraFB <- ggplot(summaryMoraFB, aes(x=Category, y=MoraFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,25) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(MoraFB~Category, data=data)
             
             #Plot ChimaeraFB for All Regions
             summaryChimaeraFB <- summarySE(data, measurevar="ChimFB", groupvars="Category")
             
             plotChimaeraFB <- ggplot(summaryChimaeraFB, aes(x=Category, y=ChimFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=ChimFB-se, ymax=ChimFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,15) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(ChimFB~Category, data=data)
             
             #Plot HelicolenusFB for All Regions
             summaryHelicolenusFB <- summarySE(data, measurevar="HeliFB", groupvars="Category")
             
             plotHelicolenusFB <- ggplot(summaryHelicolenusFB, aes(x=Category, y=HeliFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=HeliFB-se, ymax=HeliFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,25) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(HeliFB~Category, data=data)
             
             #Plot LophiusFB for All Regions
             summaryLophiusFB <- summarySE(data, measurevar="LophiFB", groupvars="Category")
             
             plotLophiusFB <- ggplot(summaryLophiusFB, aes(x=Category, y=LophiFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=LophiFB-se, ymax=LophiFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,120) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(LophiFB~Category, data=data)
             
             #Plot MolvaFB for All Regions
             summaryMolvaFB <- summarySE(data, measurevar="MolvaFB", groupvars="Category")
             
             plotMolvaFB <- ggplot(summaryMolvaFB, aes(x=Category, y=MolvaFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,15) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(MolvaFB~Category, data=data)
             
             #Plot BrosmeFB for All Regions
             summaryBrosmeFB <- summarySE(data, measurevar="BrosmeFB", groupvars="Category")
             
             plotBrosmeFB <- ggplot(summaryBrosmeFB, aes(x=Category, y=BrosmeFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=BrosmeFB-se, ymax=BrosmeFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,15) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(BrosmeFB~Category, data=data)
             
             #Plot HoplostethusFB for All Regions
             summaryHoplostethusFB <- summarySE(data, measurevar="HoploFB", groupvars="Category")
             
             plotHoplostethusFB <- ggplot(summaryHoplostethusFB, aes(x=Category, y=HoploFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=HoploFB-se, ymax=HoploFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,15) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(HoploFB~Category, data=data)
             
             #Plot PhycisFB for All Regions
             summaryPhycisFB <- summarySE(data, measurevar="PhycisFB", groupvars="Category")
             
             plotPhycisFB <- ggplot(summaryPhycisFB, aes(x=Category, y=PhycisFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=PhycisFB-se, ymax=PhycisFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,0.5) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(PhycisFB~Category, data=data)
             
             
             #Plot AllSpeciesFB for All Regions
             summaryAllSpeciesFB <- summarySE(data, measurevar="AllFB", groupvars="Category")
             
             plotAllSpeciesFB <- ggplot(summaryAllSpeciesFB, aes(x=Category, y=AllFB)) +
               geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
               geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                             width=.1,                    # Width of the error bars
                             position=position_dodge(.9)) +
                               xlab("") +
                               ylab("") +
                               ylim(0,150) +
                               #scale_y_continuous(breaks=-1:20000*1000) +
                               #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                               geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                               theme_map
             
             # Kruskal-Wallis Test
             kruskal.test(AllFB~Category, data=data)
             
             
# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(7, 4)))

print(plotbelgicaLepidionFB, vp = vplayout(1, 1))
print(plotbelgicaSynaphobranchusFB, vp = vplayout(2, 1))
print(plotbelgicaMoraFB, vp = vplayout(3, 1))
print(plotbelgicaChimaeraFB, vp = vplayout(4, 1))
print(plotbelgicaHelicolenusFB, vp = vplayout(5, 1))
print(plotbelgicaLophiusFB, vp = vplayout(6, 1))
print(plotbelgicaMolvaFB, vp = vplayout(7, 1))

print(plothattonLepidionFB, vp = vplayout(1, 2))
print(plothattonSynaphobranchusFB, vp = vplayout(2, 2))
print(plothattonMoraFB, vp = vplayout(3, 2))
print(plothattonChimaeraFB, vp = vplayout(4, 2))
print(plothattonHelicolenusFB, vp = vplayout(5, 2))
print(plothattonLophiusFB, vp = vplayout(6, 2))
print(plothattonMolvaFB, vp = vplayout(7, 2))

print(plotrockallLepidionFB, vp = vplayout(1, 3))
print(plotrockallSynaphobranchusFB, vp = vplayout(2, 3))
print(plotrockallMoraFB, vp = vplayout(3, 3))
print(plotrockallChimaeraFB, vp = vplayout(4, 3))
print(plotrockallHelicolenusFB, vp = vplayout(5, 3))
print(plotrockallLophiusFB, vp = vplayout(6, 3))
print(plotrockallMolvaFB, vp = vplayout(7, 3))

print(plotLepidionFB, vp = vplayout(1, 4))
print(plotSynaphobranchusFB, vp = vplayout(2, 4))
print(plotMoraFB, vp = vplayout(3, 4))
print(plotChimaeraFB, vp = vplayout(4, 4))
print(plotHelicolenusFB, vp = vplayout(5, 4))
print(plotLophiusFB, vp = vplayout(6, 4))
print(plotMolvaFB, vp = vplayout(7, 4))
             

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))

print(plotbelgicaBrosmeFB, vp = vplayout(1, 1))
print(plotbelgicaHoplostethusFB, vp = vplayout(2, 1))
print(plotbelgicaPhycisFB, vp = vplayout(3, 1))
print(plotbelgicaAllSpeciesFB, vp = vplayout(4, 1))

print(plothattonBrosmeFB, vp = vplayout(1, 2))
print(plothattonHoplostethusFB, vp = vplayout(2, 2))
print(plothattonPhycisFB, vp = vplayout(3, 2))
print(plothattonAllSpeciesFB, vp = vplayout(4, 2))

print(plotrockallBrosmeFB, vp = vplayout(1, 3))
print(plotrockallHoplostethusFB, vp = vplayout(2, 3))
print(plotrockallPhycisFB, vp = vplayout(3, 3))
print(plotrockallAllSpeciesFB, vp = vplayout(4, 3))

print(plotBrosmeFB, vp = vplayout(1, 4))
print(plotHoplostethusFB, vp = vplayout(2, 4))
print(plotPhycisFB, vp = vplayout(3, 4))
print(plotAllSpeciesFB, vp = vplayout(4, 4))