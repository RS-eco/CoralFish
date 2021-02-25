library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
setwd("C:/Users/MBiber/Documents/Analysis_R")

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data <- read.delim("CoralFISH_BarGraph.csv", sep=",",header=T)

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
  geom_bar(colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LepiFA-se, ymax=LepiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.0)) +                
                  xlab("") +
                  ylab(expression(paste(italic("Lepidion eques")))) +
                  ylim(0,250) +
                  geom_hline(yintercept=0, width=.5)  + #add horizontal 0 line
                  theme_map


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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot LophiusFA for Belgica Mound
summarybelgicaLophiusFA <- summarySE(subbelgica, measurevar="LophiFA", groupvars="Category")

plotbelgicaLophiusFA <- ggplot(summarybelgicaLophiusFA, aes(x=Category, y=LophiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFA-se, ymax=LophiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFA <- summarySE(subbelgica, measurevar="MolvaFA", groupvars="Category")

plotbelgicaMolvaFA <- ggplot(summarybelgicaMolvaFA, aes(x=Category, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,6) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot BrosmeFA for Belgica Mound
summarybelgicaBrosmeFA <- summarySE(subbelgica, measurevar="BrosmeFA", groupvars="Category")

plotbelgicaBrosmeFA <- ggplot(summarybelgicaBrosmeFA, aes(x=Category, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,5) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot HoplostethusFA for Belgica Mound
summarybelgicaHoplostethusFA <- summarySE(subbelgica, measurevar="HoploFA", groupvars="Category")

plotbelgicaHoplostethusFA <- ggplot(summarybelgicaHoplostethusFA, aes(x=Category, y=HoploFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFA-se, ymax=HoploFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,4) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot AllSpeciesFA for Belgica Mound
summarybelgicaAllSpeciesFA <- summarySE(subbelgica, measurevar="AllFA", groupvars="Category")

plotbelgicaAllSpeciesFA <- ggplot(summarybelgicaAllSpeciesFA, aes(x=Category, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("Total") +
                  ylim(0,300) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=subbelgica)

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(HoploFA~Category, data=subhatton)

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot LophiusFA for Rockall Bank
summaryrockallLophiusFA <- summarySE(subrockall, measurevar="LophiFA", groupvars="Category")

plotrockallLophiusFA <- ggplot(summaryrockallLophiusFA, aes(x=Category, y=LophiFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiFA-se, ymax=LophiFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0,10) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot MolvaFA for Rockall Bank
summaryrockallMolvaFA <- summarySE(subrockall, measurevar="MolvaFA", groupvars="Category")

plotrockallMolvaFA <- ggplot(summaryrockallMolvaFA, aes(x=Category, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0,6) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot BrosmeFA for Rockall Bank
summaryrockallBrosmeFA <- summarySE(subrockall, measurevar="BrosmeFA", groupvars="Category")

plotrockallBrosmeFA <- ggplot(summaryrockallBrosmeFA, aes(x=Category, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Brosme brosme")))) +
                  ylim(0,5) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot HoplostethusFA for Rockall Bank
summaryrockallHoplostethusFA <- summarySE(subrockall, measurevar="HoploFA", groupvars="Category")

plotrockallHoplostethusFA <- ggplot(summaryrockallHoplostethusFA, aes(x=Category, y=HoploFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoploFA-se, ymax=HoploFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Hoplostethus atlanticus")))) +
                  ylim(0,4) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(SynFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(SigFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(MoraFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(ChimFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(HeliFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(LophiFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(MolvaFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(BrosmeFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(HoploFA~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(AllFA~Category, data=subrockall)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 3)))

print(plotbelgicaHelicolenusFA, vp = vplayout(1, 1))
print(plotbelgicaLepidionFA, vp = vplayout(2, 1))
print(plotbelgicaSigmopsFA, vp = vplayout(5, 1))
print(plotbelgicaSynaphobranchusFA, vp = vplayout(3, 1))
print(plotbelgicaAllSpeciesFA, vp = vplayout(4, 1))

print(plothattonHelicolenusFA, vp = vplayout(1, 2))
print(plothattonLepidionFA, vp = vplayout(2, 2))
print(plothattonSigmopsFA, vp = vplayout(5, 2))
print(plothattonSynaphobranchusFA, vp = vplayout(3, 2))
print(plothattonAllSpeciesFA, vp = vplayout(4, 2))

print(plotrockallHelicolenusFA, vp = vplayout(1, 3))
print(plotrockallLepidionFA, vp = vplayout(2, 3))
print(plotrockallSigmopsFA, vp = vplayout(5, 3))
print(plotrockallSynaphobranchusFA, vp = vplayout(3, 3))
print(plotrockallAllSpeciesFA, vp = vplayout(4, 3))


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
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot SynaphobranchusFB for Belgica Mound
summarybelgicaSynaphobranchusFB <- summarySE(subbelgica, measurevar="SynFB", groupvars="Category")

plotbelgicaSynaphobranchusFB <- ggplot(summarybelgicaSynaphobranchusFB, aes(x=Category, y=SynFB)) +
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

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFB <- summarySE(subbelgica, measurevar="MolvaFB", groupvars="Category")

plotbelgicaMolvaFB <- ggplot(summarybelgicaMolvaFB, aes(x=Category, y=MolvaFB)) +
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


#Plot AllSpeciesFB for Belgica Mound
summarybelgicaAllSpeciesFB <- summarySE(subbelgica, measurevar="AllFB", groupvars="Category")

plotbelgicaAllSpeciesFB <- ggplot(summarybelgicaAllSpeciesFB, aes(x=Category, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("Total") +
                  ylim(0,150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(LepiFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(SynFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(MoraFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(ChimFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(HeliFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(LophiFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(MolvaFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(BrosmeFB~Category, data=subbelgica)

# Kruskal-Wallis Test
kruskal.test(HoploFB~Category, data=subbelgica)

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
kruskal.test(LepiFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(SynFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(MoraFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(ChimFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(HeliFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(LophiFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(MolvaFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(BrosmeFB~Category, data=subhatton)

# Kruskal-Wallis Test
kruskal.test(HoploFB~Category, data=subhatton)

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

#Plot BrosmeFB for Rockall Bank
summaryrockallBrosmeFB <- summarySE(subrockall, measurevar="BrosmeFB", groupvars="Category")

plotrockallBrosmeFB <- ggplot(summaryrockallBrosmeFB, aes(x=Category, y=BrosmeFB)) +
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

#Plot HoplostethusFB for Rockall Bank
summaryrockallHoplostethusFB <- summarySE(subrockall, measurevar="HoploFB", groupvars="Category")

plotrockallHoplostethusFB <- ggplot(summaryrockallHoplostethusFB, aes(x=Category, y=HoploFB)) +
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
kruskal.test(LepiFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(SynFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(MoraFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(ChimFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(HeliFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(LophiFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(MolvaFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(BrosmeFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(HoploFB~Category, data=subrockall)

# Kruskal-Wallis Test
kruskal.test(AllFB~Category, data=subrockall)

             
# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 3)))

print(plotbelgicaHelicolenusFB, vp = vplayout(1, 1))
print(plotbelgicaLepidionFB, vp = vplayout(2, 1))
print(plotbelgicaLophiusFB, vp = vplayout(3, 1))
print(plotbelgicaMoraFB, vp = vplayout(4, 1))
print(plotbelgicaAllSpeciesFB, vp = vplayout(5, 1))

print(plothattonHelicolenusFB, vp = vplayout(1, 2))
print(plothattonLepidionFB, vp = vplayout(2, 2))
print(plothattonLophiusFB, vp = vplayout(3, 2))
print(plothattonMoraFB, vp = vplayout(4, 2))
print(plothattonAllSpeciesFB, vp = vplayout(5, 2))

print(plotrockallHelicolenusFB, vp = vplayout(1, 3))
print(plotrockallLepidionFB, vp = vplayout(2, 3))
print(plotrockallLophiusFB, vp = vplayout(3, 3))
print(plotrockallMoraFB, vp = vplayout(4, 3))
print(plotrockallAllSpeciesFB, vp = vplayout(5, 3))





