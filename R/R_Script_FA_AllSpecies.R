library(ggplot2)
library(grid)
library(plyr)

#Location of summarise Script
#setwd("C:/Users/MBiber/Documents/Analysis_R")
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

#setwd("C:/Users/MBiber/Documents/Analysis_R")
data3 <- read.delim("VideoData_FA_FB.csv", sep=",",header=T)

#attach(data3)

#One-Way Anova, Summary, 2 Plots, unsuitable for this Analysis!
#modLepidionFA <- aov(LepidionFA~CoralNoCoral, data=data3)
#summary(modLepidionFA)
#plot(TukeyHSD(modLepidionFA))
#plot(CoralNoCoral,LepidionFA, data=data3)

#Kruskal-Wallis Test - non-parametric test for more than 2 samples
#> kruskal.test(LepidionFA ~ CoralNoCoral, data =data3)

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
subbelgica <- subset(data3, Area == "Belgica Mound")

#Plot LepidionFA for Belgica Mound
summarybelgicaLepidionFA <- summarySE(subbelgica, measurevar="LepidionFA", groupvars="CoralNoCoral")

plotbelgicaLepidionFA <- ggplot(summarybelgicaLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +                
    xlab("") +
    ylab(expression(paste(italic("Lepidion eques")))) +
    ylim(0,400) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LepidionFA~CoralNoCoral, data=subbelgica)

#Plot SynaphobranchusFA for Belgica Mound
summarybelgicaSynaphobranchusFA <- summarySE(subbelgica, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plotbelgicaSynaphobranchusFA <- ggplot(summarybelgicaSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
    ylab(expression(paste(italic("Synaphobranchus kaupii")))) +
    ylim(0,50) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SynaphobranchusFA~CoralNoCoral, data=subbelgica)

#Plot SigmopsFA for Belgica Mound
summarybelgicaSigmopsFA <- summarySE(subbelgica, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plotbelgicaSigmopsFA <- ggplot(summarybelgicaSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab(expression(paste(italic("Sigmops bathyphilus")))) +
     ylim(0,100) +
     #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SigmopsFA~CoralNoCoral, data=subbelgica)

#Plot MoraFA for Belgica Mound
summarybelgicaMoraFA <- summarySE(subbelgica, measurevar="MoraFA", groupvars="CoralNoCoral")

plotbelgicaMoraFA <- ggplot(summarybelgicaMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
    xlab("") +
      ylab(expression(paste(italic("Mora moro")))) +
      ylim(0,25) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MoraFA~CoralNoCoral, data=subbelgica)

#Plot ChimaeraFA for Belgica Mound
summarybelgicaChimaeraFA <- summarySE(subbelgica, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plotbelgicaChimaeraFA <- ggplot(summarybelgicaChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
    xlab("") +
     ylab(expression(paste(italic("Chimaera monstrosa")))) +
     ylim(0,50) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(ChimaeraFA~CoralNoCoral, data=subbelgica)

#Plot HelicolenusFA for Belgica Mound
summarybelgicaHelicolenusFA <- summarySE(subbelgica, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plotbelgicaHelicolenusFA <- ggplot(summarybelgicaHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
    xlab("") +
     ylab(expression(paste(italic("Helicolenus dactylopterus")))) +
     ylim(0,200) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HelicolenusFA~CoralNoCoral, data=subbelgica)

#Plot LophiusFA for Belgica Mound
summarybelgicaLophiusFA <- summarySE(subbelgica, measurevar="LophiusFA", groupvars="CoralNoCoral")

plotbelgicaLophiusFA <- ggplot(summarybelgicaLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LophiusFA~CoralNoCoral, data=subbelgica)

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFA <- summarySE(subbelgica, measurevar="MolvaFA", groupvars="CoralNoCoral")

plotbelgicaMolvaFA <- ggplot(summarybelgicaMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MolvaFA~CoralNoCoral, data=subbelgica)

#Plot BrosmeFA for Belgica Mound
summarybelgicaBrosmeFA <- summarySE(subbelgica, measurevar="BrosmeFA", groupvars="CoralNoCoral")

plotbelgicaBrosmeFA <- ggplot(summarybelgicaBrosmeFA, aes(x=CoralNoCoral, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(BrosmeFA~CoralNoCoral, data=subbelgica)

#Plot HoplostethusFA for Belgica Mound
summarybelgicaHoplostethusFA <- summarySE(subbelgica, measurevar="HoplostethusFA", groupvars="CoralNoCoral")

plotbelgicaHoplostethusFA <- ggplot(summarybelgicaHoplostethusFA, aes(x=CoralNoCoral, y=HoplostethusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoplostethusFA-se, ymax=HoplostethusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HoplostethusFA~CoralNoCoral, data=subbelgica)

#Plot PhycisFA for Belgica Mound
summarybelgicaPhycisFA <- summarySE(subbelgica, measurevar="PhycisFA", groupvars="CoralNoCoral")

plotbelgicaPhycisFA <- ggplot(summarybelgicaPhycisFA, aes(x=CoralNoCoral, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(PhycisFA~CoralNoCoral, data=subbelgica)

#Plot PseudotriakisFA for Belgica Mound
summarybelgicaPseudotriakisFA <- summarySE(subbelgica, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")

plotbelgicaPseudotriakisFA <- ggplot(summarybelgicaPseudotriakisFA, aes(x=CoralNoCoral, y=PseudotriakisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudotriakisFA-se, ymax=PseudotriakisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(PseudotriakisFA~CoralNoCoral, data=subbelgica)

#Plot RajaFA for Belgica Mound
summarybelgicaRajaFA <- summarySE(subbelgica, measurevar="RajaFA", groupvars="CoralNoCoral")

plotbelgicaRajaFA <- ggplot(summarybelgicaRajaFA, aes(x=CoralNoCoral, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(RajaFA~CoralNoCoral, data=subbelgica)

#Plot AllSpeciesFA for Belgica Mound
summarybelgicaAllSpeciesFA <- summarySE(subbelgica, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotbelgicaAllSpeciesFA <- ggplot(summarybelgicaAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Total")))) +
                  ylim(0,400) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFA~CoralNoCoral, data=subbelgica)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 3)))
print(plotbelgicaLepidionFA, vp = vplayout(1, 1))
print(plotbelgicaSynaphobranchusFA, vp = vplayout(2, 1))
print(plotbelgicaSigmopsFA, vp = vplayout(3, 1))
print(plotbelgicaMoraFA, vp = vplayout(4, 1))
#print(plotbelgicaChimaeraFA, vp = vplayout(1, 1))
#print(plotbelgicaHelicolenusFA, vp = vplayout(2, 1))
#print(plotbelgicaLophiusFA, vp = vplayout(3, 1))
#print(plotbelgicaMolvaFA, vp = vplayout(4, 1))
#print(plotbelgicaBrosmeFA, vp = vplayout(1, 9))
#print(plotbelgicaHoplostethusFA, vp = vplayout(1, 10))
#print(plotbelgicaPhycisFA, vp = vplayout(1, 11))
#print(plotbelgicaPseudotriakisFA, vp = vplayout(1, 12))
#print(plotbelgicaRajaFA, vp = vplayout(1, 13))
#print(plotbelgicaAllSpeciesFA, vp = vplayout(9, 1))

#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data3, Area == "Hatton Bank")

#Plot LepidionFA for Hatton Bank
summaryhattonLepidionFA <- summarySE(subhatton, measurevar="LepidionFA", groupvars="CoralNoCoral")

plothattonLepidionFA <- ggplot(summaryhattonLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +               
    xlab("") +
     ylab("") +
     ylim(0,400) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LepidionFA~CoralNoCoral, data=subhatton)

#Plot SynaphobranchusFA for Hatton Bank
summaryhattonSynaphobranchusFA <- summarySE(subhatton, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plothattonSynaphobranchusFA <- ggplot(summaryhattonSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab("") +
     ylim(0,50) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SynaphobranchusFA~CoralNoCoral, data=subhatton)

#Plot SigmopsFA for Hatton Bank
summaryhattonSigmopsFA <- summarySE(subhatton, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plothattonSigmopsFA <- ggplot(summaryhattonSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab("") +
     ylim(0,100) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SigmopsFA~CoralNoCoral, data=subhatton)

#Plot MoraFA for Hatton Bank
summaryhattonMoraFA <- summarySE(subhatton, measurevar="MoraFA", groupvars="CoralNoCoral")

plothattonMoraFA <- ggplot(summaryhattonMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MoraFA~CoralNoCoral, data=subhatton)

#Plot ChimaeraFA for Hatton Bank
summaryhattonChimaeraFA <- summarySE(subhatton, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plothattonChimaeraFA <- ggplot(summaryhattonChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,50) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(ChimaeraFA~CoralNoCoral, data=subhatton)

#Plot HelicolenusFA for Hatton Bank
summaryhattonHelicolenusFA <- summarySE(subhatton, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plothattonHelicolenusFA <- ggplot(summaryhattonHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,200) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HelicolenusFA~CoralNoCoral, data=subhatton)

#Plot LophiusFA for Hatton Bank
summaryhattonLophiusFA <- summarySE(subhatton, measurevar="LophiusFA", groupvars="CoralNoCoral")

plothattonLophiusFA <- ggplot(summaryhattonLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LophiusFA~CoralNoCoral, data=subhatton)

#Plot MolvaFA for Hatton Bank
summaryhattonMolvaFA <- summarySE(subhatton, measurevar="MolvaFA", groupvars="CoralNoCoral")

plothattonMolvaFA <- ggplot(summaryhattonMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MolvaFA~CoralNoCoral, data=subhatton)

#Plot BrosmeFA for Hatton Bank
summaryhattonBrosmeFA <- summarySE(subhatton, measurevar="BrosmeFA", groupvars="CoralNoCoral")

plothattonBrosmeFA <- ggplot(summaryhattonBrosmeFA, aes(x=CoralNoCoral, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(BrosmeFA~CoralNoCoral, data=subhatton)

#Plot HoplostethusFA for Hatton Bank
summaryhattonHoplostethusFA <- summarySE(subhatton, measurevar="HoplostethusFA", groupvars="CoralNoCoral")

plothattonHoplostethusFA <- ggplot(summaryhattonHoplostethusFA, aes(x=CoralNoCoral, y=HoplostethusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoplostethusFA-se, ymax=HoplostethusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HoplostethusFA~CoralNoCoral, data=subhatton)

#Plot PhycisFA for Hatton Bank
summaryhattonPhycisFA <- summarySE(subhatton, measurevar="PhycisFA", groupvars="CoralNoCoral")

plothattonPhycisFA <- ggplot(summaryhattonPhycisFA, aes(x=CoralNoCoral, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(PhycisFA~CoralNoCoral, data=subhatton)

#Plot PseudotriakisFA for Hatton Bank
summaryhattonPseudotriakisFA <- summarySE(subhatton, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")

plothattonPseudotriakisFA <- ggplot(summaryhattonPseudotriakisFA, aes(x=CoralNoCoral, y=PseudotriakisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudotriakisFA-se, ymax=PseudotriakisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(PseudotriakisFA~CoralNoCoral, data=subhatton)

#Plot RajaFA for Hatton Bank
summaryhattonRajaFA <- summarySE(subhatton, measurevar="RajaFA", groupvars="CoralNoCoral")

plothattonRajaFA <- ggplot(summaryhattonRajaFA, aes(x=CoralNoCoral, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(RajaFA~CoralNoCoral, data=subhatton)

#Plot AllSpeciesFA for Hatton Bank
summaryhattonAllSpeciesFA <- summarySE(subhatton, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plothattonAllSpeciesFA <- ggplot(summaryhattonAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,400) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFA~CoralNoCoral, data=subhatton)

# Make plot with grid layout
print(plothattonLepidionFA, vp = vplayout(1, 2))
print(plothattonSynaphobranchusFA, vp = vplayout(2, 2))
print(plothattonSigmopsFA, vp = vplayout(3, 2))
print(plothattonMoraFA, vp = vplayout(4, 2))
#print(plothattonChimaeraFA, vp = vplayout(1, 2))
#print(plothattonHelicolenusFA, vp = vplayout(2, 2))
#print(plothattonLophiusFA, vp = vplayout(3, 2))
#print(plothattonMolvaFA, vp = vplayout(4, 2))
#print(plothattonBrosmeFA, vp = vplayout(2, 9))
#print(plothattonHoplostethusFA, vp = vplayout(2, 10))
#print(plothattonPhycisFA, vp = vplayout(2, 11))
#print(plothattonPseudotriakisFA, vp = vplayout(2, 12))
#print(plothattonRajaFA, vp = vplayout(2, 13))
#print(plothattonAllSpeciesFA, vp = vplayout(9, 2))

#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data3, Area == "Rockall Bank")

#Plot LepidionFA for Rockall Bank
summaryrockallLepidionFA <- summarySE(subrockall, measurevar="LepidionFA", groupvars="CoralNoCoral")

plotrockallLepidionFA <- ggplot(summaryrockallLepidionFA, aes(x=CoralNoCoral, y=LepidionFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFA-se, ymax=LepidionFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("")+
    ylab("") +
    ylim(0,400) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LepidionFA~CoralNoCoral, data=subrockall)

#Plot SynaphobranchusFA for Rockall Bank
summaryrockallSynaphobranchusFA <- summarySE(subrockall, measurevar="SynaphobranchusFA", groupvars="CoralNoCoral")

plotrockallSynaphobranchusFA <- ggplot(summaryrockallSynaphobranchusFA, aes(x=CoralNoCoral, y=SynaphobranchusFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFA-se, ymax=SynaphobranchusFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab("") +
     ylim(0,50) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SynaphobranchusFA~CoralNoCoral, data=subrockall)

#Plot SigmopsFA for Rockall Bank
summaryrockallSigmopsFA <- summarySE(subrockall, measurevar="SigmopsFA", groupvars="CoralNoCoral")

plotrockallSigmopsFA <- ggplot(summaryrockallSigmopsFA, aes(x=CoralNoCoral, y=SigmopsFA)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SigmopsFA-se, ymax=SigmopsFA+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab("") +
     ylim(0,100) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SigmopsFA~CoralNoCoral, data=subrockall)

#Plot MoraFA for Rockall Bank
summaryrockallMoraFA <- summarySE(subrockall, measurevar="MoraFA", groupvars="CoralNoCoral")

plotrockallMoraFA <- ggplot(summaryrockallMoraFA, aes(x=CoralNoCoral, y=MoraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFA-se, ymax=MoraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,25) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MoraFA~CoralNoCoral, data=subrockall)

#Plot ChimaeraFA for Rockall Bank
summaryrockallChimaeraFA <- summarySE(subrockall, measurevar="ChimaeraFA", groupvars="CoralNoCoral")

plotrockallChimaeraFA <- ggplot(summaryrockallChimaeraFA, aes(x=CoralNoCoral, y=ChimaeraFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFA-se, ymax=ChimaeraFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,50) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(ChimaeraFA~CoralNoCoral, data=subrockall)

#Plot HelicolenusFA for Rockall Bank
summaryrockallHelicolenusFA <- summarySE(subrockall, measurevar="HelicolenusFA", groupvars="CoralNoCoral")

plotrockallHelicolenusFA <- ggplot(summaryrockallHelicolenusFA, aes(x=CoralNoCoral, y=HelicolenusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFA-se, ymax=HelicolenusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,200) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HelicolenusFA~CoralNoCoral, data=subrockall)

#Plot LophiusFA for Rockall Bank
summaryrockallLophiusFA <- summarySE(subrockall, measurevar="LophiusFA", groupvars="CoralNoCoral")

plotrockallLophiusFA <- ggplot(summaryrockallLophiusFA, aes(x=CoralNoCoral, y=LophiusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFA-se, ymax=LophiusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LophiusFA~CoralNoCoral, data=subrockall)

#Plot MolvaFA for Rockall Bank
summaryrockallMolvaFA <- summarySE(subrockall, measurevar="MolvaFA", groupvars="CoralNoCoral")

plotrockallMolvaFA <- ggplot(summaryrockallMolvaFA, aes(x=CoralNoCoral, y=MolvaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFA-se, ymax=MolvaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,5) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MolvaFA~CoralNoCoral, data=subrockall)

#Plot BrosmeFA for Rockall Bank
summaryrockallBrosmeFA <- summarySE(subrockall, measurevar="BrosmeFA", groupvars="CoralNoCoral")

plotrockallBrosmeFA <- ggplot(summaryrockallBrosmeFA, aes(x=CoralNoCoral, y=BrosmeFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=BrosmeFA-se, ymax=BrosmeFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(BrosmeFA~CoralNoCoral, data=subrockall)

#Plot HoplostethusFA for Rockall Bank
summaryrockallHoplostethusFA <- summarySE(subrockall, measurevar="HoplostethusFA", groupvars="CoralNoCoral")

plotrockallHoplostethusFA <- ggplot(summaryrockallHoplostethusFA, aes(x=CoralNoCoral, y=HoplostethusFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HoplostethusFA-se, ymax=HoplostethusFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HoplostethusFA~CoralNoCoral, data=subrockall)

#Plot PhycisFA for Rockall Bank
summaryrockallPhycisFA <- summarySE(subrockall, measurevar="PhycisFA", groupvars="CoralNoCoral")

plotrockallPhycisFA <- ggplot(summaryrockallPhycisFA, aes(x=CoralNoCoral, y=PhycisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PhycisFA-se, ymax=PhycisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(PhycisFA~CoralNoCoral, data=subrockall)

#Plot PseudotriakisFA for Rockall Bank
summaryrockallPseudotriakisFA <- summarySE(subrockall, measurevar="PseudotriakisFA", groupvars="CoralNoCoral")

plotrockallPseudotriakisFA <- ggplot(summaryrockallPseudotriakisFA, aes(x=CoralNoCoral, y=PseudotriakisFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=PseudotriakisFA-se, ymax=PseudotriakisFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(PseudotriakisFA~CoralNoCoral, data=subrockall)

#Plot RajaFA for Rockall Bank
summaryrockallRajaFA <- summarySE(subrockall, measurevar="RajaFA", groupvars="CoralNoCoral")

plotrockallRajaFA <- ggplot(summaryrockallRajaFA, aes(x=CoralNoCoral, y=RajaFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=RajaFA-se, ymax=RajaFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(RajaFA~CoralNoCoral, data=subrockall)

#Plot AllSpeciesFA for Rockall Bank
summaryrockallAllSpeciesFA <- summarySE(subrockall, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotrockallAllSpeciesFA <- ggplot(summaryrockallAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,400) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFA~CoralNoCoral, data=subrockall)

# Make plot with grid layout
print(plotrockallLepidionFA, vp = vplayout(1, 3))
print(plotrockallSynaphobranchusFA, vp = vplayout(2, 3))
print(plotrockallSigmopsFA, vp = vplayout(3, 3))
print(plotrockallMoraFA, vp = vplayout(4, 3))
#print(plotrockallChimaeraFA, vp = vplayout(1, 3))
#print(plotrockallHelicolenusFA, vp = vplayout(2, 3))
#print(plotrockallLophiusFA, vp = vplayout(3, 3))
#print(plotrockallMolvaFA, vp = vplayout(4, 3))
#print(plotrockallAllSpeciesFA, vp = vplayout(9, 3))