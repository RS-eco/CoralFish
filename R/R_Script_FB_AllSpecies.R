library(ggplot2)
library(grid)
library(plyr)

#Location of summarise Script
#setwd("C:/Users/MBiber/Documents/Analysis_R")
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

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

#setwd("C:/Users/MBiber/Documents/Analysis_R")
data <- read.delim("VideoData_FA_FB.csv", sep=",",header=T)


# Belgica Mound

#Subset data for Belgica Mound
subbelgica <- subset(data, Area == "Belgica Mound")

#Plot LepidionFB for Belgica Mound
summarybelgicaLepidionFB <- summarySE(subbelgica, measurevar="LepidionFB", groupvars="CoralNoCoral")

plotbelgicaLepidionFB <- ggplot(summarybelgicaLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +                
    xlab("") +
    ylab(expression(paste(italic("Lepidion eques")))) +
    ylim(0, 40) + 
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LepidionFB~CoralNoCoral, data=subbelgica)

#Kruskal-Wallis Test - non-parametric test for more than 2 samples
#> kruskal.test(LepidionFA ~ CoralNoCoral, data =data3)

#Plot SynaphobranchusFB for Belgica Mound
summarybelgicaSynaphobranchusFB <- summarySE(subbelgica, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plotbelgicaSynaphobranchusFB <- ggplot(summarybelgicaSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
    ylab(expression(paste(italic("Synaphobranchus kaupii")))) +
    ylim(0, 0.8) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SynaphobranchusFB~CoralNoCoral, data=subbelgica)

#Plot MoraFA for Belgica Mound
summarybelgicaMoraFB <- summarySE(subbelgica, measurevar="MoraFB", groupvars="CoralNoCoral")

plotbelgicaMoraFB <- ggplot(summarybelgicaMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
    xlab("") +
      ylab(expression(paste(italic("Mora moro")))) +
      ylim(0, 100) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MoraFB~CoralNoCoral, data=subbelgica)

#Plot ChimaeraFB for Belgica Mound
summarybelgicaChimaeraFB <- summarySE(subbelgica, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plotbelgicaChimaeraFB <- ggplot(summarybelgicaChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
    xlab("") +
     ylab(expression(paste(italic("Chimaera monstrosa")))) +
     ylim(0, 40) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(ChimaeraFB~CoralNoCoral, data=subbelgica)

#Plot HelicolenusFB for Belgica Mound
summarybelgicaHelicolenusFB <- summarySE(subbelgica, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plotbelgicaHelicolenusFB <- ggplot(summarybelgicaHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
    xlab("") +
     ylab(expression(paste(italic("Helicolenus dactylopterus")))) +
     ylim(0, 50) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HelicolenusFB~CoralNoCoral, data=subbelgica)

#Plot LophiusFA for Belgica Mound
summarybelgicaLophiusFB <- summarySE(subbelgica, measurevar="LophiusFB", groupvars="CoralNoCoral")

plotbelgicaLophiusFB <- ggplot(summarybelgicaLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Lophius piscatorius")))) +
                  ylim(0, 150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LophiusFB~CoralNoCoral, data=subbelgica)

#Plot MolvaFA for Belgica Mound
summarybelgicaMolvaFB <- summarySE(subbelgica, measurevar="MolvaFB", groupvars="CoralNoCoral")

plotbelgicaMolvaFB <- ggplot(summarybelgicaMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Molva dypterygia")))) +
                  ylim(0, 10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MolvaFB~CoralNoCoral, data=subbelgica)

#Plot AllSpeciesFB for Belgica Mound
summarybelgicaAllSpeciesFB <- summarySE(subbelgica, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plotbelgicaAllSpeciesFB <- ggplot(summarybelgicaAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab(expression(paste(italic("Total")))) +
                  ylim(0, 200) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFB~CoralNoCoral, data=subbelgica)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 3)))
print(plotbelgicaLepidionFB, vp = vplayout(1, 1))
print(plotbelgicaSynaphobranchusFB, vp = vplayout(2, 1))
print(plotbelgicaMoraFB, vp = vplayout(3, 1))
print(plotbelgicaChimaeraFB, vp = vplayout(4, 1))
#print(plotbelgicaHelicolenusFB, vp = vplayout(1, 1))
#print(plotbelgicaLophiusFB, vp = vplayout(2, 1))
#print(plotbelgicaMolvaFB, vp = vplayout(3, 1))
#print(plotbelgicaAllSpeciesFB, vp = vplayout(8, 1))

#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Area == "Hatton Bank")

#Plot LepidionFB for Hatton Bank
summaryhattonLepidionFB <- summarySE(subhatton, measurevar="LepidionFB", groupvars="CoralNoCoral")

plothattonLepidionFB <- ggplot(summaryhattonLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +              
    xlab("") +
     ylab("") +
     ylim(0, 40) + 
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LepidionFB~CoralNoCoral, data=subhatton)

#Plot SynaphobranchusFB for Hatton Bank
summaryhattonSynaphobranchusFB <- summarySE(subhatton, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plothattonSynaphobranchusFB <- ggplot(summaryhattonSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab("") +
     ylim(0, 0.8) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SynaphobranchusFB~CoralNoCoral, data=subhatton)

#Plot MoraFB for Hatton Bank
summaryhattonMoraFB <- summarySE(subhatton, measurevar="MoraFB", groupvars="CoralNoCoral")

plothattonMoraFB <- ggplot(summaryhattonMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MoraFB~CoralNoCoral, data=subhatton)

#Plot ChimaeraFB for Hatton Bank
summaryhattonChimaeraFB <- summarySE(subhatton, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plothattonChimaeraFB <- ggplot(summaryhattonChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 40) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(ChimaeraFA~CoralNoCoral, data=subhatton)

#Plot HelicolenusFB for Hatton Bank
summaryhattonHelicolenusFB <- summarySE(subhatton, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plothattonHelicolenusFB <- ggplot(summaryhattonHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 50) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HelicolenusFB~CoralNoCoral, data=subhatton)

#Plot LophiusFA for Hatton Bank
summaryhattonLophiusFB <- summarySE(subhatton, measurevar="LophiusFB", groupvars="CoralNoCoral")

plothattonLophiusFB <- ggplot(summaryhattonLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LophiusFB~CoralNoCoral, data=subhatton)

#Plot MolvaFB for Hatton Bank
summaryhattonMolvaFB <- summarySE(subhatton, measurevar="MolvaFB", groupvars="CoralNoCoral")

plothattonMolvaFB <- ggplot(summaryhattonMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MolvaFB~CoralNoCoral, data=subhatton)

#Plot AllSpeciesFB for Hatton Bank
summaryhattonAllSpeciesFB <- summarySE(subhatton, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plothattonAllSpeciesFB <- ggplot(summaryhattonAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 200) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFB~CoralNoCoral, data=subhatton)

# Make plot with grid layout
print(plothattonLepidionFB, vp = vplayout(1, 2))
print(plothattonSynaphobranchusFB, vp = vplayout(2, 2))
print(plothattonMoraFB, vp = vplayout(3, 2))
print(plothattonChimaeraFB, vp = vplayout(4, 2))
#print(plothattonHelicolenusFB, vp = vplayout(1, 2))
#print(plothattonLophiusFB, vp = vplayout(2, 2))
#print(plothattonMolvaFB, vp = vplayout(3, 2))
#print(plothattonAllSpeciesFB, vp = vplayout(8, 2))

#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Area == "Rockall Bank")

#Plot LepidionFB for Rockall Bank
summaryrockallLepidionFB <- summarySE(subrockall, measurevar="LepidionFB", groupvars="CoralNoCoral")

plotrockallLepidionFB <- ggplot(summaryrockallLepidionFB, aes(x=CoralNoCoral, y=LepidionFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=LepidionFB-se, ymax=LepidionFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("")+
    ylab("") +
    ylim(0, 40) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LepidionFB~CoralNoCoral, data=subrockall)

#Plot SynaphobranchusFB for Rockall Bank
summaryrockallSynaphobranchusFB <- summarySE(subrockall, measurevar="SynaphobranchusFB", groupvars="CoralNoCoral")

plotrockallSynaphobranchusFB <- ggplot(summaryrockallSynaphobranchusFB, aes(x=CoralNoCoral, y=SynaphobranchusFB)) +
    geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
    geom_errorbar(aes(ymin=SynaphobranchusFB-se, ymax=SynaphobranchusFB+se),
                  width=.1,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("") +
     ylab("") +
     ylim(0, 0.8) +
    #scale_y_continuous(breaks=-1:20000*1000) +
    #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
    geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
    theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(SynaphobranchusFB~CoralNoCoral, data=subrockall)

#Plot MoraFB for Rockall Bank
summaryrockallMoraFB <- summarySE(subrockall, measurevar="MoraFB", groupvars="CoralNoCoral")

plotrockallMoraFB <- ggplot(summaryrockallMoraFB, aes(x=CoralNoCoral, y=MoraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MoraFB-se, ymax=MoraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MoraFB~CoralNoCoral, data=subrockall)

#Plot ChimaeraFB for Rockall Bank
summaryrockallChimaeraFB <- summarySE(subrockall, measurevar="ChimaeraFB", groupvars="CoralNoCoral")

plotrockallChimaeraFB <- ggplot(summaryrockallChimaeraFB, aes(x=CoralNoCoral, y=ChimaeraFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=ChimaeraFB-se, ymax=ChimaeraFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 40) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(ChimaeraFB~CoralNoCoral, data=subrockall)

#Plot HelicolenusFB for Rockall Bank
summaryrockallHelicolenusFB <- summarySE(subrockall, measurevar="HelicolenusFB", groupvars="CoralNoCoral")

plotrockallHelicolenusFB <- ggplot(summaryrockallHelicolenusFB, aes(x=CoralNoCoral, y=HelicolenusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=HelicolenusFB-se, ymax=HelicolenusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 50) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(HelicolenusFB~CoralNoCoral, data=subrockall)

#Plot LophiusFB for Rockall Bank
summaryrockallLophiusFB <- summarySE(subrockall, measurevar="LophiusFB", groupvars="CoralNoCoral")

plotrockallLophiusFB <- ggplot(summaryrockallLophiusFB, aes(x=CoralNoCoral, y=LophiusFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=LophiusFB-se, ymax=LophiusFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 150) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(LophiusFB~CoralNoCoral, data=subrockall)

#Plot MolvaFB for Rockall Bank
summaryrockallMolvaFB <- summarySE(subrockall, measurevar="MolvaFB", groupvars="CoralNoCoral")

plotrockallMolvaFB <- ggplot(summaryrockallMolvaFB, aes(x=CoralNoCoral, y=MolvaFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=MolvaFB-se, ymax=MolvaFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 10) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(MolvaFB~CoralNoCoral, data=subrockall)

#Plot AllSpeciesFB for Rockall Bank
summaryrockallAllSpeciesFB <- summarySE(subrockall, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plotrockallAllSpeciesFB <- ggplot(summaryrockallAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0, 200) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFB~CoralNoCoral, data=subrockall)

# Make plot with grid layout
print(plotrockallLepidionFB, vp = vplayout(1, 3))
print(plotrockallSynaphobranchusFB, vp = vplayout(2, 3))
print(plotrockallMoraFB, vp = vplayout(3, 3))
print(plotrockallChimaeraFB, vp = vplayout(4, 3))
#print(plotrockallHelicolenusFB, vp = vplayout(1, 3))
#print(plotrockallLophiusFB, vp = vplayout(2, 3))
#print(plotrockallMolvaFB, vp = vplayout(3, 3))
#print(plotrockallAllSpeciesFB, vp = vplayout(8, 3))