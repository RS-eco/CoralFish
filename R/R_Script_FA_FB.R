library(ggplot2)
library(grid)
library(plyr)

#Location of summarise Script
#setwd("C:/Users/Matthias/Documents/Analysis_R")
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

#setwd("C:/Users/Matthias/Documents/Analysis_R")
data3 <- read.delim("VideoData_FA_FB.csv", sep=",",header=T)

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

#Plot AllSpeciesFA for Belgica Mound
summarybelgicaAllSpeciesFA <- summarySE(subbelgica, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotbelgicaAllSpeciesFA <- ggplot(summarybelgicaAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,500) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFA~CoralNoCoral, data=subbelgica)

#Kruskall-Wallis Test, non-paramteric rank test
kruskal.test(AllSpeciesFA~CoralNoCoral, data=subbelgica)


#Plot AllSpeciesFB for Belgica Mound
summarybelgicaAllSpeciesFB <- summarySE(subbelgica, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plotbelgicaAllSpeciesFB <- ggplot(summarybelgicaAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0,200) +
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFB~CoralNoCoral, data=subbelgica)

#Kruskall-Wallis Test, non-paramteric rank test
kruskal.test(AllSpeciesFB~CoralNoCoral, data=subbelgica)

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 3)))
print(plotbelgicaAllSpeciesFA, vp = vplayout(1, 1))
print(plotbelgicaAllSpeciesFB, vp = vplayout(2, 1))

#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data3, Area == "Hatton Bank")

#Plot AllSpeciesFA for Hatton Bank
summaryhattonAllSpeciesFA <- summarySE(subhatton, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plothattonAllSpeciesFA <- ggplot(summaryhattonAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,500) +
                  #scale_y_continuous(breaks=seq(0,400,100)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFA~CoralNoCoral, data=subhatton)

#Kruskall-Wallis Test, non-paramteric rank test
kruskal.test(AllSpeciesFA~CoralNoCoral, data=subhatton)

#Plot AllSpeciesFB for Hatton Bank
summaryhattonAllSpeciesFB <- summarySE(subhatton, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plothattonAllSpeciesFB <- ggplot(summaryhattonAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("") +
                  ylim(0,200) +
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFB~CoralNoCoral, data=subhatton)

#Kruskall-Wallis Test, non-paramteric rank test
kruskal.test(AllSpeciesFB~CoralNoCoral, data=subhatton)

# Arrange and display the plots into a 2x3 grid
print(plothattonAllSpeciesFA, vp = vplayout(1, 2))
print(plothattonAllSpeciesFB, vp = vplayout(2, 2))

#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data3, Area == "Rockall Bank")

#Plot AllSpeciesFA for Rockall Bank
summaryrockallAllSpeciesFA <- summarySE(subrockall, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotrockallAllSpeciesFA <- ggplot(summaryrockallAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("")  +
                  ylab("") +
                  ylim(0,500) +
                  #scale_y_continuous(breaks=seq(0,400,100)) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFA~CoralNoCoral, data=subrockall)

#Kruskall-Wallis Test, non-paramteric rank test
kruskal.test(AllSpeciesFA~CoralNoCoral, data=subrockall)

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
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#plotrockallAllSpeciesFB <- plotrockallAllSpeciesFB + geom_point(aes(y=y, x=x)) 
#plotrockallAllSpeciesFB <- plotrockallAllSpeciesFB + facet_wrap(~ AllSpeciesFB, scales = "free") 
#plotrockallAllSpeciesFB <- plotrockallAllSpeciesFB + ylim(0, 150) 


# Mann-Whitney U-Test - non-parametric test for 2 samples
wilcox.test(AllSpeciesFB~CoralNoCoral, data=subrockall)

#Kruskall-Wallis Test, non-paramteric rank test
kruskal.test(AllSpeciesFB~CoralNoCoral, data=subrockall)

# Arrange and display the plots into a 2x3 grid
print(plotrockallAllSpeciesFA, vp = vplayout(1, 3))
print(plotrockallAllSpeciesFB, vp = vplayout(2, 3))

