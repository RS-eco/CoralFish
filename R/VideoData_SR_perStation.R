library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
#setwd("C:/Users/MBiber/Documents/Analysis_R")

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data <- read.delim("VideoData_SR_perStation.csv", sep=",",header=T)

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

#Plot Species Richness for Belgica Mound
summarybelgicaSR <- summarySE(subbelgica, measurevar="SR", groupvars="Category")

plotbelgicaSR <- ggplot(summarybelgicaSR, aes(x=Category, y=SR)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SR-se, ymax=SR+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +                
                  xlab("") +
                  ylab("Belgica Mound") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Region == "Hatton Bank")

#Plot Species Richness for Hatton Bank
summaryhattonSR <- summarySE(subhatton, measurevar="SR", groupvars="Category")

plothattonSR <- ggplot(summaryhattonSR, aes(x=Category, y=SR)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SR-se, ymax=SR+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +               
                  xlab("") +
                  ylab("Hatton Bank") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Region == "Rockall Bank")

#Plot Species Richness for Rockall Bank
summaryrockallSR <- summarySE(subrockall, measurevar="SR", groupvars="Category")

plotrockallSR <- ggplot(summaryrockallSR, aes(x=Category, y=SR)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SR-se, ymax=SR+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("Rockall Bank") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot Species Richness for All Regions
summarySR <- summarySE(data, measurevar="SR", groupvars="Category")

plotSR <- ggplot(summarySR, aes(x=Category, y=SR)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=SR-se, ymax=SR+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("All Regions") +
                  ylim(0,6) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

print(plotbelgicaSR, vp = vplayout(1, 1))
print(plothattonSR, vp = vplayout(1, 2))
print(plotrockallSR, vp = vplayout(2, 1))
print(plotSR, vp = vplayout(2, 2))


# Kruskal-Wallis Test
kruskal.test(SR~Category, data=subbelgica)

kruskal.test(SR~Category, data=subhatton)

kruskal.test(SR~Category, data=subrockall)

kruskal.test(SR~Category, data=data)
