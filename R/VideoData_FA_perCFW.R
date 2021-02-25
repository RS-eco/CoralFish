library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
#setwd("C:/Users/MBiber/Documents/Analysis_R")

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data <- read.delim("VideoData_FA_FB_perCFW.csv", sep=",",header=T)

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

#Plot Fish abundance for Belgica Mound
summarybelgicaFA <- summarySE(subbelgica, measurevar="FishAbundance_ha", groupvars="Category")

plotbelgicaFA <- ggplot(summarybelgicaFA, aes(x=Category, y=FishAbundance_ha)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=FishAbundance_ha-se, ymax=FishAbundance_ha+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +                
                  xlab("") +
                  ylab("Belgica Mound") +
                  ylim(0,900) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Region == "Hatton Bank")

#Plot Species Richness for Hatton Bank
summaryhattonFA <- summarySE(subhatton, measurevar="FishAbundance_ha", groupvars="Category")

plothattonFA <- ggplot(summaryhattonFA, aes(x=Category, y=FishAbundance_ha)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=FishAbundance_ha-se, ymax=FishAbundance_ha+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +               
                  xlab("") +
                  ylab("Hatton Bank") +
                  ylim(0,900) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Region == "Rockall Bank")

#Plot Species Richness for Rockall Bank
summaryrockallFA <- summarySE(subrockall, measurevar="FishAbundance_ha", groupvars="Category")

plotrockallFA <- ggplot(summaryrockallFA, aes(x=Category, y=FishAbundance_ha)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=FishAbundance_ha-se, ymax=FishAbundance_ha+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("Rockall Bank") +
                  ylim(0,900) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Plot Species Richness for All Regions
summaryFA <- summarySE(data, measurevar="FishAbundance_ha", groupvars="Category")

plotFA <- ggplot(summaryFA, aes(x=Category, y=FishAbundance_ha)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=FishAbundance_ha-se, ymax=FishAbundance_ha+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("") +
                  ylab("All Regions") +
                  ylim(0,900) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

print(plotbelgicaFA, vp = vplayout(1, 1))
print(plothattonFA, vp = vplayout(1, 2))
print(plotrockallFA, vp = vplayout(2, 1))
print(plotFA, vp = vplayout(2, 2))


# Kruskal-Wallis Test
kruskal.test(FishAbundance_ha~Category, data=subbelgica)

kruskal.test(FishAbundance_ha~Category, data=subhatton)

kruskal.test(FishAbundance_ha~Category, data=subrockall)

kruskal.test(FishAbundance_ha~Category, data=data)
