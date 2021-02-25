library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/BarGraph")

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

#Fish abundance
summaryAllSpeciesFA <- summarySE(data, measurevar="AllFA", groupvars="Category")

plotAllSpeciesFA <- ggplot(summaryAllSpeciesFA, aes(x=Category, y=AllFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFA-se, ymax=AllFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework (presence/absence)") +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,250) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Fish biomass
summaryAllSpeciesFB <- summarySE(data, measurevar="AllFB", groupvars="Category")

plotAllSpeciesFB <- ggplot(summaryAllSpeciesFB, aes(x=Category, y=AllFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllFB-se, ymax=AllFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Coral framework (presence/absence)") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0,100) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Kruskal-Wallis Test
kruskal.test(AllFA~Category, data=data)
kruskal.test(AllFB~Category, data=data)
 
# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

print(plotAllSpeciesFA, vp = vplayout(1, 1))
print(plotAllSpeciesFB, vp = vplayout(1, 2))






