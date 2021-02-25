library(ggplot2)
library(grid)
library(plyr)
library(reshape)

#Set working directory
#setwd("C:/Users/MBiber/Documents/Analysis_R")

#Location of summarise Script
source("R_Function_SummariseData.R")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data <- read.delim("VideoData_FA_FB_CFW_perStation.csv", sep=",",header=T)

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


#Plot Fish abundance
summaryFA <- summarySE(data, measurevar="TotalFishAbundance.ha", groupvars="Category")

plotFA <- ggplot(summaryFA, aes(x=Category, y=TotalFishAbundance.ha)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=TotalFishAbundance.ha-se, ymax=TotalFishAbundance.ha+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +                
                  xlab("% Coral framework") +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,300) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Plot Fish biomass
summaryFB <- summarySE(data, measurevar="TotalFishBiomass.kg", groupvars="Category")

plotFB <- ggplot(summaryFB, aes(x=Category, y=TotalFishBiomass.kg)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=TotalFishBiomass.kg-se, ymax=TotalFishBiomass.kg+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("% Coral framework") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0,300) +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))

print(plotFA, vp = vplayout(1, 1))
print(plotFB, vp = vplayout(2, 1))

# Kruskal-Wallis Test
kruskal.test(TotalFishAbundance.ha~Category, data=data)

kruskal.test(TotalFishBiomass.kg~Category, data=data)
