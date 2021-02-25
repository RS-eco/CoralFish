library(ggplot2)
library(grid)
library(plyr)

#Location of summarise Script
#setwd("C:/Users/Matthias/Documents/Analysis_R")
source("R_Function_SummariseData.R")

#setwd("C:/Users/Matthias/Documents/Analysis_R")
data <- read.delim("Summary_VS_FA&FB.csv", sep=",",header=T)

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

# Belgica Mound

#Subset data for Belgica Mound
subbelgica <- subset(data, Area == "Belgica Mound")

#Plot AllSpeciesFA&FB for Belgica Mound

plotbelgicaFA_FB <- ggplot(subbelgica, aes(x=CoralNoCoral, y=Mean, fill=FA_or_FB)) +
  geom_bar(stat='identity', position=position_dodge(0.9), colour="black") + 
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(0.9)) +
                  #opts(title="") + 
                  xlab("Coral framework") +
                  ylab("Fish abundance & biomass") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Subset data for Hatton Bank
subhatton <- subset(data, Area == "Hatton Bank")

#Plot AllSpeciesFA&FB for Hatton Bank

plothattonFA_FB <- ggplot(subhatton, aes(x=CoralNoCoral, y=Mean, fill=FA_or_FB)) +
  geom_bar(stat='identity', position=position_dodge(0.9), colour="black") + 
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(0.9)) +
                  #opts(title="") + 
                  xlab("Coral framework") +
                  ylab("Fish abundance & biomass") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Subset data for Rockall Bank
subrockall <- subset(data, Area == "Rockall Bank")

#Plot AllSpeciesFA&FB for Rockall Bank

plotrockallFA_FB <- ggplot(subrockall, aes(x=CoralNoCoral, y=Mean, fill=FA_or_FB)) +
  geom_bar(stat='identity', position=position_dodge(0.9), colour="black") + 
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(0.9)) +
                  #opts(title="") + 
                  xlab("Coral framework") +
                  ylab("Fish abundance & biomass") +
                  #scale_y_continuous(breaks=-1:20000*1000) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Make plot with grid layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(plotbelgicaFA_FB, vp = vplayout(1, 3))
print(plothattonFA_FB, vp = vplayout(1, 2))
print(plotrockallFA_FB, vp = vplayout(1, 1))