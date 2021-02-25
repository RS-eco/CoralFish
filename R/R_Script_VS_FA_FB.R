library(ggplot2)
library(grid)
library(plyr)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm),
                      max  = max   (xx[,col], na.rm=na.rm),
                      min  = min   (xx[,col], na.rm=na.rm)
                      )
                 },
                 measurevar,
                 na.rm
                 )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


#setwd("C:/Users/Matthias/Documents/Analysis_R")
data <- read.delim("VideoData_FA_FB.csv", sep=",",header=T)


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

#Kruskall-Wallis Test, non-paramteric rank test
kw_FA_belgica<-kruskal.test(AllSpeciesFA~CoralNoCoral, data=subbelgica)
kw_FA_belgica$star <- "" 
kw_FA_belgica$star[kw_FA_belgica$p.value <= .05]  <- "*" 
kw_FA_belgica$star[kw_FA_belgica$p.value <= .01]  <- "**" 
kw_FA_belgica$star[kw_FA_belgica$p.value <= .001] <- "***"

#Plot AllSpeciesFA for Belgica Mound
summarybelgicaAllSpeciesFA <- summarySE(subbelgica, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotbelgicaAllSpeciesFA <- ggplot(summarybelgicaAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,500) +
                  geom_text(aes(label=kw_FA_belgica$star), colour="black", vjust=-2, size=10) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_FB_belgica<-kruskal.test(AllSpeciesFB~CoralNoCoral, data=subbelgica)
kw_FB_belgica$star <- "" 
kw_FB_belgica$star[kw_FB_belgica$p.value <= .05]  <- "*" 
kw_FB_belgica$star[kw_FB_belgica$p.value <= .01]  <- "**" 
kw_FB_belgica$star[kw_FB_belgica$p.value <= .001] <- "***"

#Plot AllSpeciesFB for Belgica Mound
summarybelgicaAllSpeciesFB <- summarySE(subbelgica, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plotbelgicaAllSpeciesFB <- ggplot(summarybelgicaAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Belgica Mound") +
                  xlab("Coral framework") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0,200) +
                  geom_text(aes(label=kw_FB_belgica$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Hatton Bank

#Subset data for Hatton Bank
subhatton <- subset(data, Area == "Hatton Bank")

#Kruskall-Wallis Test, non-paramteric rank test
kw_FA_hatton<-kruskal.test(AllSpeciesFA~CoralNoCoral, data=subhatton)
kw_FA_hatton$star <- "" 
kw_FA_hatton$star[kw_FA_hatton$p.value <= .05]  <- "*" 
kw_FA_hatton$star[kw_FA_hatton$p.value <= .01]  <- "**" 
kw_FA_hatton$star[kw_FA_hatton$p.value <= .001] <- "***"

#Plot AllSpeciesFA for Hatton Bank
summaryhattonAllSpeciesFA <- summarySE(subhatton, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plothattonAllSpeciesFA <- ggplot(summaryhattonAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,500) +
                  geom_text(aes(label=kw_FA_hatton$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,400,100)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_FB_hatton<-kruskal.test(AllSpeciesFB~CoralNoCoral, data=subhatton)
kw_FB_hatton$star <- "" 
kw_FB_hatton$star[kw_FB_hatton$p.value <= .05]  <- "*" 
kw_FB_hatton$star[kw_FB_hatton$p.value <= .01]  <- "**" 
kw_FB_hatton$star[kw_FB_hatton$p.value <= .001] <- "***"

#Plot AllSpeciesFB for Hatton Bank
summaryhattonAllSpeciesFB <- summarySE(subhatton, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plothattonAllSpeciesFB <- ggplot(summaryhattonAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Hatton Bank") +
                  xlab("Coral framework") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0,200) +
                  geom_text(aes(label=kw_FB_hatton$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Rockall Bank

#Subset data for Rockall Bank
subrockall <- subset(data, Area == "Rockall Bank")

#Kruskall-Wallis Test, non-paramteric rank test
kw_FA_rockall<-kruskal.test(AllSpeciesFA~CoralNoCoral, data=subrockall)
kw_FA_rockall$star <- "" 
kw_FA_rockall$star[kw_FA_rockall$p.value <= .05]  <- "*" 
kw_FA_rockall$star[kw_FA_rockall$p.value <= .01]  <- "**" 
kw_FA_rockall$star[kw_FA_rockall$p.value <= .001] <- "***"

#Plot AllSpeciesFA for Rockall Bank
summaryrockallAllSpeciesFA <- summarySE(subrockall, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotrockallAllSpeciesFA <- ggplot(summaryrockallAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework")  +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,500) +
                  geom_text(aes(label=kw_FA_rockall$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,400,100)) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_FB_rockall<-kruskal.test(AllSpeciesFB~CoralNoCoral, data=subrockall)
kw_FB_rockall$star <- "" 
kw_FB_rockall$star[kw_FB_rockall$p.value <= .05]  <- "*" 
kw_FB_rockall$star[kw_FB_rockall$p.value <= .01]  <- "**" 
kw_FB_rockall$star[kw_FB_rockall$p.value <= .001] <- "***"

#Plot AllSpeciesFB for Rockall Bank
summaryrockallAllSpeciesFB <- summarySE(subrockall, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plotrockallAllSpeciesFB <- ggplot(summaryrockallAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="Rockall Bank") +
                  xlab("Coral framework") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0, 200) +
                  geom_text(aes(label=kw_FB_rockall$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#All Areas

#Kruskall-Wallis Test, non-paramteric rank test
kw_FA_data<-kruskal.test(AllSpeciesFA~CoralNoCoral, data=data)
kw_FA_data$star <- "" 
kw_FA_data$star[kw_FA_data$p.value <= .05]  <- "*" 
kw_FA_data$star[kw_FA_data$p.value <= .01]  <- "**" 
kw_FA_data$star[kw_FA_data$p.value <= .001] <- "***"

#Plot AllSpeciesFA for All Areas
summarydataAllSpeciesFA <- summarySE(data, measurevar="AllSpeciesFA", groupvars="CoralNoCoral")

plotdataAllSpeciesFA <- ggplot(summarydataAllSpeciesFA, aes(x=CoralNoCoral, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework")  +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,500) +
                  geom_text(aes(label=kw_FA_data$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,400,100)) +
                  #scale_x_discrete(limits=c("non-coral","coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

#Kruskall-Wallis Test, non-paramteric rank test
kw_FB_data<-kruskal.test(AllSpeciesFB~CoralNoCoral, data=data)
kw_FB_data$star <- "" 
kw_FB_data$star[kw_FB_areas$p.value <= .05]  <- "*" 
kw_FB_data$star[kw_FB_areas$p.value <= .01]  <- "**" 
kw_FB_data$star[kw_FB_areas$p.value <= .001] <- "***"

#Plot AllSpeciesFB for All Areas
summarydataAllSpeciesFB <- summarySE(data, measurevar="AllSpeciesFB", groupvars="CoralNoCoral")

plotdataAllSpeciesFB <- ggplot(summarydataAllSpeciesFB, aes(x=CoralNoCoral, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  opts(title="All Areas") +
                  xlab("Coral framework") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0, 200) +
                  geom_text(aes(label=kw_FB_data$star), colour="black", vjust=-2, size=10) +
                  #scale_y_continuous(breaks=seq(0,150,50)) +
                  #scale_x_discrete(limits=c("No Coral","Coral")) + #Reorders x factors on the graph
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Make plot with grid layout (2*3)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 4)))

print(plotbelgicaAllSpeciesFA, vp = vplayout(1, 1))
print(plotbelgicaAllSpeciesFB, vp = vplayout(2, 1))

print(plothattonAllSpeciesFA, vp = vplayout(1, 2))
print(plothattonAllSpeciesFB, vp = vplayout(2, 2))

print(plotrockallAllSpeciesFA, vp = vplayout(1, 3))
print(plotrockallAllSpeciesFB, vp = vplayout(2, 3))

print(plotdataAllSpeciesFA, vp = vplayout(1, 4))
print(plotdataAllSpeciesFB, vp = vplayout(2, 4))