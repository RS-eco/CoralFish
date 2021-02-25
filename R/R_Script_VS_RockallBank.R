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


#Subset data for Belgica Mound
subrockall <- subset(data, Area == "Rockall Bank")


#Plot CFW per Area
summaryCFW <- summarySE(subrockall, measurevar="X.CFW", groupvars="Habitat")

plotCFW <- ggplot(summaryCFW, aes(x=Habitat, y=X.CFW)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=X.CFW-se, ymax=X.CFW+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Habitat") +
                  ylab("% Coral framework") +
                  ylim(0,100) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Plot AllSpeciesFA per Area
summarySpeciesFA <- summarySE(subrockall, measurevar="AllSpeciesFA", groupvars="Habitat")

plotSpeciesFA <- ggplot(summarySpeciesFA, aes(x=Habitat, y=AllSpeciesFA)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFA-se, ymax=AllSpeciesFA+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Habitat") +
                  ylab("Fish abundance (ind. ha-1)") +
                  ylim(0,400) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map


#Plot AllSpeciesFB per Area
summarySpeciesFB <- summarySE(subrockall, measurevar="AllSpeciesFB", groupvars="Habitat")

plotSpeciesFB <- ggplot(summarySpeciesFB, aes(x=Habitat, y=AllSpeciesFB)) +
  geom_bar(position=position_dodge(.9), colour="black", fill="#FFFFFF", width=.5) +
  geom_errorbar(aes(ymin=AllSpeciesFB-se, ymax=AllSpeciesFB+se),
                width=.1,                    # Width of the error bars
                position=position_dodge(.9)) +
                  xlab("Habitat") +
                  ylab("Fish biomass (kg ha-1)") +
                  ylim(0,150) +
                  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
                  theme_map

# Make plot with grid layout (1*3)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))

print(plotCFW, vp = vplayout(1, 1))
print(plotSpeciesFA, vp = vplayout(1, 2))
print(plotSpeciesFB, vp = vplayout(1, 3))

#Kruskall-Wallis Test, non-parametric rank test
#kruskal.test(AllSpeciesFA~Habitat, data=data)