library(ggplot2)

#setwd("C:/Users/Matthias/Documents/Analysis_R")
data <- read.delim("VideoData_RelativeFA.csv", sep=",",header=T)

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

#Sort data
#data<-data[order(data$Area, data$Fish.abundance, decreasing = TRUE),] #Orders the table!!
#Sorting of data is not effecting the graph

#Create horizontal stacked bar chart of % relative fish species abundance
ggplot(data, aes(x=Fish.species, y=Fish.abundance,  fill=Area)) + 
  geom_bar(colour="black") +  #Makes it stacked with black frame
  scale_fill_manual(values=c("green", "blue", "red")) + #Changes colour of fillings
  coord_flip() + #Switches bar chart to horizontal orientation
  #scale_fill_hue(name="Sex of payer") +      # Set legend title
  #xlab("Time of day") + ylab("Total bill") + # Set axis labels
  #opts(title="Average bill for 2 people") +  # Set title
  geom_hline(yintercept=0, width=.5) + #add horizontal 0 line
  theme_map
