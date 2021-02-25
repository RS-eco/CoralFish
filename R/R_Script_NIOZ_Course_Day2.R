#Load data
peake <- read.delim("~/03_Science/Methods/Statistics/R/R_course_NIOZ/Day 01/peake.txt")

View(peake)

#Plot data
plot(peake$SPECIES ~ peake$AREA, ylab = "Number of species", xlab = "Mussel clump area")

#A Loess smoother is automatically added to the plot
scatter.smooth(peake$AREA, peake$SPECIES, ylab = "species number", xlab = "mussel clump area")

#Log transform relationship
larea <- log(peake$AREA, 10)
lspecies <- log(peake$SPECIES, 10)
plot(lspecies ~ larea, ylab = "log species number", xlab = "log mussel clump area")

#Try linear model, only takes error of species into account not of area, so have to use different model
lm.peake <- lm(lspecies ~ larea)
summary(lm.peake)

#Run Anova on linear model
anova(lm.peake)

#Plot linear model
par(mfrow = c(2, 2))
plot(lm.peake)
#Plots of linear models shows the data points that are out of range!!!

#draw line of best fit
plot(lspecies ~ larea, ylab = "log species number", xlab = "log mussel clump area")
abline(lm.peake, col="red")

#Identify outlier manually using the log-transformed data
identify(larea ~ lspecies)

#Exclude certain data range
peake_ex<-peake[peake$LSPECIES>.6,]

#Exclude 1st row
peake_ex<-peake[-1,]

#Load outliers package
library(outliers)

#Details of outlier
outlier(peake)

#Remove outliers
peake_noOut<-rm.outlier(peake)

#Plot data without outliers
plot(peake_noOut$SPECIES ~ peake_noOut$AREA, ylab = "Number of species", xlab = "Mussel clump area")

#Log transform relationship
larea_noOut <- log(peake_noOut$AREA, 10)
lspecies_noOut <- log(peake_noOut$SPECIES, 10)
plot(lspecies_noOut ~ larea_noOut, ylab = "log species number", xlab = "log mussel clump area")


#Linear model II regression
library(lmodel2)
lm.peake_m2 <- lmodel2(lspecies_noOut ~ larea_noOut, range.y="relative", range.x="relative", nperm=1000)
lm.peake_m2
summary(lm.peake_m2)

#Plot MA regression
plot(lm.peake_m2)

#Plot ordinary least squares (OLS), major axis (MA), standard major axis (SMA), 
#and ranged major axis (RMA) regression lines.
par(mfrow = c(2,2))
plot(lm.peake_m2, "OLS")
plot(lm.peake_m2, "MA")
plot(lm.peake_m2, "SMA")
plot(lm.peake_m2, "RMA")

#Transpose/Transform/Flip dataset
t(peake)
#Changes structure
str(peake)

#Save table
write.table(peake, "C:/Users/Matthias/peake.txt")
#Graphs can be saved manually

#Combine two vectors
genus<-c("Macoma","Cerastoderma","Mytilus")
species<-c("balthica","edule","edulis")
paste(genus,species)


#Merge two data sets with the function merge(), similar to database linking
my.species.data <- data.frame(station = rep(c("A", "B", "C"), each = 4), 
                              specnum = c(rep(1:4, times = 3)), 
                              number = c(123, 83, 96, 0, 33, 56, 28, 58, 25, 93, 45, 92))
my.species.data
my.temp.data <- data.frame(station = c("A", "B", "C", "D"), temperature = c(12, 13, 16, 20))
my.temp.data
my.species.temp.data <- merge(my.temp.data, my.species.data)
my.species.temp.data
#Doesn't show Temperature for Station D.
#You can change settings to display all of both tables (4 way options, in Access only 3)!

species.list <- data.frame(genus, species)
species.list
merge(my.species.temp.data, species.list, by.x="specnum", by.y="row.names", all=TRUE)
#Loose species number 4, Add all=TRUE and you will get NA fors Species number 4.

#Reshaping - Cross-Table /Pivot-Table
#Change data from long to wide, doesn't work!!!
my.new.species.data.wide<-reshape(my.species.data, timevar = "spectrum", idvar = "station", direction = "wide")

#Sort data
order(my.species.data$number, decreasing=TRUE) #Vector that gives order in a row.
my.species.data

my.species.data[order(my.species.data$number, decreasing=TRUE),] #Orders the table!!!

#Summarize data
table(my.species.data$station)
table(my.species.data$station, my.species.data$specnum)

#Number of unique values in a vector (e.g. Number of species)
unique(my.species.data)
length(unique(my.species.data$specnum))

#Load array cermac data
load("~/03_Science/Methods/Statistics/R/R_course_NIOZ/Day 02/cermac.Rdata")

#Display data
cermac

#Mean of 3rd Margin, total mean biomass per Species
apply(cermac, MARGIN = 3, FUN = mean)

#Mean of biomass per Location
apply(cermac, MARGIN= c(2,3), FUN=mean)

#Sum of everything, not devided by species
apply(cermac, MARGIN = c(1,2), FUN=sum)

#Load data frame cermac.dfdf
load("~/03_Science/Methods/Statistics/R/R_course_NIOZ/Day 02/cermacdf.Rdata")

#Apply function
tapply(X = cermac.df$biomass, INDEX = cermac.df$species, FUN=mean)

#Aggregate same function as tapply, but all rows at once.
aggregate(cermac.df$biomass, by=list(cermac.df$station, cermac.df$species), FUN=mean)

#Produce summary able to use for boxplot!!!
aggcermac<-aggregate(cermac.df$biomass, by=list(cermac.df$station, cermac.df$species), FUN=summary)
str(aggcermac)

#Produce boxplot
boxplot(aggcermac[,3])
t(aggcermac[,3])
boxplot(t(aggcermac[,3]), names=paste(aggcermac[,1], aggcermac[,2]), main="Cerastoderma & Macoma 2002 - 2011")



#2nd exercise

#Load new-cars dataset
newcars <- read.delim("~/03_Science/Methods/Statistics/R/R_course_NIOZ/Day 02/newcars.txt")
View(newcars)

#Load brands dataset
brands<- read.delim("~/03_Science/Methods/Statistics/R/R_course_NIOZ/Day 02/brands.txt")

#Merge both datasets
newcars_02<-merge(newcars, brands, all=TRUE)

#Produce table with fuel per continent
table(newcars_02$continent, newcars_02$fuel)

#Plot table
plot(table(newcars_02$continent, newcars_02$fuel))
plot(newcars_02$continent, newcars_02$fuel)

#Look at structure of newcars
str(newcars_02)

#Get help, details of function
?table()

#Create table to find the number of cars per brand
table(newcars_02$brand)

#In another table see how many observations there are per energy label divided in fuel categories.
table(newcars_02$energylabel, newcars$fuel)

#Calculate mean consumption rates for these groups
newcars_02_means<-tapply(X=newcars_02$consumption, INDEX=list(newcars_02$energylabel, newcars_02$fuel), FUN=mean)

#Produce a pairs plot showing price, cilender, 
#power, speed, accelerate, consumption, CO2, 
#mass, loadingvolume, tank, hybrid
pairs(newcars_02[,c(3,5,6,7,8,9,10,12,13,14,15 )], gap=0)

#Produce a pairs plot showing price, power, speed, accelerate, consumption, CO2, mass
pairs(newcars_02[,c(3,6,7,8,9,10,12)], gap=0)

#Produce histogramm correlation coefficients and smoothers, see ?pairs for function
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
pairs(newcars_02[,c(3,6,7,8,9,10,12)], panel=panel.smooth,
      cex = 1.5, pch = 24, bg="light blue",
      diag.panel=panel.hist, cex.labels = 2, font.labels=2)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(newcars_02[,c(3,6,7,8,9,10,12)], lower.panel=panel.smooth, upper.panel=panel.cor)

#Plot consumption against mass
plot(newcars_02$mass, newcars_02$consumption)

#More advanced (Coplot)
coplot(newcars_02$consumption ~ newcars_02$mass | newcars_02$continent)
coplot(newcars_02$consumption, newcars_02$mass | newcars_02$continent*newcars$fuel)
#Or
plot(newcars_02$mass, newcars_02$consumption, col=newcars_02$energylabel, pch=20)
#Hybrid is in 0 and 1 values, 0 is unvisible, add +1
plot(newcars_02$mass, newcars_02$consumption, col=newcars_02$hybrid+1, pch=20)

#Add a regression line
lm_fuel<-lm(newcars_02$consumption~newcars_02$mass)
summary(lm_fuel)

# Plot outcome of linear model
par(mfrow = c(2,2))
plot(lm_fuel)

#Add regression line, but doesn't show regression line!!!
plot(newcars_02$mass, newcars_02$consumption)
abline(lm_fuel, col="red")

#Try to calculate studentized residuals and find cars residuals higher than 2.
rstudent(lm_fuel)

#Plot graph without data
plot(newcars_02$mass, newcars_02$consumption, type="n")
#Add points to frame which have a certain residual value
carsin<-newcars_02[abs(rstudent(lm_fuel))<=2,]
points(carsin$mass, carsin$consumption)

#Add text for values higher than 2, use absolute numbers as residuals are negative and positive
carsout<-newcars_02[abs(rstudent(lm_fuel))>2,]
text(carsout$consumption~carsout$mass, labels=as.character(carsout$brand), col=c("blue","green", "red", "black", "yellow"))
points(carsout$mass, carsout$consumption, col=c("blue","green", "red", "black", "yellow"), pch=20)
#pch=20 changes dots from unfilled to filled!


#Add confidence lines

#Get confidence limits, excludes x-values
predict(lm_fuel,int="p")
#Gives you lower and upper confidence values

#Add line

#Lower limit
lines(x=newcars_02$mass, y=predict(lm_fuel,int="p")[,2])
lines(x=newcars_02$mass, y=predict(lm_fuel,int="c")[,2])

#Upper limit
lines(x=newcars_02$mass, y=predict(lm_fuel,int="p")[,3])
lines(x=newcars_02$mass, y=predict(lm_fuel,int="c")[,3])

#Produce new data.frame with confidence intervals and x-values(mass)
conflines<-data.frame(predict(lm_fuel, int="p"), newcars_02$mass)
#Order after mass
order(conflines[,4])
conflines[order(conflines[,4]),]
conflines<-conflines[order(conflines[,4]),]

#Plot new lines
#Fit of best line
lines(conflines[,4], conflines[,1])
#Line of lower confidence
lines(conflines[,4], conflines[,2])
#Line of upper confidence
lines(conflines[,4], conflines[,3])

#Function that plots all lines from one matrix
matlines(conflines[,4], conflines[,1:3])