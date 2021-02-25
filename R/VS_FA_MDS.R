VS_FA_Multivariate <- read.delim("~/VS_FA_Multivariate.txt")
View(VS_FA_Multivariate)

install.packages("vegan")
library(vegan)

#Species accumulative curve
spp.curve <- specaccum(comm = com, method = "random", permutations = 1000)
plot(spp.curve)

#NMS ordination

install.packages("ecodist")
install.packages("BiodiversityR")
install.packages("ellipse")

library(ecodist)
library(BiodiversityR)
library(ellipse)
library(vegan)

#Bray-curtis similarity
dis <- distance(com, method = "bray-curtis")

#Create a scree plot
scree <- nmds(dis, mindim = 1, maxdim = 5, nits = 10)

#Include stress level
stress <- scree$stress
> plot(stress)