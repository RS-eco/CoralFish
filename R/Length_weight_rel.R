#Length-weight relationships

#Set working directory
setwd("C:/Users/Matthias/Documents/03 Science/NIOZ/VideoSurveys/DataAnalysis/Length-weight relationship")

#Import data
Length_weight <- read.delim("length_weight_summary.csv", sep=",",header=T)
Data <- read.delim("length_weight_data.csv", sep=",", header=T)


par(mfrow = c(5,2), mar = c(5,5,2,2))
plot(Data$Brosme_Length_cm,Data$Brosme_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Brosme brosme"))))
plot(Data$Chimaera_Length_cm,Data$Chimaera_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Chimaera monstrosa"))))
plot(Data$Heli_Length_cm,Data$Heli_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Helicolenus dactylopterus"))))
plot(Data$Hoplo_Length_cm,Data$Hoplo_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Hoplostethus atlanticus"))))
plot(Data$Lepidion_Length_cm,Data$Lepidion_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Lepidion eques"))))
plot(Data$Lophius_Length_cm,Data$Lophius_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Lophius piscatorius"))))
plot(Data$Molva_Length_cm,Data$Molva_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Molva dypterygia"))))
plot(Data$Mora_Length_cm,Data$Mora_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Mora moro"))))
plot(Data$Phycis_Length_cm,Data$Phycis_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Phycis phycis"))))
plot(Data$Synapho_Length_cm,Data$Synapho_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Synaphobranchus kaupii"))))

par(mfrow = c(4,1), mar = c(5,5,2,2))
plot(Data$Heli_Length_cm,Data$Heli_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Helicolenus dactylopterus"))))
plot(Data$Lepidion_Length_cm,Data$Lepidion_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Lepidion eques"))))
plot(Data$Mora_Length_cm,Data$Mora_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Mora moro"))))
plot(Data$Synapho_Length_cm,Data$Synapho_Weight_g, type="l",lwd=2,lty=1, xlab="Total length (cm)",ylab="Weight (g)", main=expression(paste(italic("Synaphobranchus kaupii"))))
