# Real Estate analysis
# 15-Sep-2024

 # SETUP ####
# load packages
library(lubridate)
library(readxl)
library(ggplot2)
library(tidyr)
library(effects)
library(dplyr)
library(zoo) # rollmean
 
 
# Import Data
D <- read.csv("all2000_20230127.csv")
D$Total.Value <- as.numeric(gsub("[$,]", "", D$Total.Value))
D <- D[!is.na(D$Fin.area), ]
D <- D[!is.na(D$Total.Value), ]

# Separate types
Ra <- D[D$Type=='RANCH', ]
Cp <- D[D$Type=='CAPE', ]
Co <- D[D$Type=='COLONIAL', ]
Sl <- D[D$Type=='SPLIT LEVL', ]

plot(Total.Value ~ Fin.area, data=D)
plot(Total.Value ~ Lot.size, data=D)
plot(Total.Value ~ Fin.area, data=Ra)
plot(Total.Value ~ Lot.size, data=Ra)

######### All
lm_faD <- lm(Total.Value ~ Fin.area, data=D)
summary(lm_faD)
plot(allEffects(lm_faD))

glm_faD <- glm(Total.Value ~ Fin.area + Lot.size + Beds + Baths + Type, family=gaussian, data=D)
summary(glm_faD)
plot(allEffects(glm_faD))

#newdata=data.frame(nickel=c(80, 90, 99, 100, 101, 110))
#predict(glm_nickel, newdata, type="response") # odds of getting migraine given levels of nickel intake (ug)

######### Ranch
lm_faRa <- lm(Total.Value ~ Fin.area, data=Ra)
summary(lm_faRa)
plot(allEffects(lm_faRa))

plot(Total.Value ~ Fin.area, data=Ra)
abline(lm_faRa)


glm_Ra <- glm(Total.Value ~ Fin.area + Lot.size + Beds + Baths, family=gaussian, data=Ra)
summary(glm_Ra)
plot(allEffects(glm_Ra))

new_fa=data.frame(Fin.area=c(0, 5000))
new_Ra=predict(lm_faRa, new_fa, type="response")

######### Cape
lm_faCp <- lm(Total.Value ~ Fin.area, data=Cp)
summary(lm_faCp)
plot(allEffects(lm_faCp))

glm_Cp <- glm(Total.Value ~ Fin.area + Lot.size + Beds + Baths, family=gaussian, data=Cp)
summary(glm_Cp)
plot(allEffects(glm_Cp))

######### Colonial
lm_faCo <- lm(Total.Value ~ Fin.area, data=Co)
summary(lm_faCo)
plot(allEffects(lm_faCo))

glm_Co <- glm(Total.Value ~ Fin.area + Lot.size + Beds + Baths, family=gaussian, data=Co)
summary(glm_Co)
plot(allEffects(glm_Co))

######### Split Level
lm_faSl <- lm(Total.Value ~ Fin.area, data=Sl)
summary(lm_faSl)
plot(allEffects(lm_faSl))

glm_Sl <- glm(Total.Value ~ Fin.area + Lot.size + Beds + Baths, family=gaussian, data=Sl)
summary(glm_Sl)
plot(allEffects(glm_Sl))

#############Summarize

# Create an empty plot
plot(Ra$Fin.area, Ra$Total.Value, type="n", xlim=c(500, 5500), ylim=c(200000, 1200000), 
     xlab="Finished Area, sqft", ylab="Total Assesed Value, $", main="Wenham Neighborhood 2000")
lines(Ra$Fin.area, Ra$Total.Value, type="p", pch=1, col="red")
abline(lm_faRa, col="red")
lines(Cp$Fin.area, Cp$Total.Value, type="p", pch=2, col="blue")
abline(lm_faCp, col="blue")
lines(Co$Fin.area, Co$Total.Value, type="p", pch=3, col="green")
abline(lm_faCo, col="green")
lines(Sl$Fin.area, Sl$Total.Value, type="p", pch="S", col="black")
abline(lm_faSl, col="black")
legend("topleft", legend=c("Ranch", "Cape", "Colonial", "Split Level"), col=c("red", "blue", "green", "black"), lty=1)
