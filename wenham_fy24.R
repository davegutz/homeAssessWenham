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
D <- read.csv("wenham_fy24.csv")

# Separate types
Ra <- D[D$Type=='RANCH', ]
Cp <- D[D$Type=='CAPE', ]
Co <- D[D$Type=='COLONIAL', ]
Sl <- D[D$Type=='SPLIT LEVL', ]
Bu <- D[D$Type=='BUNGALOW', ]
Aq <- D[D$Type=='ANTIQUE', ]
Cv <- D[D$Type=='CONVENTONL', ]
Se <- D[D$Type=='SPLIT ENT', ]
Os <- D[D$Type=='OLD STYLE', ]
Cn <- D[D$Type=='CONDO', ]
Cy <- D[D$Type=='CONTEMPORY', ]
Es <- D[D$Type=='ESTATE', ]

plot(Value ~ Area, data=D)
plot(Value ~ Area, data=Ra)
plot(Value ~ Area, data=Cp)
plot(Value ~ Area, data=Co)
plot(Value ~ Area, data=Sl)
plot(Value ~ Area, data=Bu)
plot(Value ~ Area, data=Aq)
plot(Value ~ Area, data=Cv)
plot(Value ~ Area, data=Se)
plot(Value ~ Area, data=Os)
plot(Value ~ Area, data=Cn)
plot(Value ~ Area, data=Cy)
plot(Value ~ Area, data=Es)

######### All
lm_faD <- lm(Value ~ Area, data=D)
summary(lm_faD)
plot(allEffects(lm_faD))

glm_faD <- glm(Value ~ Area + BD + BA + Type, family=gaussian, data=D)
summary(glm_faD)
plot(allEffects(glm_faD))

#newdata=data.frame(nickel=c(80, 90, 99, 100, 101, 110))
#predict(glm_nickel, newdata, type="response") # odds of getting migraine given levels of nickel intake (ug)

######### Ranch
lm_faRa <- lm(Value ~ Area, data=Ra)
summary(lm_faRa)
plot(allEffects(lm_faRa))

plot(Value ~ Area, data=Ra)
abline(lm_faRa)

glm_Ra <- glm(Value ~ Area + BD + BA, family=gaussian, data=Ra)
summary(glm_Ra)
plot(allEffects(glm_Ra))

new_fa=data.frame(Area=c(0, 5000))
new_Ra=predict(lm_faRa, new_fa, type="response")

######### Cape
lm_faCp <- lm(Value ~ Area, data=Cp)
summary(lm_faCp)
plot(allEffects(lm_faCp))

glm_Cp <- glm(Value ~ Area + BD + BA, family=gaussian, data=Cp)
summary(glm_Cp)
plot(allEffects(glm_Cp))

######### Colonial
lm_faCo <- lm(Value ~ Area, data=Co)
summary(lm_faCo)
plot(allEffects(lm_faCo))

glm_Co <- glm(Value ~ Area + BD + BA, family=gaussian, data=Co)
summary(glm_Co)
plot(allEffects(glm_Co))

######### Split Level
lm_faSl <- lm(Value ~ Area, data=Sl)
summary(lm_faSl)
plot(allEffects(lm_faSl))

glm_Sl <- glm(Value ~ Area + BD + BA, family=gaussian, data=Sl)
summary(glm_Sl)
plot(allEffects(glm_Sl))

######### BUNGALOW
lm_faBu <- lm(Value ~ Area, data=Bu)
summary(lm_faBu)
plot(allEffects(lm_faBu))

glm_Bu <- glm(Value ~ Area + BD + BA, family=gaussian, data=Bu)
summary(glm_Bu)
plot(allEffects(glm_Bu))

######### BUNGALOW
lm_faSe <- lm(Value ~ Area, data=Se)
summary(lm_faSe)
plot(allEffects(lm_faSe))

glm_Se <- glm(Value ~ Area + BD + BA, family=gaussian, data=Se)
summary(glm_Se)
plot(allEffects(glm_Se))


######### 
#Aq <- D[D$Type=='ANTIQUE', ]
#Cv <- D[D$Type=='CONVENTONL', ]
#Se <- D[D$Type=='SPLIT ENT', ]
#Os <- D[D$Type=='OLD STYLE', ]
#Cn <- D[D$Type=='CONDO', ]
#Cy <- D[D$Type=='CONTEMPORY', ]
#Es <- D[D$Type=='ESTATE', ]



#############Summarize

##### Ranch v Cape
plot(Ra$Area, Ra$Value, type="n", xlim=c(500, 3500), ylim=c(200000, 1200000), 
     xlab="Finished Area, sqft", ylab="Total Assesed Value, $", main="Wenham Neighborhood 2000")
lines(Ra$Area, Ra$Value, type="p", pch=1, col="red")
abline(lm_faRa, col="red")
lines(Cp$Area, Cp$Value, type="p", pch=2, col="blue")
abline(lm_faCp, col="blue")
legend("topleft", legend=c("Ranch", "Cape"),
       col=c("red", "blue"), lty=1)

##### Ranch v Split Entry
plot(Ra$Area, Ra$Value, type="n", xlim=c(500, 3500), ylim=c(200000, 1200000), 
     xlab="Finished Area, sqft", ylab="Total Assesed Value, $", main="Wenham Neighborhood 2000")
lines(Ra$Area, Ra$Value, type="p", pch=1, col="red")
abline(lm_faRa, col="red")
lines(Se$Area, Se$Value, type="p", pch=5, col="orange")
abline(lm_faSe, col="orange")
legend("topleft", legend=c("Ranch", "Split Entry"),
       col=c("red", "orange"), lty=1)

# Create an empty plot
plot(Ra$Area, Ra$Value, type="n", xlim=c(500, 3500), ylim=c(200000, 1200000), 
     xlab="Finished Area, sqft", ylab="Total Assesed Value, $", main="Wenham Neighborhood 2000")
abline(lm_faRa, col="red")
abline(lm_faCp, col="blue")
abline(lm_faCo, col="green")
abline(lm_faSl, col="black")
abline(lm_faBu, col="pink")
abline(lm_faSe, col="orange")
legend("topleft", legend=c("Ranch", "Cape", "Colonial", "Split Level", "Bungalow", "Split Entry"),
       col=c("red", "blue", "green", "black", "pink", "orange"), lty=1)
