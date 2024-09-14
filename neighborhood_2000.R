# Migraine Analysis
# March 30, 2023

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
#D <- read_excel("migraine_diary.xlsx", sheet = "Migraine")
head(D)
str(D)

# Separate types
Ra <- D[D$Type=='RANCH', ]
Cp <- D[D$Type=='CAPE', ]
Co <- D[D$Type=='COLONIAL', ]
Sl <- D[D$Type=='SPLIT LEVL', ]

#format variables #####

D$migraine <- as.factor(ifelse(is.na(D$migraine), FALSE, TRUE)) # set migraine to boolean. if blank FALSE, else TRUE (migraine present)
D$time_asleep <- hour(D$time_asleep)*60 + minute(D$time_asleep)

D$severity <- ifelse(is.na(D$severity), 0, D$severity)

D$scent <- ifelse(is.na(D$scent), FALSE, TRUE)
D$num_pillsMorning
D$num_vitamins
D$period <- ifelse(is.na(D$period), FALSE, TRUE)
D$menstrual_phase <- factor(D$menstrual_phase, levels = c("menses", "week 1", "week 2", "week 3", "week 4", "week 5"))

# make column of Date as number of days elapsed since start (to make numeric)
firstDate <- D$Date[1]
D$dateNum = as.numeric(difftime(D$Date, firstDate, units = "days")) 

# drop data that doesn't have date
D <- D[!is.na(D$Date), ]

# make rolling average of values
# avg over past 2 days
D$nickelAvg2 <- c(NA, rollmean(D$nickel, 2, na.pad = FALSE, align = c("right")))
D$timeAsleepAvg2 <- c(NA, rollmean(D$time_asleep, 2, na.pad = FALSE, align = c("right")))
D$totalScreenAvg2 <- c(NA, rollmean(D$total_screen, 2, na.pad = FALSE, align = c("right")))
D$fiberAvg2 <- c(NA, rollmean(D$fiber, 2, na.pad = FALSE, align = c("right")))
D$tyramineAvg2 <- c(NA, rollmean(D$Tyramine, 2, na.pad = FALSE, align = c("right")))
D$histamineAvg2 <- c(NA, rollmean(D$Histamine, 2, na.pad = FALSE, align = c("right")))
D$histamineSIGHIAvg2 <- c(NA, rollmean(D$Histamine_SIGHI, 2, na.pad = FALSE, align = c("right")))

# avg over past 3 days
D$nickelAvg3 <- c(NA, NA, rollmean(D$nickel, 3, na.pad = FALSE, align = c("right")))
D$timeAsleepAvg3 <- c(NA, NA, rollmean(D$time_asleep, 3, na.pad = FALSE, align = c("right")))
D$totalScreenAvg3 <- c(NA, NA, rollmean(D$total_screen, 3, na.pad = FALSE, align = c("right")))
D$fiberAvg3 <- c(NA, NA, rollmean(D$fiber, 3, na.pad = FALSE, align = c("right")))
D$tyramineAvg3 <- c(NA, NA, rollmean(D$Tyramine, 3, na.pad = FALSE, align = c("right")))
D$histamineAvg3 <- c(NA, NA, rollmean(D$Histamine, 3, na.pad = FALSE, align = c("right")))
D$histamineSIGHIAvg3 <- c(NA, NA, rollmean(D$Histamine_SIGHI, 3, na.pad = FALSE, align = c("right")))




# lag
# 1) make lag
# screen time lad
D$screenlag1 <- lag(D$total_screen, n = 1L)
D$screenlag2 <- lag(D$total_screen, n = 2L)
D$screenlag3 <- lag(D$total_screen, n = 3L)
D$screenlag4 <- lag(D$total_screen, n = 4L)
D$screenlag5 <- lag(D$total_screen, n = 5L)

# NICKEL Lag
D$nickellag1 <- lag(D$nickel, n = 1L)
D$nickellag2 <- lag(D$nickel, n = 2L)
D$nickellag3 <- lag(D$nickel, n = 3L)
D$nickellag4 <- lag(D$nickel, n = 4L)
D$nickellag5 <- lag(D$nickel, n = 5L)

# FIBER Lag
D$fiberlag1 <- lag(D$fiber, n = 1L)
D$fiberlag2 <- lag(D$fiber, n = 2L)
D$fiberlag3 <- lag(D$fiber, n = 3L)
D$fiberlag4 <- lag(D$fiber, n = 4L)
D$fiberlag5 <- lag(D$fiber, n = 5L)

# TYRAMINE Lag
D$tyraminelag1 <- lag(D$Tyramine, n = 1L)
D$tyraminelag2 <- lag(D$Tyramine, n = 2L)
D$tyraminelag3 <- lag(D$Tyramine, n = 3L)
D$tyraminelag4 <- lag(D$Tyramine, n = 4L)
D$tyraminelag5 <- lag(D$Tyramine, n = 5L)

# Histamine Lag
D$histaminelag1 <- lag(D$Histamine, n = 1L)
D$histaminelag2 <- lag(D$Histamine, n = 2L)
D$histaminelag3 <- lag(D$Histamine, n = 3L)
D$histaminelag4 <- lag(D$Histamine, n = 4L)
D$histaminelag5 <- lag(D$Histamine, n = 5L)


# Histamine Lag SIGHI
D$histamineSIGHIlag1 <- lag(D$Histamine_SIGHI, n = 1L)
D$histamineSIGHIlag2 <- lag(D$Histamine_SIGHI, n = 2L)
D$histamineSIGHIlag3 <- lag(D$Histamine_SIGHI, n = 3L)
D$histamineSIGHIlag4 <- lag(D$Histamine_SIGHI, n = 4L)
D$histamineSIGHIlag5 <- lag(D$Histamine_SIGHI, n = 5L)

# PressureChange Lag (minMinusMax from day before)
D$pressure_minMinusMaxYesterday_lag1 <- lag(D$pressure_minMinusMaxYesterday, n = 1L)
D$pressure_minMinusMaxYesterday_lag2 <- lag(D$pressure_minMinusMaxYesterday, n = 2L)
D$pressure_minMinusMaxYesterday_lag3 <- lag(D$pressure_minMinusMaxYesterday, n = 3L)
D$pressure_minMinusMaxYesterday_lag4 <- lag(D$pressure_minMinusMaxYesterday, n = 4L)
D$pressure_minMinusMaxYesterday_lag5 <- lag(D$pressure_minMinusMaxYesterday, n = 5L)

# PressureChange Lag (avg change)
D$pressure_avg_changefromdaybefore_lag1 <- lag(D$pressure_avg_changefromdaybefore, n = 1L)
D$pressure_avg_changefromdaybefore_lag2 <- lag(D$pressure_avg_changefromdaybefore, n = 2L)
D$pressure_avg_changefromdaybefore_lag3 <- lag(D$pressure_avg_changefromdaybefore, n = 3L)
D$pressure_avg_changefromdaybefore_lag4 <- lag(D$pressure_avg_changefromdaybefore, n = 4L)
D$pressure_avg_changefromdaybefore_lag5 <- lag(D$pressure_avg_changefromdaybefore, n = 5L)


# make long data and plot #####

measure <- "Date"
value <- "measurement"
gathercols <- c("time_asleep", "nickel", "migraine", "severity", "period")
DAbbrev <- D[, c("Date", gathercols)]
DLong <- gather(DAbbrev, measure, value, gathercols)
DLong <- drop_na(DLong)# dropNa
dim(DLong)

# plot
ggplot(data = DLong, aes(x = Date, y = value, group = measure, color = measure)) +
  geom_line()


# plot #####

plot(D$severity ~ D$time_asleep)
plot(D$severity ~ D$nickel)

plot(D$severity ~ D$Date)

# model #####

# Nickel Intake (ug)
lm_nickel <- lm(nickel ~ migraine, data = D)
summary(lm_nickel)
plot(allEffects(lm_nickel))

glm_nickel <- glm(migraine ~ nickel, family = binomial, data = D)
summary(glm_nickel)
plot(allEffects(glm_nickel))
exp(coef(glm_nickel)[2]) # nickel odds 1.012368  = odds of migraine goes up 1.2% for every 1 mcg of nickel... 12% for every 10mcg
plot(allEffects(glm_nickel), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

newdata = data.frame(nickel = c(80, 90, 99, 100, 101, 110))
predict(glm_nickel, newdata, type = "response") # odds of getting migraine given levels of nickel intake (ug)

# nickel moving avg (looks at avg of past 3 days)
glm_nickelAvg3 <- glm(migraine ~ nickelAvg3, family = binomial, data = D)
summary(glm_nickelAvg3)
plot(allEffects(glm_nickelAvg3))
exp(coef(glm_nickelAvg3)[2]) # nickel odds 1.01
plot(allEffects(glm_nickelAvg3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# nickel moving avg (looks at avg of past 2 days)
glm_nickelAvg2 <- glm(migraine ~ nickelAvg2, family = binomial, data = D)
summary(glm_nickelAvg2)
plot(allEffects(glm_nickelAvg2))
exp(coef(glm_nickelAvg2)[2]) # 1.014 nickel odds HIGHEST of the 3 nickels  (higher than that day and avg over 3 days and lag of one day)
plot(allEffects(glm_nickelAvg2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# nickel lag of one day
glm_nickelLag1 <- glm(migraine ~ nickellag1, family = binomial, data = D)
summary(glm_nickelLag1)
plot(allEffects(glm_nickelLag1))
exp(coef(glm_nickelLag1)[2]) # 1.011
plot(allEffects(glm_nickelLag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# FIBER
lm_fiber <- lm(fiber ~ migraine, data = D)
summary(lm_fiber)
plot(allEffects(lm_fiber)) # not sig

glm_fiber <- glm(migraine ~ fiber, family = binomial, data = D)
summary(glm_fiber)
plot(allEffects(glm_fiber))
exp(coef(glm_fiber)[2]) # fiber odds .98
plot(allEffects(glm_fiber), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

newdata = data.frame(fiber = c(0, 5, 10, 15, 20, 25, 30))
predict(glm_fiber, newdata, type = "response") # odds of getting migraine given levels of nickel intake (ug)

# fiber moving avg (looks at avg of past 3 days)
glm_fiberAvg3 <- glm(migraine ~ fiberAvg3, family = binomial, data = D)
summary(glm_fiberAvg3) # not sig... but approaching. effect size?
plot(allEffects(glm_fiberAvg3))
exp(coef(glm_fiberAvg3)[2]) # fiber odds... .914. odds of getting migrain decreases by 9% for every gram of fiber consumed
plot(allEffects(glm_fiberAvg3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# fiber moving avg (looks at avg of past 2 days)
glm_fiberAvg2 <- glm(migraine ~ fiberAvg2, family = binomial, data = D)
summary(glm_fiberAvg2)
plot(allEffects(glm_fiberAvg2))
exp(coef(glm_fiberAvg2)[2]) # 0.98
plot(allEffects(glm_fiberAvg2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# fiber lag of one day
glm_fiberLag1 <- glm(migraine ~ fiberlag1, family = binomial, data = D)
summary(glm_fiberLag1)
plot(allEffects(glm_fiberLag1))
exp(coef(glm_fiberLag1)[2]) # 0.98
plot(allEffects(glm_fiberLag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# fiber lag of 2 days
glm_fiberLag2 <- glm(migraine ~ fiberlag2, family = binomial, data = D)
summary(glm_fiberLag2) ## SIGNIFICANT
plot(allEffects(glm_fiberLag2))
exp(coef(glm_fiberLag2)[2]) # 0.86. Odds of getting migraine decreases by 14% for very gram of fiber consumed 2 days before
plot(allEffects(glm_fiberLag2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# fiber lag of 3 days
glm_fiberLag3 <- glm(migraine ~ fiberlag3, family = binomial, data = D)
summary(glm_fiberLag3)
plot(allEffects(glm_fiberLag3))
exp(coef(glm_fiberLag3)[2]) #
plot(allEffects(glm_fiberLag3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


## TYRAMINE
# Tyramine has non-significant effect
# FOR whether or not item is on tyramine avoid list:
# OR = 1.25 for 2-day movign avg. odds of getting migraine goes up by 25% for every item with tyramine consumed
# for amount of tyramine (in mg)

lm_tyramine <- lm(Tyramine ~ migraine, data = D)
summary(lm_tyramine)
plot(allEffects(lm_tyramine)) # not sig

glm_tyramine <- glm(migraine ~ Tyramine, family = binomial, data = D)
summary(glm_tyramine)
plot(allEffects(glm_tyramine))
exp(coef(glm_tyramine)[2]) # tyramine odds 1.10; odds of migraine goes up 10% for every item of tyramine eaten
plot(allEffects(glm_tyramine), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

newdata = data.frame(Tyramine = c(0, 1, 2, 3, 4, 5, 6, 7))
predict(glm_tyramine, newdata, type = "response") # odds of getting migraine given # of tyramine-containing foods

# tyramine moving avg (looks at avg of past 3 days)
glm_tyramineAvg3 <- glm(migraine ~ tyramineAvg3, family = binomial, data = D)
summary(glm_tyramineAvg3) # not sig...
plot(allEffects(glm_tyramineAvg3))
exp(coef(glm_tyramineAvg3)[2]) # tyramine odds... 1.24. odds of getting migraine incrases by 24% for every food w/ tyramine
plot(allEffects(glm_tyramineAvg3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# tyramine moving avg (looks at avg of past 2 days)
glm_tyramineAvg2 <- glm(migraine ~ tyramineAvg2, family = binomial, data = D)
summary(glm_tyramineAvg2)
plot(allEffects(glm_tyramineAvg2))
exp(coef(glm_tyramineAvg2)[2]) # = 1.25%
plot(allEffects(glm_tyramineAvg2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# tyramine lag of one day
glm_tyramineLag1 <- glm(migraine ~ tyraminelag1, family = binomial, data = D)
summary(glm_tyramineLag1)
plot(allEffects(glm_tyramineLag1))
exp(coef(glm_tyramineLag1)[2]) # 1.21
plot(allEffects(glm_tyramineLag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# tyramine lag of 2 days
glm_tyramineLag2 <- glm(migraine ~ tyraminelag2, family = binomial, data = D)
summary(glm_tyramineLag2) ## SIGNIFICANT
plot(allEffects(glm_tyramineLag2))
exp(coef(glm_tyramineLag2)[2]) # 1.05
plot(allEffects(glm_tyramineLag2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# tyramine lag of 3 days
glm_tyramineLag3 <- glm(migraine ~ tyraminelag3, family = binomial, data = D)
summary(glm_tyramineLag3)
plot(allEffects(glm_tyramineLag3))
exp(coef(glm_tyramineLag3)[2]) #0.99
plot(allEffects(glm_tyramineLag3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


## HISTAMINE ####
lm_histamine <- lm(Histamine ~ migraine, data = D)
summary(lm_histamine)
plot(allEffects(lm_histamine)) # p = .08

glm_histamine <- glm(migraine ~ Histamine, family = binomial, data = D)
summary(glm_histamine) # p - .09
plot(allEffects(glm_histamine))
exp(coef(glm_histamine)[2]) # tyramine odds 1.33; odds of migraine goes up 33% for every serving of high histamine food
plot(allEffects(glm_histamine), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

newdata = data.frame(Histamine = c(0, 1, 2, 3, 4, 5, 6, 7))
predict(glm_histamine, newdata, type = "response") # odds of getting migraine given # of tyramine-containing foods

# histamine moving avg (looks at avg of past 3 days)
glm_histamineAvg3 <- glm(migraine ~ histamineAvg3, family = binomial, data = D)
summary(glm_histamineAvg3) # not sig...
plot(allEffects(glm_histamineAvg3))
exp(coef(glm_histamineAvg3)[2]) # histamine odds... 1.42. odds of getting migraine incrases by 42% for every serving of high-histamine food
plot(allEffects(glm_histamineAvg3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# histamine moving avg (looks at avg of past 2 days)
glm_histamineAvg2 <- glm(migraine ~ histamineAvg2, family = binomial, data = D)
summary(glm_histamineAvg2) # p = .05
plot(allEffects(glm_histamineAvg2))
exp(coef(glm_histamineAvg2)[2]) # = 1.56%
plot(allEffects(glm_histamineAvg2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# histamine lag of one day
glm_histamineLag1 <- glm(migraine ~ histaminelag1, family = binomial, data = D)
summary(glm_histamineLag1) # NOT SIGNIFICANT
plot(allEffects(glm_histamineLag1))
exp(coef(glm_histamineLag1)[2]) # 1.21
plot(allEffects(glm_histamineLag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# histamine lag of 2 days
glm_histamineLag2 <- glm(migraine ~ histaminelag2, family = binomial, data = D)
summary(glm_histamineLag2) ## NOT SIGNIFICANT
plot(allEffects(glm_histamineLag2))
exp(coef(glm_histamineLag2)[2]) # 1.05
plot(allEffects(glm_histamineLag2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# histamine lag of 3 days
glm_histamineLag3 <- glm(migraine ~ histaminelag3, family = binomial, data = D)
summary(glm_histamineLag3) # not sig
plot(allEffects(glm_histamineLag3))
exp(coef(glm_histamineLag3)[2]) #0.99
plot(allEffects(glm_histamineLag3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


### HISTAMINE SIGHI #####

lm_histamineSIGHI <- lm(Histamine_SIGHI ~ migraine, data = D)
summary(lm_histamineSIGHI)
plot(allEffects(lm_histamineSIGHI)) # p = .08

glm_histamineSIGHI <- glm(migraine ~ Histamine_SIGHI, family = binomial, data = D)
summary(glm_histamineSIGHI) # p - .09
plot(allEffects(glm_histamineSIGHI))
exp(coef(glm_histamineSIGHI)[2]) # tyramine odds 1.33; odds of migraine goes up 33% for every serving of high histamine food
plot(allEffects(glm_histamineSIGHI), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

newdata = data.frame(Histamine_SIGHI = c(0, 1, 2, 3, 4, 5, 6, 7))
predict(glm_histamineSIGHI, newdata, type = "response") # odds of getting migraine given # of tyramine-containing foods

# histamine moving avg (looks at avg of past 3 days)
glm_histamineSIGHIAvg3 <- glm(migraine ~ histamineSIGHIAvg3, family = binomial, data = D)
summary(glm_histamineSIGHIAvg3) # not sig...
plot(allEffects(glm_histamineSIGHIAvg3))
exp(coef(glm_histamineSIGHIAvg3)[2]) # histamine odds... 1.42. odds of getting migraine incrases by 42% for every serving of high-histamine food
plot(allEffects(glm_histamineSIGHIAvg3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# histamine moving avg (looks at avg of past 2 days)
glm_histamineSIGHIAvg2 <- glm(migraine ~ histamineSIGHIAvg2, family = binomial, data = D)
summary(glm_histamineSIGHIAvg2) # p = .05
plot(allEffects(glm_histamineSIGHIAvg2))
exp(coef(glm_histamineSIGHIAvg2)[2]) # = 1.56%
plot(allEffects(glm_histamineSIGHIAvg2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# histamine lag of one day
glm_histamineSIGHILag1 <- glm(migraine ~ histamineSIGHIlag1, family = binomial, data = D)
summary(glm_histamineSIGHILag1) # NOT SIGNIFICANT
plot(allEffects(glm_histamineSIGHILag1))
exp(coef(glm_histamineSIGHILag1)[2]) # 1.21
plot(allEffects(glm_histamineSIGHILag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# histamine lag of 2 days
glm_histamineSIGHILag2 <- glm(migraine ~ histamineSIGHIlag2, family = binomial, data = D)
summary(glm_histamineSIGHILag2) ## NOT SIGNIFICANT
plot(allEffects(glm_histamineSIGHILag2))
exp(coef(glm_histamineSIGHILag2)[2]) # 1.05
plot(allEffects(glm_histamineSIGHILag2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# histamine lag of 3 days
glm_histamineSIGHILag3 <- glm(migraine ~ histamineSIGHIlag3, family = binomial, data = D)
summary(glm_histamineSIGHILag3) # not sig
plot(allEffects(glm_histamineSIGHILag3))
exp(coef(glm_histamineSIGHILag3)[2]) #0.99
plot(allEffects(glm_histamineSIGHILag3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 



## SLEEP ####

# Time Asleep (min)
lm_timeAsleep <- lm(time_asleep ~ migraine, data = D)
summary(lm_timeAsleep)
plot(allEffects(lm_timeAsleep))

glm_sleep <- glm(migraine ~ time_asleep, family = binomial, data = D)
summary(glm_sleep)
plot(allEffects(glm_sleep))
exp(coef(glm_sleep)[2]) # nickel odds

# avg over last 3 days
glm_sleepAvg3 <- glm(migraine ~ timeAsleepAvg3, family = binomial, data = D)
summary(glm_sleepAvg3)
plot(allEffects(glm_sleepAvg3))
exp(coef(glm_sleepAvg3)[2]) #  odds

# avg over last 2 days
glm_sleepAvg2 <- glm(migraine ~ timeAsleepAvg2, family = binomial, data = D)
summary(glm_sleepAvg2)
plot(allEffects(glm_sleepAvg2))
exp(coef(glm_sleepAvg2)[2]) #  odds


# period (yes/no)
lm_period <- lm(period ~ migraine, data = D)
summary(lm_period)
plot(allEffects(lm_period))

glm_pd <- glm(migraine ~ period, family = binomial, data = D)
summary(glm_pd)
plot(allEffects(glm_pd))

exp(coef(glm_pd)[2]) # nickel odds


# menstrual cycle
lm_mensCycle <- lm(menstrual_cycle ~ migraine, data = D)
summary(lm_mensCycle)
plot(allEffects(lm_mensCycle))

glm_mensCycle <- glm(migraine ~ menstrual_cycle, family = binomial, data = D)
summary(glm_mensCycle)
plot(allEffects(glm_mensCycle))
exp(coef(glm_mensCycle)[2]) # nickel odds

# menstrual cycle days until period
glm_DaysUntilMens <- glm(migraine ~ days_until_period, family = binomial, data = D)
summary(glm_DaysUntilMens)
plot(allEffects(glm_DaysUntilMens))
exp(coef(glm_DaysUntilMens)[2]) # nickel odds
plot(migraine ~ days_until_period, data = D)

# menstrual phase (leaving out ovulation b/c too few data points)
glm_mensPhase <- glm(migraine ~ menstrual_phase, family = binomial, data = D)
summary(glm_mensPhase)
plot(allEffects(glm_mensPhase))

plot(allEffects(glm_mensPhase), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# coffee
glm_coffee <- glm(migraine ~ coffee, family = binomial, data = D)
summary(glm_coffee)
plot(allEffects(glm_coffee), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# exercise
glm_exercise <- glm(migraine ~ exercise, family = binomial, data = D)
summary(glm_exercise)
plot(allEffects(glm_exercise), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# screentime
glm_screen <- glm(migraine ~ total_screen, family = binomial, data = D)
summary(glm_screen)
plot(allEffects(glm_screen), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(glm_screen)[2]) #  odds HIGHEST of screen time avgs

# screentime rolling avg
# 3 days
glm_screenAvg3 <- glm(migraine ~ totalScreenAvg3, family = binomial, data = D)
summary(glm_screenAvg3)
plot(allEffects(glm_screenAvg3), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(glm_screenAvg3)[2]) #  odds

# 2 days
glm_screenAvg2 <- glm(migraine ~ totalScreenAvg2, family = binomial, data = D)
summary(glm_screenAvg2)
plot(allEffects(glm_screenAvg2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(glm_screenAvg2)[2]) #  odds

# 1 day lag
glm_screenLag1 <- glm(migraine ~ screenlag1, family = binomial, data = D)
summary(glm_screenLag1)
plot(allEffects(glm_screenLag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(glm_screenLag1)[2]) #  odds

# 2 day lag
glm_screenLag2 <- glm(migraine ~ screenlag2, family = binomial, data = D)
summary(glm_screenLag2)
plot(allEffects(glm_screenLag2), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(glm_screenLag2)[2]) #  odds

# scent
glm_scent <- glm(migraine ~ scent, family = binomial, data = D)
summary(glm_scent)
plot(allEffects(glm_scent), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

exp(coef(glm_scent)[2]) # nickel odds


# Number of vitamins

glm_numVit <- glm(migraine ~ num_vitamins, family = binomial, data = D)
summary(glm_numVit)
plot(allEffects(glm_numVit), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# Number of pills taken in morning
glm_numPills <- glm(migraine ~ num_pillsMorning, family = binomial, data = D)
summary(glm_numPills)
plot(allEffects(glm_numPills), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


#### WEATHER ######
# AVG TEMP
lm_temp_avg <- lm(temp_avg ~ migraine, data = D)
summary(lm_temp_avg)
plot(allEffects(lm_temp_avg))

glm_temp_avg <- glm(migraine ~ temp_avg, family = binomial, data = D)
summary(glm_temp_avg)
plot(allEffects(glm_temp_avg))
exp(coef(glm_temp_avg)[2]) # temp avg odds 1.015696  = odds of migraine goes up 1.5% for every 1 degree F increase in temp
plot(allEffects(glm_temp_avg), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

newdata = data.frame(temp_avg = c(80, 90, 99, 100, 101, 110))
predict(glm_temp_avg, newdata, type = "response") # odds of getting migraine given levels of nickel intake (ug)


# MAX TEMP
lm_temp_max <- lm(temp_max ~ migraine, data = D)
summary(lm_temp_max)
plot(allEffects(lm_temp_max))

glm_temp_max <- glm(migraine ~ temp_max, family = binomial, data = D)
summary(glm_temp_max)
plot(allEffects(glm_temp_max))
exp(coef(glm_temp_max)[2]) # temp max odds 1.012  = odds of migraine goes up 1.2% for every 1 degree F increase in max temp
plot(allEffects(glm_temp_max), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# Pressure (in) AVG
lm_pressure_avg <- lm(pressure_avg ~ migraine, data = D)
summary(lm_pressure_avg)
plot(allEffects(lm_pressure_avg))

glm_pressure_avg <- glm(migraine ~ pressure_avg, family = binomial, data = D)
summary(glm_pressure_avg)
plot(allEffects(glm_pressure_avg))
exp(coef(glm_pressure_avg)[2]) # pressure avg odds 1.256  = odds of migraine goes up 25% for every 1  inch increase in pressure
plot(allEffects(glm_pressure_avg), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# Pressure change (in) min of today - max from yesterday
lm_pressure_changeMinMinusMax1day <- lm(pressure_minMinusMaxYesterday ~ migraine, data = D)
summary(lm_pressure_changeMinMinusMax1day)
plot(allEffects(lm_pressure_changeMinMinusMax1day))

glm_pressure_changeMinMinusMax1day <- glm(migraine ~ pressure_minMinusMaxYesterday, family = binomial, data = D)
summary(glm_pressure_changeMinMinusMax1day)
plot(allEffects(glm_pressure_changeMinMinusMax1day))
exp(coef(glm_pressure_changeMinMinusMax1day)[2]) # pressure avg odds 0.626  = odds of migraine goes up 1 inch pressure decrease.... decreases by 38%... increase by 60% (1/0.626) for every 1in decrease in pressure?
plot(allEffects(glm_pressure_changeMinMinusMax1day), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# Pressure change (in) min of today - max from yesterday lag 1
lm_pressure_changeMinMinusMax1day_lag1 <- lm(pressure_minMinusMaxYesterday_lag1 ~ migraine, data = D)
summary(lm_pressure_changeMinMinusMax1day_lag1)
plot(allEffects(lm_pressure_changeMinMinusMax1day_lag1))

glm_pressure_changeMinMinusMax1day_lag1 <- glm(migraine ~ pressure_minMinusMaxYesterday_lag1, family = binomial, data = D)
summary(glm_pressure_changeMinMinusMax1day_lag1)
plot(allEffects(glm_pressure_changeMinMinusMax1day_lag1))
exp(coef(glm_pressure_changeMinMinusMax1day_lag1)[2]) # pressure avg odds 0.63  = odds of migraine goes up 1 inch pressure decrease.... decreases by 37%... increase by 59% (1/0.63) for every 1in decrease in pressure?
plot(allEffects(glm_pressure_changeMinMinusMax1day_lag1), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# Pressure change (in) avg today - yesterday
lm_pressure_avg_changefromdaybefore <- lm(pressure_avg_changefromdaybefore ~ migraine, data = D)
summary(lm_pressure_avg_changefromdaybefore)
plot(allEffects(lm_pressure_avg_changefromdaybefore))

glm_pressure_avg_changefromdaybefore <- glm(migraine ~ pressure_avg_changefromdaybefore, family = binomial, data = D)
summary(glm_pressure_avg_changefromdaybefore)
plot(allEffects(glm_pressure_avg_changefromdaybefore))
exp(coef(glm_pressure_avg_changefromdaybefore)[2]) # pressure avg odds 0.51  = odds of migraine goes up 1 inch pressure decrease.... decreases by 49%... increase by 96% (1/0.51) for every 1in decrease in pressure?
plot(allEffects(glm_pressure_avg_changefromdaybefore), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# PRECIPITATION avg
lm_precipitation <- lm(precipitation ~ migraine, data = D)
summary(lm_precipitation)
plot(allEffects(lm_precipitation))

glm_precipitation <- glm(migraine ~ precipitation, family = binomial, data = D)
summary(glm_precipitation)
plot(allEffects(glm_precipitation))
exp(coef(glm_precipitation)[2]) #
plot(allEffects(glm_precipitation), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
# No sig


# HUMIDITY
lm_humidity_avg <- lm(humidity_avg ~ migraine, data = D)
summary(lm_humidity_avg)
plot(allEffects(lm_humidity_avg))

glm_humidity_avg <- glm(migraine ~ humidity_avg, family = binomial, data = D)
summary(glm_humidity_avg)
plot(allEffects(glm_humidity_avg))
exp(coef(glm_humidity_avg)[2]) # .997
plot(allEffects(glm_humidity_avg), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
# no effect

# TEMP CHANGE


# MIXED EFFECTS MODELS #####
lmer_NiPdScreenSleep <- glm(migraine ~ nickel + menstrual_cycle + time_asleep + total_screen + tyramineAvg2, family = binomial, data = D)
summary(lmer_NiPdScreenSleep)
plot(allEffects(lmer_NiPdScreenSleep), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 

# mixed effects model plus fiber
lmer_NiPdScreenSleepFiber <- glm(migraine ~ nickel + menstrual_cycle + time_asleep + total_screen + fiberlag2 + tyramineAvg2, family = binomial, data = D)
summary(lmer_NiPdScreenSleepFiber)
plot(allEffects(lmer_NiPdScreenSleepFiber), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


# just nickel & period (only two significant ones)
lmer_NiPd <- glm(migraine ~ nickel * menstrual_cycle, family = binomial, data = D[D$menstrual_phase != "ovulation",])
summary(lmer_NiPd)
plot(allEffects(lmer_NiPd))
plot(allEffects(lmer_NiPd), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(lmer_NiPd)[2]) # nickel odds. every 1 ug increase in nickel odds of having migraine multiples by 1.015
exp(coef(lmer_NiPd)[2]*10) # 15% increase odds for every 10 ug of nickel
exp(coef(lmer_NiPd)[3]) # period odds. every day in cycle odds of hacing migraine decreases.. mulitples by 92%. 92% chance have migraine compared to day before in cycle


# just nickel & period & fiberlag2 
lmer_NiPdFi <- glm(migraine ~ nickel + menstrual_cycle + fiberlag2, family = binomial, data = D[D$menstrual_phase != "ovulation",])
summary(lmer_NiPdFi)
plot(allEffects(lmer_NiPdFi))
plot(allEffects(lmer_NiPdFi), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(lmer_NiPdFi)[2]) # nickel odds. every 1 ug increase in nickel odds of having migraine multiples by 1.018
exp(coef(lmer_NiPdFi)[2]*10) # 19% increase odds for every 10 ug of nickel
exp(coef(lmer_NiPdFi)[3]) # period odds. every day in cycle odds of having migraine decreases.. mulitples by 91%. 91% chance have migraine compared to day before in cycle
exp(coef(lmer_NiPdFi)[4]) # fiberlag2 odds. every 1 gram of fiber decreasing odds of migraine by 15%


# just nickel & fiberlag2 
lmer_NiFi <- glm(migraine ~ nickel * fiberlag2, family = binomial, data = D[D$menstrual_phase != "ovulation",])
summary(lmer_NiFi)
plot(allEffects(lmer_NiFi))
plot(allEffects(lmer_NiFi), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(lmer_NiFi)[2]) # nickel odds. every 1 ug increase in nickel odds of having migraine multiples by 1.013
exp(coef(lmer_NiFi)[2]*10) # 14% increase odds for every 10 ug of nickel
exp(coef(lmer_NiFi)[3]) # fiberlag2 odds. every 1 gram of fiber decreasing odds of migraine by 10%


# Nickl, Fiber, Tyramine
lmer_NiFiTy <- glm(migraine ~ nickel * fiberlag2 * tyramineAvg2, family = binomial, data = D)
summary(lmer_NiFiTy)

lmer_NiFiTy <- glm(migraine ~ nickel + fiberlag2 + tyramineAvg2, family = binomial, data = D)
summary(lmer_NiFiTy) # only fiber lag2 sig
plot(allEffects(lmer_NiFiTy))
plot(allEffects(lmer_NiFiTy), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(lmer_NiFiTy)[2]) # nickel odds. every 1 ug increase in nickel odds of having migraine multiples by 1.014
exp(coef(lmer_NiFiTy)[2]*10) # 1.15 a 15% increase odds for every 10 ug of nickel
exp(coef(lmer_NiFiTy)[3]) # fiberlag2 odds = 0.85. every 1 gram of fiber decreasing odds of migraine by 15%
exp(coef(lmer_NiFiTy)[4]) # tyramineAvg2 odds = 1.53. every 1 item of tyramine containing food increases odds of getting migraine by 53%

# Nicel, fiber, histamine
lmer_NiFiHi <- glm(migraine ~ nickel + fiberlag2 + histamineAvg2, family = binomial, data = D)
summary(lmer_NiFiHi) # only fiber lag2  and histamine Avg 2 sig
plot(allEffects(lmer_NiFiHi))
plot(allEffects(lmer_NiFiHi), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(lmer_NiFiHi)[2]) # nickel odds. every 1 ug increase in nickel odds of having migraine multiples by 1.011
exp(coef(lmer_NiFiHi)[2]*10) # 1.12 a 12% increase odds for every 10 ug of nickel
exp(coef(lmer_NiFiHi)[3]) # fiberlag2 odds = 0.83. every 1 gram of fiber decreasing odds of migraine by 17%
exp(coef(lmer_NiFiHi)[4]) # tyramineAvg2 odds = 2.05. every 1 serving of high-histamine  food increases odds of getting migraine by 205%


# nickel avg over two days + period
lmer_NiAvgPd <- glm(migraine ~ nickelAvg2 + menstrual_cycle, family = binomial, data = D[D$menstrual_phase != "ovulation",])
summary(lmer_NiAvgPd)
plot(allEffects(lmer_NiAvgPd))
plot(allEffects(lmer_NiAvgPd), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 
exp(coef(lmer_NiAvgPd)[2]) # nickel odds. every 1 ug increase in nickel odds of having migraine multiples by 1.015
exp(coef(lmer_NiAvgPd)[2]*10) # 15% increase odds for every 10 ug of nickel
exp(coef(lmer_NiAvgPd)[3]) # period odds. every day in cycle odds of hacing migraine decreases.. mulitples by 92%. 92% chance have migraine compared to day before in cycle


# model with migraine severity
# should i be treating date or day as random effect? b/c longitudinal
lmer_NiAvgPdSeverity <- lm(severity ~ nickelAvg2 + menstrual_cycle, data = D)
summary(lmer_NiAvgPdSeverity)
plot(allEffects(lmer_NiAvgPdSeverity), type = "response", main = "Effects Plot",  ylab = "Migraine Severity") 


lmer_NiAvgPdFiSeverity <- lm(severity ~ nickelAvg2 + menstrual_cycle + fiberlag2, data = D)
summary(lmer_NiAvgPdFiSeverity)
plot(allEffects(lmer_NiAvgPdFiSeverity), type = "response", main = "Effects Plot",  ylab = "Migraine Severity") 


lmer_NiAvgPdScreenSeverity <- lm(severity ~ nickelAvg2 + menstrual_cycle + totalScreenAvg2, data = D)
summary(lmer_NiAvgPdScreenSeverity)
plot(allEffects(lmer_NiAvgPdScreenSeverity), type = "response", main = "Effects Plot",  ylab = "Migraine Severity") 


## Mixed effects migraine with pressure
lmer_NiAvgPdPressSeverity <- lm(severity ~ nickelAvg2 + menstrual_cycle + pressure_minMinusMaxYesterday, data = D)
summary(lmer_NiAvgPdPressSeverity)
plot(allEffects(lmer_NiAvgPdPressSeverity), type = "response", main = "Effects Plot",  ylab = "Migraine Severity") 


# Cross-correlation #####
D_sevNi <- D[, c("nickel", "severity", "Date")]
D_sevNi <- drop_na(D_sevNi)
ccfvalues = ccf(D_sevNi$severity, D_sevNi$nickel)
ccfvalues

# CC time_asleep
D_sevSleep <- D[, c("time_asleep", "severity", "Date")]
D_sevSleep <- drop_na(D_sevSleep)
ccfvalues = ccf(D_sevSleep$severity, D_sevSleep$time_asleep)
ccfvalues

## screen time
D_sevScreen <- D[, c("total_screen", "severity", "Date")]
D_sevScreen <- drop_na(D_sevScreen)
ccfvalues = ccf(D_sevScreen$severity, D_sevScreen$total_screen)
ccfvalues
## Lag of 5 days = highest correlation # NB. may not be exactly 5 days d/t missing data

# models with lag ####

# SCREEN LAG MODEL
glm_screenLag <- glm(migraine ~ total_screen + screenlag1 + screenlag2 + screenlag3 + screenlag4 + screenlag5, family = binomial, data = D)
summary(glm_screenLag)
plot(allEffects(glm_screenLag))

# NICKEL LAG MODEL
glm_nickelLag <- glm(migraine ~ nickel + nickellag1 + nickellag2 + nickellag3 + nickellag4 + nickellag5, family = binomial, data = D)
summary(glm_nickelLag)
plot(allEffects(glm_nickelLag))

# LMER with screenlag2
lmer_NiPdScreenLag <- glm(migraine ~ nickel + screenlag2 + menstrual_cycle, family = binomial, data = D[D$menstrual_phase != "ovulation",])
summary(lmer_NiPdScreenLag)
plot(allEffects(lmer_NiPdScreenLag), type = "response", main = "Effects Plot",  ylab = "P(Migraine)") 


##### Decision trees for THRESHOLD #####
library(rpart)
library(rpart.plot)
library(kableExtra)

#### NI decisioan tree #####
# create data frames with just Migraine & Nickel
D_Ni <- D[,c("migraine", "nickel")]
D_Ni <- D_Ni[!is.na(D_Ni$nickel),]

# grow tree
fit_Ni <- rpart(migraine ~ nickel, method="class", data = D_Ni)

# plot tree
prp(fit_Ni, faclen = 0, cex = 0.8, extra = 1)
title("Migraine ~ Nickel Decision Tree", cex.sub = .8)

# make  confusion matrix ####
conf.matrixNi <- table(D_Ni$migraine, predict(fit_Ni,type="class"))
#rownames(conf.matrixASR) <- paste("Actual", rownames(conf.matrixASR), sep = ":")
#colnames(conf.matrixASR) <- paste("Pred", colnames(conf.matrixASR), sep = ":")
#calculate accuracy for each row
mySum = 0

#for (i in 1:ncol(conf.matrixNi)) { mySum = mySum + conf.matrixNi[i,i] }
#ASR_accuracy = mySum / sum(conf.matrixNi)# Make accuracy column
# Make accuracy column
Accuracy <- c()
for (i in 1:ncol(conf.matrixNi)) {Accuracy[i] <-  conf.matrixNi[i,i] / sum(conf.matrixNi[i, ]) }
conf.matrixNi <- cbind(conf.matrixNi, Accuracy)
conf.matrixNi



#### fiberlag2 decisioan tree #####
# create data frames with just Migraine & fiber lag 2
D_Fi <- D[,c("migraine", "fiberlag2")]
D_Fi <- D_Fi[!is.na(D_Fi$fiberlag2),]

# grow tree
fit_Fi <- rpart(migraine ~ fiberlag2, method="class", data = D_Fi)

# plot tree
prp(fit_Fi, faclen = 0, cex = 0.8, extra = 1)
title("Migraine ~ fiberlag2 Decision Tree", cex.sub = .8)

# make  confusion matrix ####
conf.matrixFi <- table(D_Fi$migraine, predict(fit_Fi,type="class"))

#calculate accuracy for each row
Accuracy <- c()
for (i in 1:ncol(conf.matrixFi)) {Accuracy[i] <-  conf.matrixFi[i,i] / sum(conf.matrixFi[i, ]) }
conf.matrixFi <- cbind(conf.matrixFi, Accuracy)
conf.matrixFi


#### menstruation decisioan tree #####
# create data frames with just Migraine & Mensutral cycle
D_Mens <- D[,c("migraine", "menstrual_cycle")]
D_Mens <- D_Mens[!is.na(D_Mens$menstrual_cycle),]

# grow tree
fit_Mens <- rpart(migraine ~ menstrual_cycle, method="class", data = D_Mens)

# plot tree
prp(fit_Mens, faclen = 0, cex = 0.8, extra = 1)
title("Migraine ~ menstrual_cycle Decision Tree", cex.sub = .8)

# make  confusion matrix ####
conf.matrixMens <- table(D_Mens$migraine, predict(fit_Mens,type="class"))

#calculate accuracy for each row
Accuracy <- c()
for (i in 1:ncol(conf.matrixMens)) {Accuracy[i] <-  conf.matrixMens[i,i] / sum(conf.matrixMens[i, ]) }
conf.matrixMens <- cbind(conf.matrixMens, Accuracy)
conf.matrixMens
