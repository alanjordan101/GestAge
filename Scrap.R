
options(scipen=999)

library(tidyverse)
library(survival)
library(mgcv)

#################################################################
# Load Data
Train <- readRDS("C:\\CohortBirths")
#################################################################




#################################################################
# Create survival object for Cox Proportional Hazard Model

surv <- Surv(Train$combgest, rep(1,nrow(Train)))

mod1 <- coxph(surv ~ bmi, data=Train, ties="exact", coxph.control(iter.max = 200))

mod2 <- coxph(surv ~ bmi + I(bmi * log(combgest)), data=Train)
#################################################################


#################################################################
#Translate Data to STATA

library(haven)
write_dta(Train, "c:/CohortBirths/Train.dta")
#################################################################


#################################################################
#Create Data file for Discrete Time Hazard Model

library(data.table)
library(mgcv)

Train <- Train %>% mutate(case = 1:nrow(Train)) %>% setDT()

exd <- Train[rep(seq(.N), gest2-19), ]

exd[, week    := 1]
exd[, start   := cumsum(week)+18, case]
exd[, stop    := cumsum(week)+19, case]
exd[, event   := ifelse(stop == gest2, 1,  0)] 

exd <- data.frame(exd)


gam1 <- gam(event ~ I(gest2^-2.5) + bmi , family=binomial(link=cloglog), data =exd)
#################################################################








dthm <- glm(event ~ bmi , family=binomial(link=cloglog), data =exd)
