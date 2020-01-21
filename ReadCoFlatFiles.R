
library(readr)
library(dplyr)
library(lmtest)


# Store the format csv files in the same folder as the programs and set that as the default directory.
setwd("C:\\CohortBirths\\CDCflatFiles")
 





dat <- read.table(header=TRUE, text="
varr         start end   form
bfacil       32    32    n
mager        75    76    n
mbstate_rec  84    84    n
restatus     104   104   n
mracehisp    117   117   n
mar_p        119   119   c
dmar         120   120   n
meduc        124   124   n
fagecomb     147   148   n
fracehisp    162   162   n
feduc        163   163   n
tbo_rec      182   182   n
ilop_r       206   208   n
ilop_r11     209   210   n
precare5     227   227   n
previs       238   239   n
cig_0        253   254   n
cig_1        255   256   n
cig_2        257   258   n
cig_3        259   260   n
mhtr         280   281   n
bmi          283   286   n
pwgt_r       292   294   n
dwgt_r       299   301   n

rf_pdiab     313   313   c
rf_gdiab     314   314   c
rf_phype     315   315   c
rf_ghype     316   316   c
rf_ehype     317   317   c
rf_ppb       318   318   c
rf_inft      325   325   c
rf_drg       326   326   c
rf_art       327   327   c

no_risks     337   337   n

ip_gon       343   343   c
ip_syph      344   344   c
ip_chlam     345   345   c
ip_hepb      346   346   c
ip_hepc      347   347   c

 
ld_indl      389   389   c
ld_augm      390   390   c

apgar5       444   445   n
apgar10      448   449   n
dplural      454   454   n
sex          475   475   c
combgest     490   491   n
brthwgt      512   515   n
uOB_Indu     1337  1337  n
aged         1356  1358  n
manner       1362  1362  n
")

colpos <- fwf_positions( 
	start=dat$start, 
	end=dat$end, 
	col_names = dat$varr)

ctypes <- paste(dat$form, collapse="")



Clean_dat <- function(data) {
	data %>% 
		mutate(
			died24 = 	ifelse(!is.na(aged) & aged == 0, 1, 0 ),
			usres =         ifelse(restatus %in% c(1, 2, 3), 1, 0),
			mbstate_rec = 	recode(mbstate_rec, 
						'1' = "BornInUS", 
						'2' = "NotBornInUS", 
						'3' = "Unknown"),
			bfacil = 	recode(bfacil,
						'1' = "Hospital",
						'2' = "Freestanding Birth Center",
						'3' = "Home (intended)",
						'4' = "Home (not intended)",
						'5' = "Home (unknown if intended)",
						'6' = "Clinic / Doctor’s Office",
						'7' = "Other",
						'9' = "Unknown"),
			mracehisp = 	recode(mracehisp, 
						'1' = "NonHispWhite",
						'2' = "NonHispBlack",
						'3' = "NonHispAIAN",
						'4' = "NonHispAsian",
						'5' = "NonHispNHOPI",
						'6' = "NonHispMultRace",
						'7' = "Hisp",
						'8' = "Unknown"),
			mar_p = 	recode(mar_p,  
						Y = "PatAckn",
						N = "NotPatAckn",
						U = "UnknPatAckn",
						X = "PatAckNotApp"),
			dmar = 		recode(dmar,
						'1' = "Married",
						'2' = "Unmarried"),
			meduc = 	recode(meduc,
						'1' = "8th Gr or Less",
						'2' = "Some High School",
						'3' = "HS Grad or GED",
						'4' = "Some Coll",
						'5' = "Assoc Deg",
						'6' = "Bach Deg",
						'7' = "Mast Deg",
						'8' = "PHD",
						'9' = "Unknown"),
			fagecomb = 	na_if(fagecomb, 99),
			fracehisp = 	recode(fracehisp, 
						'1' = "NonHispWhite",
						'2' = "NonHispBlack",
						'3' = "NonHispAIAN",
						'4' = "NonHispAsian",
						'5' = "NonHispNHOPI",
						'6' = "NonHispMultRace",
						'7' = "Hisp",
						'8' = "Unknown",
						'9' = "Unknown"), # Lumping unknown nonhispanic into unknown
			feduc = 	recode(feduc,
						'1' = "8th Gr or Less",
						'2' = "Some High School",
						'3' = "HS Grad or GED",
						'4' = "Some Coll",
						'5' = "Assoc Deg",
						'6' = "Bach Deg",
						'7' = "Mast Deg",
						'8' = "PHD",
						'9' = "Unknown"),
			first = 	ifelse(tbo_rec == 1, 1, 0),
			first = 	na_if(first, 9),
			birthint1 = 	ifelse((ilop_r11 > 5 & ilop_r11  !=88), 10, ilop_r11),
			birthint1 = 	na_if(birthint1, 99),
			precare5 = 	recode(precare5,
						'1' = "1st-3rd Month",
						'2' = "4th- 6th Month",
						'3' = "7th - Final Month",
						'4' = "No PreNat Care",
						'5' = "Unknown"),
			previs = 	na_if(previs, 99),
			cig_0 = 	na_if(cig_0, 99),  
			cig_1 = 	na_if(cig_1, 99), 
			cig_2 = 	na_if(cig_2, 99), 
			cig_3 = 	na_if(cig_3, 99), 
			mhttr = 	na_if(mhtr, 99), 
			bmi = 		na_if(bmi, 99.9), 
			pwgt_r = 	na_if(pwgt_r, 999),  
			dwgt_r = 	na_if(dwgt_r, 999),  
			apgar5 = 	na_if(apgar5, 99), 
			apgar10 = 	na_if(apgar10, 99), 
			combgest = 	na_if(combgest,  99),  
			brthwgt = 	na_if(brthwgt, 9999),
			combgest2  =  	combgest^2,
			combgest3  =  	combgest^3,
			brthwgt2   =  	brthwgt ^2,
			brthwgt3   = 	brthwgt ^3,
			twin       = 	ifelse(dplural   == 2, 1, 0),
			triplet    = 	ifelse(dplural   == 3, 1, 0),
			quadruplet = 	ifelse(dplural   == 4, 1, 0),
			quintuplet = 	ifelse(dplural   >  4, 1, 0),
			black      = 	ifelse(mracehisp == "NonHispBlack", 1, 0),
			aian       = 	ifelse(mracehisp == "NonHispAIAN", 1, 0),
			asian      = 	ifelse(mracehisp == "NonHispAsian", 1, 0),
			nhopi      = 	ifelse(mracehisp == "NonHispNHOPI", 1, 0),
			multrace   = 	ifelse(mracehisp == "NonHispMultRace", 1, 0),
			hisp       = 	ifelse(mracehisp == "Hisp", 1, 0),
			unkrace    = 	ifelse(mracehisp == "Unknown", 1, 0),
			otherrace  =	ifelse(aian == 1 | nhopi == 1, 1, 0),

			someHi	   = 	ifelse(meduc == "Some High School", 1, 0),
			hiGrad     =	ifelse(meduc == "HS Grad or GED", 1, 0),
			someCol	   =    ifelse(meduc == "Some Coll", 1, 0),
			aDeg	   =    ifelse(meduc == "Assoc Deg", 1, 0),
			bach	   =    ifelse(meduc == "Bach Deg", 1, 0),
			mast	   =    ifelse(meduc == "Mast Deg", 1, 0),
			phd	   =    ifelse(meduc == "PHD", 1, 0),
			unkEd	   =    ifelse(meduc == "Unknown", 1, 0),
			boy	   =  	ifelse(sex   == "M", 1, 0),
			dplural2   =    dplural^2,
			induction  = 	recode(uOB_Indu,
						'1' = "1",
						'2' = "0",
						'9' = "Unknown"),
			gest2	   =	ifelse(combgest < 20, 20, combgest)
			)
			}



Keep_Vars <- function(data) {
	data %>% 
	select(year, combgest, gest2,  induction, bmi,  dplural) %>%
	filter(complete.cases(.))
	}

SampPerc <- function(data, perc )	{
	data %>%
	mutate(	rand = rnorm(nrow(data))) %>%
	arrange(rand) %>%
	mutate(SampIncl = ifelse(row_number() <= nrow(data)*perc, 1, 0)) %>%
	select(-rand)
	}

SampN <- function(data, n )	{
	data %>%
	mutate(	rand = rnorm(nrow(data))) %>%
	arrange(rand) %>%
	mutate(SampIncl = ifelse(row_number() <= n, 1, 0)) %>%
	select(-rand)
	}




B2014 <- read_fwf("VS14LKBC.PublicUse.DUSDENOM_2019-08-22"   , col_positions=colpos, col_types=ctypes) %>% mutate(year = 2014)
B2015 <- read_fwf("VS15LKBC.PublicUse.DUSDENOM"   , col_positions=colpos, col_types=ctypes) %>% mutate(year = 2015)



set.seed = 123
Dat <- rbind(B2014, B2015)  %>% Clean_dat() %>% Keep_Vars() %>% SampPerc(.75)

Training <- Dat %>% filter(SampIncl == 1)
HoldOut <- Dat %>% filter(SampIncl == 0)


Train <- Training %>% SampN(10000) %>% filter(SampIncl == 1)


#######################  Save R files

saveRDS(Training, "Training.rds")
saveRDS(HoldOut, "HoldOut.rds")














####################  Sample Cox Model

library(survival)
 

surv_object <- Surv(time = Train$gest2, event = rep(T,nrow(Train)))
fit.coxph <- coxph(surv_object ~ dplural , data = Train)
summary(fit.coxph)
####################  End Sample Cox Model


####################  Sample MGCV Cox Model

library(mgcv)


fit.coxph.mgcv <- gam(gest2 ~ s(bmi) , data = Train, family=cox.ph(), weights= rep(1, nrow(Train)) )
summary(fit.coxph.mgcv)
####################  End Sample Cox Model









####################  Sample Discrete Time Hazard Model

library(data.table)
Train <- Train %>% mutate(case = 1:nrow(Train)) %>% setDT()


exd <- Train[rep(seq(.N), gest2-19), ]
exd[, week    := 1]
exd[, start   := cumsum(week)+18, case]
exd[, stop    := cumsum(week)+19, case]
exd[, event   := ifelse(stop == gest2, 1,  0)]

dthm <- glm(event ~ bmi + gest2, family=binomial(link=cloglog), data =exd)


table(exd$start)

#saveRDS(Training, "Training.rds")
#saveRDS(HoldOut, "HoldOut.rds")




 
