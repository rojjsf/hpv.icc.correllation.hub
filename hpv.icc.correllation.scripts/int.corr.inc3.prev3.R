

library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)

#### a table for the countries with good prevalence data but incidence estimates ####
###############################################
 #### 1. incidence ####
################################################
l <- c( "*Viet Nam, Hanoi",
        "Pakistan, South Karachi",
        "Bhutan",
        "Mongolia, Ulanbataar",
        "Nepal, Bharatpur",
        "Rwanda, Kigali",
        "Mexico, Morelos State",
        "Georgia, Tbilisi",
        "Vanuatu, Port Vila",
        "Fiji, Suva",
        "Nigeria, Ibadan",
        "Guinea, Conakry")

inc3 <- data.frame(matrix(nrow = length(l), ncol = 31))
colnames(inc3) <- colnames(inc)

inc3$loc <- c( "*Viet Nam, Hanoi",
              "Pakistan, South Karachi",
               "Bhutan",
              "Mongolia, Ulanbataar",
              "Nepal, Bharatpur",
              "Rwanda, Kigali",
              "Mexico, Morelos State",
              "Georgia, Tbilisi",
              "Vanuatu, Port Vila",
              "Fiji, Suva",
              "Nigeria, Ibadan",
              "Guinea, Conakry")

inc3$REGISTRY <- c("125",# vietnam, hanoi
                  "45860199", # pakistan
                   NA,# bhutan
                   NA, # mongolia
                  NA, # nepal
                  NA, # rwanda
                   NA, # mexico
                  NA, # georgia
                  NA, # Vanuatu
                  NA, # Fiji
                  NA, # Nigeria
                  NA) #Guinea

inc3$cid <- c("14", # vietnam, hanoi
               "25",# pakistan
              "11", # bhutan
              "31",# mongolia
              "34",# nepal
              "32", # rwanda
              "26", # mexico
              "30", # georgia
              "33", # Vanuatu
              "28", # Fiji
              "7", # Nigeria
              "29") #Guinea

inc3$years <- c("1993-1997",# vietnam, hanoi
                "1998-2002",# pakistan
                "est", # estimate D2 indian districts
                "est", # mongolia
                "est", # nepal
                "est", # rwanda
                "est", # mexico
                "est", # georgia
                "est", # Vanuatu
                "est", # Fiji
                "est", # Nigeria
                "est") #Guinea

inc3$sgcentre <- c("1", # hanoi
                   "60", # pakistan
                   "65",# bhutan
                   "25", # mongolia
                   "42",# nepal
                   "66", # Rwanda
                   "8", # Mexico
                   "45", # georgia
                   "64", # Vanuatu
                   NA, # Fiji
                   "10", # Nigeria
                   "11") #Guinea

#### 1.1 vietnam, hanoi #### 
ci58 <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-VIII/CI5-VIII.csv")
viet2 <- ci58 %>%
  filter(X1 == 125)%>%
  filter(X1.1 == 2) %>% # females
  filter(X1.2 == 120) # cervical cancer

viet2[, c("X1", "X1.1", "X1.2")] <- NULL
inc3[inc3$cid == 14, 2:14] <- viet2[4:16, "X67"] # cases
inc3[inc3$cid == 14, 15:27] <- viet2[4:16, "X515691"] # pyears


 #### 1.2 pakistan, karachi ####

pak.data <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-IXd/45860199.csv", header = TRUE)
head(pak.data)
pak <- pak.data %>%
  filter(X1 == 2) %>% # females
  filter(X001 == 117) # cervical cancer
pak[, c("X1", "X001")] <- NULL
inc3[inc3$cid == 25, 2:14] <- pak[3:15, "X58"] # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 25, 15:27] <- pak[3:15, "X652475"] # pyears


 
 ####1.3 bhutan ####

bhut.inc <- c(0,	0,	2.9,	10.5,	17.5,	26.3,	38.7,	44.7,	44.3,	36.7,	31.4,	19.8,	15.1)
# downloaded from ICO on 17/07/2018
# Incidence rates (2007-2008, Tashi Dendup, personal communication) were scaled using site and s
# sex-specific percentages of microscopically verified cases obtained from the mean of four Indian cancer registries (Dibrugarh, Kamrup, Mizoram and Sikkim)
# and applied to 2012 population., D2
inc3[inc3$cid == 11, 2:14] <- bhut.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 11, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### 1.4 mongolia ####
mong.inc <- c(0,	1.3,	8.5,	17.8,	32.1,	47.5,	62.3,	73.9,	76, 71.9,	65.1,	55.3,	53.6)
# downloaded from ICO on 18/07/2018
# most recent national incidence rates projected to 2012 population, D5
inc3[inc3$cid == 31, 2:14] <- mong.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 31, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### 1.5 Nepal ####

nepal.inc <- c(0,	0.5,	4.3,	8.5,	22.9,	36.6,	53.3,	74.6,	63.9,	68.5,	45.7,	32.9,	17.3)
# downloaded from ICO on 18/07/2018
# estimates using Indian data, G6
inc3[inc3$cid == 34, 2:14] <- nepal.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 34, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

 #### 1.6. Rwanda, Kigali ####
rwa.inc <- c(0.7,	0.4,	8.5,	22.3,	42,	68.1,	104.9,	137.5,	156,	151.5,	137.2,	113.9,	79.6)
# downloaded from ICO on 18/07/2018
# estimate from Tanzanian and Ugandian data (1990), F6
inc3[inc3$cid == 32, 2:14] <- rwa.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 32, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### 1.6. Mexico, Morelos State ####
mex.inc <- c(0.3,	6.5,	16.5,	26.8,	38.5,	46.7,	51.1,	53.2,	54.2,	55.5,	57.5,	59.9,	62.9)
# downloaded from ICO on 18/07/2018
# estimate from estimated mortality data using modelled survival,  E6
inc3[inc3$cid == 26, 2:14] <- mex.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 26, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### 1.7. Georgia, tbilisi ####
# downloaded from ICO on 25/07/2018
# Estimated from estimated national mortality (2001-2010) for 2012 and modelled survival.
georg.inc <- c(0,	0,	0,	19.5,	31.6,	38.7,	40.6,	40.1,	35.2,	27.8,	21.7,	17.1,	14.2)
inc3[inc3$cid == 30, 2:14] <- georg.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 30, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### 1.8. vanuatu, port vila ####
# downloaded from ICO on 25/07/2018
# Incidence rates (1999-2003) applied to the 2012 population.
van.inc <- c(0,	0,	19.3,	46,	14,	46.3,	52.3,	43.8,	29.3,	80.1,	53.8,	0,	0)
inc3[inc3$cid == 33, 2:14] <- van.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 33, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### 1.9 fiji #### 
# downloaded from ICO on 25/07/2018
#Site-specific incidence rates for all ages were estimated from estimated national mortality data (2009) for 2012 
#and modelled survival and partitioned using sex- and age-specific proportions from Fidji cancer registry (1991-1999).
fiji.inc <- c(2.6,	2.7,	16.5,	23.6,	74.4,	101.6,	96.4,	100.2,	70.2,	128.7,	74.3,	84.7,	58.1)
inc3[inc3$cid == 28, 2:14] <- fiji.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 28, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)


#### 1.10. nigeria, ibadan ####
# downloaded from ICO on 25/07/2018
#We computed a simple average of the rates from:
#  Abuja Cancer Registry (2009-2012) ï¿½scaledï¿½ using site- and sex-specific percentages of microscopically verified cases obtained from Ibadan (2006-2009).
#Calabar Cancer Registry (2009-2011) ï¿½scaledï¿½ using site- and sex-specific percentages of microscopically verified cases obtained from Ibadan (2006-2009).
#Ibadan Cancer Registry (2006-2009).
#The rates were applied to 2012 national population to obtain the estimated number of new cancer cases by sex and site for all ages. The estimated ï¿½all agesï¿½ numbers 
#were partitioned by age-group using sex-, site- and age-specific distributions obtained from Ibadan cancer registry (2000-2009).
nig.inc <- c(0,	0.4,	1.9,	8.9,	21.2,	39.7,	61.3,	79.5,	97.6,	114.8,	123.8,	125.8,	120.2)
inc3[inc3$cid == 7, 2:14] <- nig.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 7, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)


#### 1.11 guinea ####
# downloaded from ICO on 25/07/2018
# Incidence rates from Conakry (2004-2009) covering around 20% of the population were applied to 2012 population.
guin.inc <- c(0.5,	1.9,	8,	18.5,	33.8,	52.1,	68.3,	90.8,	112.8,	143.1,	168.5,	187.2,	202.4)
inc3[inc3$cid == 29, 2:14] <- guin.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 29, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)




##################################################
 #### 2. prevalence ####
############################################

#own prev3.pooled table to avoid confusion
# prev3.pooled has same structure as prev.pooled
m <- matrix(data = NA, nrow = length(l), ncol = length(levels(pooled.hrisk$age.grp)) + 5)
prev3.pooled <- data.frame(m)
colnames(prev3.pooled) <- c("cid", "sgcentre", "loc", "Year", "n", levels(pooled.hrisk$age.grp))
prev3.pooled$loc <- inc3$loc
prev3.pooled$cid <- inc3$cid
prev3.pooled$sgcentre <- inc3$sgcentre


#### 2.1 vietnam, hanoi #### 
viet2.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 1, ]
n.viet2 <- dim(viet2.hrisk)[1]
viet2.table <- table(viet2.hrisk$age.grp, viet2.hrisk$hpvsino)
viet2.table <- as.data.frame.matrix(viet2.table)

viet2.table <- viet2.table %>%
  mutate("cid" = 14) %>%
  mutate("loc" = "viet2") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

viet2.table
prev3.pooled[prev3.pooled$cid == 14, 6:(dim(prev3.pooled)[2])] <- viet2.table$prev 
prev3.pooled[prev3.pooled$cid == 14, "n"] <- dim(viet2.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 14, "sgcentre"] <- 1
prev3.pooled[prev3.pooled$cid == 14, "Year"] <- "1993-1997"



#### 2.2 pakistan, karachi ###

pak.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 60, ]
n.pak <- dim(pak.hrisk)[1]
pak.table <- table(pak.hrisk$age.grp, pak.hrisk$hpvsino)
pak.table <- as.data.frame.matrix(pak.table)

pak.table <- pak.table %>%
  mutate("cid" = 25) %>%
  mutate("loc" = "pak") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

pak.table
prev3.pooled[prev3.pooled$cid == 25, 6:(dim(prev3.pooled)[2])] <- pak.table$prev 
# is added to general pooled data table. 
# shouldnt be a problem for analsys of quality = 1 only, as incidence will be missing in prev.inc.table
prev3.pooled[prev3.pooled$cid == 25, "n"] <- dim(pak.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 25, "sgcentre"] <- 60
prev3.pooled[prev3.pooled$cid == 25, "Year"] <- "1993-1997"



#### 2.3 bhutan ####

bhut.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 65, ]
n.bhut <- dim(bhut.hrisk)[1]
bhut.table <- table(bhut.hrisk$age.grp, bhut.hrisk$hpvsino)
bhut.table <- as.data.frame.matrix(bhut.table)

bhut.table <- bhut.table %>%
  mutate("cid" = 11) %>%
  mutate("loc" = "bhut") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

bhut.table
prev3.pooled[prev3.pooled$cid == 11, 6:(dim(prev3.pooled)[2])] <- bhut.table$prev 
# is added to general pooled data table. 
# shouldnt be a problem for analsys of quality = 1 only, as incidence will be missing in prev.inc.table
prev3.pooled[prev3.pooled$cid == 11, "n"] <- dim(bhut.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 11, "sgcentre"] <- 65
prev3.pooled[prev3.pooled$cid == 11, "Year"] <- "est"


#### 2.4 mongolia, ulanbataar ####

mong.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 25, ]
n.mong <- dim(mong.hrisk)[1]
mong.table <- table(mong.hrisk$age.grp, mong.hrisk$hpvsino)
mong.table <- as.data.frame.matrix(mong.table)

mong.table <- mong.table %>%
  mutate("cid" = 31) %>%
  mutate("loc" = "mong") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

mong.table
prev3.pooled[prev3.pooled$cid == 31, 6:(dim(prev3.pooled)[2])] <- mong.table$prev 
prev3.pooled[prev3.pooled$cid == 31, "n"] <- dim(mong.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 31, "sgcentre"] <- 25
prev3.pooled[prev3.pooled$cid == 31, "Year"] <- "est"


#### 2.5 nepal, bharatpur ####

nepal.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 42, ]
n.nepal <- dim(nepal.hrisk)[1]
nepal.table <- table(nepal.hrisk$age.grp, nepal.hrisk$hpvsino)
nepal.table <- as.data.frame.matrix(nepal.table)

nepal.table <- nepal.table %>%
  mutate("cid" = 34) %>%
  mutate("loc" = "nepal") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

nepal.table
prev3.pooled[prev3.pooled$cid == 34, 6:(dim(prev3.pooled)[2])] <- nepal.table$prev 
prev3.pooled[prev3.pooled$cid == 34, "n"] <- dim(nepal.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 34, "sgcentre"] <- 42
prev3.pooled[prev3.pooled$cid == 34, "Year"] <- "est"


#### 2.6 Rwanda, Kigali ####

rwa.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 66, ]
n.rwa <- dim(rwa.hrisk)[1]
rwa.table <- table(rwa.hrisk$age.grp, rwa.hrisk$hpvsino)
rwa.table <- as.data.frame.matrix(rwa.table)

rwa.table <- rwa.table %>%
  mutate("cid" = 32) %>%
  mutate("loc" = "rwa") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

rwa.table
prev3.pooled[prev3.pooled$cid == 32, 6:(dim(prev3.pooled)[2])] <- rwa.table$prev 
prev3.pooled[prev3.pooled$cid == 32, "n"] <- dim(rwa.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 32, "sgcentre"] <- 66
prev3.pooled[prev3.pooled$cid == 32, "Year"] <- "est"


#### 2.7 Mexico, Morelos State ####

mex.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 8, ]
n.mex <- dim(mex.hrisk)[1]
mex.table <- table(mex.hrisk$age.grp, mex.hrisk$hpvsino)
mex.table <- as.data.frame.matrix(mex.table)

mex.table <- mex.table %>%
  mutate("cid" = 26) %>%
  mutate("loc" = "mex") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

mex.table
prev3.pooled[prev3.pooled$cid == 26, 6:(dim(prev3.pooled)[2])] <- mex.table$prev 
prev3.pooled[prev3.pooled$cid == 26, "n"] <- dim(mex.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 26, "sgcentre"] <- 8
prev3.pooled[prev3.pooled$cid == 26, "Year"] <- "est"


#### 2.8 Georiga, Tbilisi ####

georg.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 45, ]
n.georg <- dim(georg.hrisk)[1]
georg.table <- table(georg.hrisk$age.grp, georg.hrisk$hpvsino)
georg.table <- as.data.frame.matrix(georg.table)

georg.table <- georg.table %>%
  mutate("cid" = 30) %>%
  mutate("loc" = "georg") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

georg.table
prev3.pooled[prev3.pooled$cid == 30, 6:(dim(prev3.pooled)[2])] <- georg.table$prev 
prev3.pooled[prev3.pooled$cid == 30, "n"] <- dim(georg.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 30, "sgcentre"] <- 45
prev3.pooled[prev3.pooled$cid == 30, "Year"] <- "est"

#### 2.9 Vanuatu, Port Vila ####

van.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 64, ]
n.van <- dim(van.hrisk)[1]
van.table <- table(van.hrisk$age.grp, van.hrisk$hpvsino)
van.table <- as.data.frame.matrix(van.table)

van.table <- van.table %>%
  mutate("cid" = 33) %>%
  mutate("loc" = "van") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

van.table
prev3.pooled[prev3.pooled$cid == 33, 6:(dim(prev3.pooled)[2])] <- van.table$prev 
prev3.pooled[prev3.pooled$cid == 33, "n"] <- dim(van.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 33, "sgcentre"] <- 64
prev3.pooled[prev3.pooled$cid == 33, "Year"] <- "est"

#### 2.10 Fiji, Suva ####

#mex.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 8, ]
#n.mex <- dim(mex.hrisk)[1]
#mex.table <- table(mex.hrisk$age.grp, mex.hrisk$hpvsino)
#mex.table <- as.data.frame.matrix(mex.table)

prev3.pooled[prev3.pooled$cid == 7, "Year"] <- "est"

#### 2.11 Nigeria, Ibadan ####

nig.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 10, ]
n.nig <- dim(nig.hrisk)[1]
nig.table <- table(nig.hrisk$age.grp, nig.hrisk$hpvsino)
nig.table <- as.data.frame.matrix(nig.table)

nig.table <- nig.table %>%
  mutate("cid" = 7) %>%
  mutate("loc" = "nig") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

nig.table
prev3.pooled[prev3.pooled$cid == 7, 6:(dim(prev3.pooled)[2])] <- nig.table$prev 
prev3.pooled[prev3.pooled$cid == 7, "n"] <- dim(nig.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 7, "sgcentre"] <- 10
prev3.pooled[prev3.pooled$cid == 7, "Year"] <- "est"

#### 2.12 Guinea, Conakry ####

guin.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 11, ]
n.guin <- dim(guin.hrisk)[1]
guin.table <- table(guin.hrisk$age.grp, guin.hrisk$hpvsino)
guin.table <- as.data.frame.matrix(guin.table)

guin.table <- guin.table %>%
  mutate("cid" = 29) %>%
  mutate("loc" = "guin") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

guin.table
prev3.pooled[prev3.pooled$cid == 29, 6:(dim(prev3.pooled)[2])] <- guin.table$prev 
prev3.pooled[prev3.pooled$cid == 29, "n"] <- dim(guin.hrisk)[1]
prev3.pooled[prev3.pooled$cid == 29, "sgcentre"] <- 11
prev3.pooled[prev3.pooled$cid == 29, "Year"] <- "est"


#### 3. merge inc and prev tables ####
inc3.4x10 <- inc3 %>%
  transmute(sgcentre = sgcentre, loc = loc, cid = cid, years = years, 
            N25_34 = N25_29 + N30_34, 
            N35_44 = N35_39 + N40_44, 
            N45_54 = N45_49 + N50_54, 
            N55_64 = N55_59 + N60_64, # cases
            P25_34 = P25_29 + P30_34, 
            P35_44 = P35_39 + P40_44, 
            P45_54 = P45_49 + P50_54, 
            P55_64 = P55_59 + P60_64) # person years

inc3.prev3.4x10 <- merge(inc3.4x10, prev3.pooled, by = c("loc", "cid"))%>%
  mutate(sgcentre = NA, Year = Year, years = years, n = n)
