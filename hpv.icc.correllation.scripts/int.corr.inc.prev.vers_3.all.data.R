library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)


##################### I. IARC PREVALENCE AND CI5-INCIDENCE#############################################################################
### use summarised cancer registry dataof cancer in 5 continents xi ###


############################ I.1 Incidence #########################################################

# all registries in one file, cases and pop seprated
# extract wanted registries from both files, merge data frames, calc rates, assign centre codes.
cases <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/cases.csv")
pyears <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/pop.csv")

# locations from registry.txt
loc <- c("Algeria, Setif", # new
         "Uganda, Kyadondo County", # new
         "Costa Rica", # new
         "Colombia, Bucaramanga", # update
         "China, Shenyang", # new
         "India, Dindigul, Ambilikkai", # update
         "Iran (Islamic Republic of), Golestan Province", # new
         "Republic of Korea, Busan", # update
         "Viet Nam, Ho Chi Minh City", # update
         "Thailand, Lampang", # update
         "Argentina, Entre Rios Province", # new
         "Thailand, Songkhla", # update
         "Spain, Tarragona", # update (in former as Barcelona)
         "Spain, Girona", # -"-
         "Chile, Region of Antofagasta", # new
         "Italy, Turin", # update
         "The Netherlands", # update
         "Poland, Poznan")  # update
# "*Viet Nam, Hanoi", # same. in inc3 (added further down. kept sepereate as inc from ci5_9)
# "Pakistan, South Karachi")  # new(old incidence). in inc3
# countries included 2008 but now no incidences found: hanoi(Vietnam), Nigeria
# countries with iarc data but no incidence data: include with estimated incidences?



# cid = centre id, assigned by Rosa in order to idntify all matching incidences and prevalences, even from non-IARC data
cid <- c(1,  2,  5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 18,  4, 20, 19, 22)
# cid <- c(1,  2,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 18,  4, 20, 19, 22) # without costa rica


REG <- c(101200199, # *Algeria, S?tif (2008-2011)
         180000299, # *Uganda, Kyadondo County (2008-2012)
         218800099, #  Costa Rica (2008-2011)
         217000299, # Colombia, Bucaramanga (2008-2012)
         415608399, # China, Shenyang (2008-2012)
         435601199, # *India, Dindigul, Ambilikkai (2008-2012)
         436400399, # Iran (Islamic Republic of), Golestan Province (2008-2011)
         441000299, # Republic of Korea, Busan (2008-2012)
         470400299, # *Viet Nam, Ho Chi Minh City (2009-2012)
         476400599, # Thailand, Lampang (2008-2012)
         203200599, # Argentina, Entre Rios Province (2008-2011)
         476400499, # Thailand, Songkhla (2008-2012)
         572400199, # Spain, Tarragona (2008-2012)
         572401099, # Spain, Girona (2008-2012)
         215200399, # *Chile, Region of Antofagasta (2008-2010)
         538000899, # Italy, Turin (2008-2012)
         552800099, # *The Netherlands (2008-2012)
         561601099) # Poland, Poznan (2008-2012)




# numbers from original prev. studies of IARC except for numbers >= 100. These are new identification numbers for merging (as cid?!?! drop one of them!)                            
sgcentre <- c(44, # Algeria
              100, # Uganda
              19, # Costa Rica
              12, # Colombia 
              23, # Shenyang
              18, # India
              61, # Iran
              15, # Korea
              2,  # HoChiMinh
              7,  # Lampang
              9,  # Argentina
              14, # Songkla
              3,  # Spain
              3,  # Spain
              16, # Chile
              83, # Italy
              4,  # Amsterdam
              41) # Poland

years <- c("2008-2011", # *Algeria, Setif 
           "2008-2012", # *Uganda, Kyadondo County 
           "2008-2011", #  Costa Rica 
           "2008-2012", # Colombia, Bucaramanga 
           "2008-2012", # China, Shenyang 
           "2008-2012", # *India, Dindigul, Ambilikkai 
           "2008-2011", # Iran (Islamic Republic of), Golestan Province 
           "2008-2012", # Republic of Korea, Busan 
           "2009-2012", # *Viet Nam, Ho Chi Minh City
           "2008-2012", # Thailand, Lampang 
           "2008-2011", # Argentina, Entre Rios Province 
           "2008-2012", # Thailand, Songkhla 
           "2008-2012", # Spain, Tarragona 
           "2008-2012", # Spain, Girona 
           "2008-2010", # *Chile, Region of Antofagasta 
           "2008-2012", # Italy, Turin 
           "2008-2012", # *The Netherlands
           "2008-2012") # Poland, Poznan

info <- data.frame(loc, "REGISTRY" = REG, sgcentre, cid, years)


# exctract incidence count ###
cases <- cases %>%
  filter(REGISTRY %in% REG)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)

# extract person years ###
pyears <- pyears  %>%
  filter(REGISTRY %in% REG)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)

# inc: merged cases and pyears table ###
inc <- merge(cases, pyears, by = "REGISTRY") # not by sgcentre as confusion when one centre twice (eg. spain = 3)
inc <- merge(inc, info, by = "REGISTRY") # to assure infos and values are in same/correct order
inc



########################## I.2 prevalence ######################################################################
pooled.data <- read_dta("C:/Users/schultefrohlinder/Documents/R/hpv.icc.correllation/HPVPREV_POOL_V29-1.dta")

### organize the data
# missing values
sel.paper0.rm <-  which(pooled.data$sel_paper0 == 0)
pooled.data <- pooled.data[-sel.paper0.rm, ]
# delete the rows in all colums of the women that have not been selected


### select high risk types ###
Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51","ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82") # omitted apvhrx for NA reason
# age groups [15 - 90) in 5 year age groups, only keeping high risk types, id of women and their age.
pooled.hrisk <-  pooled.data[, c("sgcentre", "sgid", Hrisk, "sga3")]
# eventually remove age > 64 here? (as Delphine in her paper. analysis first?!)

### select age intervals ###
pooled.hrisk$age.grp <- factor(cut(pooled.hrisk$sga3, seq(25, 65, 10), right = FALSE), labels = c("P1", "P2", "P3", "P4")) # if change, also change filter 2 rows below!
# age groups cannot be names as intervals as then they cannot be called in glm! (adapt when changing age groups!)
# right = FALSE: interval is open on the right (does not include upper endpoint) as in the cv5xi


###select ages #
pooled.hrisk <- pooled.hrisk %>%
  filter(sga3 >= 25 &  sga3 < 65) # omit women in younger/older age groups. !!! also change above & in Uganda & Costa Rica!!!!
pooled.hrisk <- pooled.hrisk %>%
  mutate(hpvpos = rowSums(pooled.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>%# factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))

### gather in one table #
m <- matrix(data = NA, nrow = length(loc), ncol = length(levels(pooled.hrisk$age.grp)) + 5)
prev.pooled <- data.frame(m)
colnames(prev.pooled) <- c("cid", "sgcentre", "loc", "Year", "n", levels(pooled.hrisk$age.grp))
prev.pooled$loc <- inc$loc
prev.pooled$cid <- inc$cid
prev.pooled$sgcentre <- inc$sgcentre





################################# I. calculation of prevalence per country#####################################

##### I. algeria ####
alg.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 44, ] # still individual data
alg.table <- table(alg.hrisk$age.grp, alg.hrisk$hpvsino) 
alg.table <- as.data.frame.matrix(alg.table)

# as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
alg.table <- alg.table %>%
  mutate("cid" = 1) %>%
  mutate("loc" = "alg") %>%
  mutate("age.grp" = levels(alg.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


prev.pooled[prev.pooled$cid == 1, 6:(dim(prev.pooled)[2])] <- alg.table$prev 
prev.pooled[prev.pooled$cid == 1, "n"] <- dim(alg.hrisk)[1]
prev.pooled[prev.pooled$cid == 1, "sgcentre"] <- 44
prev.pooled[prev.pooled$cid == 1, "Year"] <- "2007-2008"


#### I. uganda #### 
## extra script as not in pooled data. Not population wide, only  < 25y ##

uga.data <- read_dta("C:/Users/schultefrohlinder/Documents/HPV_Prevalence/Data/Uganda/original database/girls-baseline-part-quest-clin-lab-sample-hpvres-fup-cyto-updbasefupoct2007-subtypes.dta")

uga.hrisk <- uga.data[, c("HPVNUM", "AGE", "select_paper_baseline", "h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59","h68_73", "h82")]
# select_paper_baseline =1 means women included. total n = 1275 (see variable-description.doc)
head(uga.hrisk) 

uga.data$age.grp <- factor(cut(uga.data$AGE, seq(15, 25, 10), right = FALSE))
nb.age.uga <- table(uga.data$age.grp)


includedwomen <-  which(uga.hrisk$select_paper_baseline == 1)
uga.hrisk <- uga.hrisk[includedwomen, ]
dim(uga.hrisk)

uga.hrisk$age.grp <- cut(uga.hrisk$AGE, seq(25, 65, 5), right = FALSE)

str(uga.hrisk) # hpv pos/neg coded as numeric 1/0 
uga.hrisk[is.na(uga.hrisk)] <- 0 # mathmatical functions do not work with NA so transform to 0
uga.hrisk <- uga.hrisk %>%
  mutate(hpvpos = rowSums(uga.hrisk[,  c("h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59","h68_73", "h82")])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. 
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))

uga.table <- table(uga.hrisk$age.grp, uga.hrisk$hpvsino)
uga.table <- as.data.frame.matrix(uga.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
uga.table <- uga.table %>%
  mutate("cid" = 2) %>%
  mutate("age.grp" = levels(uga.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters
head(uga.hrisk)

uga.table
prev.pooled[prev.pooled$cid == 2, 6:(dim(prev.pooled)[2])] <- uga.table$prev
prev.pooled[prev.pooled$cid == 2, "n"] <- dim(uga.hrisk)[1]
prev.pooled[prev.pooled$cid == 2, "sgcentre"] <- 100
prev.pooled[prev.pooled$cid == 2, "Year"] <- "2002-2004"



#### I. costa rica ####
pooled.data <- read_dta("C:/Users/schultefrohlinder/Documents/R/hpv.icc.correllation/HPVPREV_POOL_V29-1.dta")

# pooled.data %>%
#group_by(sgcentre) %>%
#summarise(n=n())%>%
#knitr::kable()
# in new loaded data set there is costa rica included

cori.hrisk <-  pooled.data[pooled.data$sgcentre == 19, c("sgcentre", "sgid", Hrisk, "sga3", "sel_paper0")]
cori.hrisk$age.grp <- cut(cori.hrisk$sga3, seq(25, 65, 10), right = FALSE)

cori.hrisk <- cori.hrisk %>%
  filter(sga3 >= 25 &  sga3 < 65)

cori.hrisk <- cori.hrisk %>%
  mutate(hpvpos = rowSums(cori.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>%# factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))
cori.table <- table(cori.hrisk$age.grp, cori.hrisk$hpvsino)
cori.table <- as.data.frame.matrix(cori.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined#
cori.table <- cori.table %>%
  mutate("cid" = 5) %>%
  mutate("loc" = "cori") %>%
  mutate("age.grp" = levels(cori.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


cori.table
# # write.xlsx(cori.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 5, 6:(dim(prev.pooled)[2])] <- cori.table$prev
prev.pooled[prev.pooled$cid == 5, "n"] <- dim(cori.hrisk)[1]
prev.pooled[prev.pooled$cid == 5, "sgcentre"] <- 19
prev.pooled[prev.pooled$cid == 5, "Year"] <- "1993-1994"




#### I. bucamaranga, colombia (col) ####

col.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 12, ] # important to use pooled.hrisk as because of costa rica data.pooled is complete again (not used women included again)
col.table <- table(col.hrisk$age.grp, col.hrisk$hpvsino)
col.table <- as.data.frame.matrix(col.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
col.table <- col.table %>%
  mutate("cid" = 6) %>%
  mutate("loc" = "col") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters
col.table
# write.xlsx(col.table, file = "hpv.prevalence.xlsx") 
prev.pooled[prev.pooled$cid == 6, 6:(dim(prev.pooled)[2])] <- col.table$prev
prev.pooled[prev.pooled$cid == 6, "sgcentre"] <- 12
prev.pooled[prev.pooled$cid == 6, "n"] <- dim(col.hrisk)[1]
prev.pooled[prev.pooled$cid == 6, "Year"] <- "1993-1995"




#### I. shenyang, china (chin)####

chin.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 23, ]
chin.table <- table(chin.hrisk$age.grp, chin.hrisk$hpvsino)
chin.table <- as.data.frame.matrix(chin.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
chin.table <- chin.table %>%
  mutate("cid" = 8) %>%
  mutate("loc" = "chin") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

chin.table
# write.xlsx(chin.table, file = "hpv.prevalence.xlsx") 
prev.pooled[prev.pooled$cid == 8, 6:(dim(prev.pooled)[2])] <- chin.table$prev
prev.pooled[prev.pooled$cid == 8, "n"] <- dim(chin.hrisk)[1]
prev.pooled[prev.pooled$cid == 8, "sgcentre"] <- 23
prev.pooled[prev.pooled$cid == 8, "Year"] <- "2005"



#### I. india ####

ind.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 18, ]
ind.table <- table(ind.hrisk$age.grp, ind.hrisk$hpvsino)
ind.table <- as.data.frame.matrix(ind.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
ind.table <- ind.table %>%
  mutate("cid" = 9) %>%
  mutate("loc" = "ind") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

ind.table
# write.xlsx(ind.table, file = "hpv.prevalence.xlsx") 
prev.pooled[prev.pooled$cid == 9, 6:(dim(prev.pooled)[2])] <- ind.table$prev
prev.pooled[prev.pooled$cid == 9, "n"] <- dim(ind.hrisk)[1]
prev.pooled[prev.pooled$cid == 9, "sgcentre"] <- 18
prev.pooled[prev.pooled$cid == 9, "Year"] <- "2004"



#### I. iran ####
iran.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 61, ]
iran.table <- table(iran.hrisk$age.grp, iran.hrisk$hpvsino)
iran.table <- as.data.frame.matrix(iran.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
iran.table <- iran.table %>%
  mutate("cid" = 10) %>%
  mutate("loc" = "iran") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

iran.table

# write.xlsx(iran.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 10, 6:(dim(prev.pooled)[2])] <- iran.table$prev
prev.pooled[prev.pooled$cid == 10, "n"] <- dim(iran.hrisk)[1]
prev.pooled[prev.pooled$cid == 10, "sgcentre"] <- 61
prev.pooled[prev.pooled$cid == 10, "Year"] <- "2013-2014"

#### I. south korea ####
soko.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 15, ]
n.soko <- dim(soko.hrisk)[1]
soko.table <- table(soko.hrisk$age.grp, soko.hrisk$hpvsino)
soko.table <- as.data.frame.matrix(soko.table)

soko.table <- soko.table %>%
  mutate("cid" = 12) %>%
  mutate("loc" = "soko") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


soko.table
prev.pooled[prev.pooled$cid == 12, 6:(dim(prev.pooled)[2])] <- soko.table$prev
prev.pooled[prev.pooled$cid == 12, "n"] <- dim(soko.hrisk)[1]
prev.pooled[prev.pooled$cid == 12, "sgcentre"] <- 15
prev.pooled[prev.pooled$cid == 12, "Year"] <- "1999-2000"


#### I. ho chi minh, vietnam (viet1)####

viet1.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 2, ]
n.viet1 <- dim(viet1.hrisk)[1]
viet1.table <- table(viet1.hrisk$age.grp, viet1.hrisk$hpvsino)
viet1.table <- as.data.frame.matrix(viet1.table)

viet1.table <- viet1.table %>%
  mutate("cid" = 13) %>%
  mutate("loc" = "viet1") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


viet1.table
# write.xlsx(viet1.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 13, 6:(dim(prev.pooled)[2])] <- viet1.table$prev
prev.pooled[prev.pooled$cid == 13, "n"] <- dim(viet1.hrisk)[1]
prev.pooled[prev.pooled$cid == 13, "sgcentre"] <- 2
prev.pooled[prev.pooled$cid == 13, "Year"] <- "1997"


#### I. lampang, thailand (thai1) ####

thai1.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 7, ]
n.thai1 <- dim(thai1.hrisk)[1]
thai1.table <- table(thai1.hrisk$age.grp, thai1.hrisk$hpvsino)
thai1.table <- as.data.frame.matrix(thai1.table)

thai1.table <- thai1.table %>%
  mutate("cid" = 15) %>%
  mutate("loc" = "thai1") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


thai1.table
# write.xlsx(thai1.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 15, 6:(dim(prev.pooled)[2])] <- thai1.table$prev
prev.pooled[prev.pooled$cid == 15, "n"] <- dim(thai1.hrisk)[1]
prev.pooled[prev.pooled$cid == 15, "sgcentre"] <- 7
prev.pooled[prev.pooled$cid == 15, "Year"] <- "1997-1998"

#### I. entre dos rios province, argentina(arg) ####

arg.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 9, ]
n.arg <- dim(arg.hrisk)[1]
arg.table <- table(arg.hrisk$age.grp, arg.hrisk$hpvsino)
arg.table <- as.data.frame.matrix(arg.table)

arg.table <- arg.table %>%
  mutate("cid" = 3) %>%
  mutate("loc" = "arg") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


arg.table
# write.xlsx(arg.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 3, 6:(dim(prev.pooled)[2])] <- arg.table$prev
prev.pooled[prev.pooled$cid == 3, "n"] <- dim(arg.hrisk)[1]
prev.pooled[prev.pooled$cid == 3, "sgcentre"] <- 9
prev.pooled[prev.pooled$cid == 3, "Year"] <- "1998"

#### I. songkla, thailand (thai2) ####

thai2.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 14, ]
n.thai2 <- dim(thai2.hrisk)[1]
thai2.table <- table(thai2.hrisk$age.grp, thai2.hrisk$hpvsino)
thai2.table <- as.data.frame.matrix(thai2.table)

thai2.table <- thai2.table %>%
  mutate("cid" = 16) %>%
  mutate("loc" = "thai2") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


thai2.table
# write.xlsx(thai2.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 16, 6:(dim(prev.pooled)[2])] <- thai2.table$prev
prev.pooled[prev.pooled$cid == 16, "n"] <- dim(thai2.hrisk)[1]
prev.pooled[prev.pooled$cid == 16, "sgcentre"] <- 14
prev.pooled[prev.pooled$cid == 16, "Year"] <- "1997-1999"

#### I. spain ####
####two incidences, however only one prevalence. therefore prev(spain1) = prev(spain2) = prev(spain)

spain.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 3, ]
n.spain <- dim(spain.hrisk)[1]
spain.table <- table(spain.hrisk$age.grp, spain.hrisk$hpvsino)
spain.table <- as.data.frame.matrix(spain.table)

spain.table <- spain.table %>%
  mutate("cid" = 17) %>%
  mutate("loc" = "spain") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


spain.table
# write.xlsx(spain.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 17, 6:(dim(prev.pooled)[2])] <- spain.table$prev
prev.pooled[prev.pooled$cid == 18, 6:(dim(prev.pooled)[2])] <- spain.table$prev
prev.pooled[prev.pooled$cid == 17, "n"] <- dim(spain.hrisk)[1]
prev.pooled[prev.pooled$cid == 18, "n"] <- dim(spain.hrisk)[1]
prev.pooled[prev.pooled$cid == 17, "sgcentre"] <- 3
prev.pooled[prev.pooled$cid == 18, "sgcentre"] <- 3
prev.pooled[prev.pooled$cid == 17, "Year"] <- "1998"
prev.pooled[prev.pooled$cid == 18, "Year"] <- "1998"


#### I. santiago, antofagasta, chile ####

chile.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 16, ]
n.chile <- dim(chile.hrisk)[1]
chile.table <- table(chile.hrisk$age.grp, chile.hrisk$hpvsino)
chile.table <- as.data.frame.matrix(chile.table)

chile.table <- chile.table %>%
  mutate("cid" = 4) %>%
  mutate("loc" = "chile") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


chile.table
# write.xlsx(chile.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 4, 6:(dim(prev.pooled)[2])] <- chile.table$prev
prev.pooled[prev.pooled$cid == 4, "n"] <- dim(chile.hrisk)[1]
prev.pooled[prev.pooled$cid == 4, "sgcentre"] <- 16
prev.pooled[prev.pooled$cid == 4, "Year"] <- "2001" #2001 orig. study, follow-up 2006 wth age.grps >70



#### I. torino, italy (ital) ####

ital.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 83, ]
n.ital <- dim(ital.hrisk)[1]
ital.table <- table(ital.hrisk$age.grp, ital.hrisk$hpvsino)
ital.table <- as.data.frame.matrix(ital.table)

ital.table <- ital.table %>%
  mutate("cid" = 20) %>%
  mutate("loc" = "ital") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


ital.table
# write.xlsx(ital.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 20, 6:(dim(prev.pooled)[2])] <- ital.table$prev
prev.pooled[prev.pooled$cid == 20, "n"] <- dim(ital.hrisk)[1]
prev.pooled[prev.pooled$cid == 20, "sgcentre"] <- 83
prev.pooled[prev.pooled$cid == 20, "Year"] <- "2002"

#### I. Amsterdam, the Netherlands (neth) ####

neth.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 4, ]
n.neth <- dim(neth.hrisk)[1]
neth.table <- table(neth.hrisk$age.grp, neth.hrisk$hpvsino)
neth.table <- as.data.frame.matrix(neth.table)

neth.table <- neth.table %>%
  mutate("cid" = 19) %>%
  mutate("loc" = "neth") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


neth.table
# write.xlsx(neth.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 19, 6:(dim(prev.pooled)[2])] <- neth.table$prev
prev.pooled[prev.pooled$cid == 19, "n"] <- dim(neth.hrisk)[1]
prev.pooled[prev.pooled$cid == 19, "sgcentre"] <- 4
prev.pooled[prev.pooled$cid == 19, "Year"] <- "1995-1998"



#### I. Warsawa, Poland (pol) ####

pol.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 41, ]
n.pol <- dim(pol.hrisk)[1]
pol.table <- table(pol.hrisk$age.grp, pol.hrisk$hpvsino)
pol.table <- as.data.frame.matrix(pol.table)

pol.table <- pol.table %>%
  mutate("cid" = 22) %>%
  mutate("loc" = "pol") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

pol.table
# write.xlsx(pol.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 22, 6:(dim(prev.pooled)[2])] <- pol.table$prev
prev.pooled[prev.pooled$cid == 22, "n"] <- dim(pol.hrisk)[1]
prev.pooled[prev.pooled$cid == 22, "sgcentre"] <- 41
prev.pooled[prev.pooled$cid == 22, "Year"] <- "2006"






############################################## I. pooled prevalence table ################################################

# remove NaN
prev.pooled[prev.pooled == "NaN"] <- NA
# remove 0
prev.pooled[prev.pooled == 0] <- NA
prev.pooled


###
###
############################### II. CI5-INC & NON-IARC-PREV ############################################
###
###

#### II.1 incidence####
inc2 <- data.frame(matrix(nrow = 8))
inc2$matrix.nrow...8. <- NULL


inc2$loc <- c("South Africa, Eastern Cape ",    # hrHPV, ALL women in primary health care centre invited (no % of participation stated). Cobas
              "Australia",                        # hrHPV, all regions, women presenting for pap smear in FREE clinics, very sesitive test (LA Roche)
              "Estonia",                          # ok: random sample from population registry, <35, line blot assay, all cyt., SELF SAMPLED
              "Uruguay",                          # except for one age grp all hrHPV types listed. random representative sample of organized screening program(40% part.), all invited women participated. 
              # representative of pop as similar abn. cyt. prev. PapilloCheckR
              # "*Jamaica, Kingston and St Andrew", # age strat only as hist
              # "Japan, Fukui Prefecture",          # only hr and lr together
              # "Bahrain: Bahraini",                # age strat for hrHPV only as hist
              "Lithuania",                        # ok, urban & rural, sample from outpatient care, not randomised (not specified), HC2
              "Canada, Ontario",                 # ok, random selection of screening specimens, HC2
              # "*France, Somme ",                  # reims. screening HC2, AIDS and recent cyt abn. excluded
              # "*Canada, Newfoundland and Labrador", # hr hpv - 2 types. routine pap smear
              "United Kingdom" ,                 # ok, screening specimens, LA Roche
              # "Czech Republic",                   # screening pop, only normal cyt
              "Kenya, Mombasa")                  # from Primary care facilities, random age strat. recruitment. LA.
inc2$cid <- c(27,	                               #*South Africa, Eastern Cape (2008-2012)
              49,	                               # Australia (2008-2012)
              63,	                               #  Estonia (2008-2012)
              40,                                # Uruguay (2008-2012)
              #43,                                #*Jamaica, Kingston and St Andrew (2008-2011)
              #50,                                #        Japan, Fukui Prefecture (2008-2012)
              #51,	                               #             Bahrain: Bahraini (2008-2012)
              61,	                               #                    Lithuania (2008-2012)
              67,	                               #            *Canada, Ontario (2008-2012)
              #39,	                               #             *France, Somme (2008-2012)
              #48,	                               #  *Canada, Newfoundland and Labrador (2008-2012) 
              55,	                               #                     United Kingdom (2008-2012)
              #64)	                               #Czech Republic (2008-2012)
              45)


inc2$REGISTRY <- c(171000199,	                          #*South Africa, Eastern Cape (2008-2012)
                   603600099,	                              # Australia (2008-2012)
                   523300099,	                              #  Estonia (2008-2012)
                   285800099,                                # Uruguay (2008-2012)
                   #238800199,                                #*Jamaica, Kingston and St Andrew (2008-2011)
                   #439200999,                                # Japan, Fukui Prefecture (2008-2012)
                   #404800046,	                              # Bahrain: Bahraini (2008-2012)
                   544000099,	                              #  Lithuania (2008-2012)
                   312401099,	                              #  *Canada, Ontario (2008-2012)
                   #525000699,	                              #  *France, Somme (2008-2012)
                   #312400899,	                              #  *Canada, Newfoundland and Labrador (2008-2012) 
                   582600099,	                              #  United Kingdom (2008-2012)
                   #520300099)	                              #  Czech Republic (2008-2012)
                   140400299)                                # *Kenya, Nairobi (2008-2012) 

inc2$years <- c("2008-2012",	                          #*South Africa, Eastern Cape (2008-2012)
                        "2008-2012",	                              # Australia (2008-2012)
                        "2008-2012",	                              #  Estonia (2008-2012)
                        "2008-2012",                                # Uruguay (2008-2012)
                        #"2008-2011",                                #*Jamaica, Kingston and St Andrew (2008-2011)
                        #"2008-2012",                                # Japan, Fukui Prefecture (2008-2012)
                        #"2008-2012",	                              # Bahrain: Bahraini (2008-2012)
                        "2008-2012",	                              #  Lithuania (2008-2012)
                        "2008-2012",	                              #  *Canada, Ontario (2008-2012)
                        #"2008-2012",	                              #  *France, Somme (2008-2012)
                        #"2008-2012",	                              #  *Canada, Newfoundland and Labrador (2008-2012) 
                        "2008-2012",	                              #  United Kingdom (2008-2012)
                        #"2008-2012",	                              #  Czech Republic (2008-2012)
                        "2008-2012")


# exctract incidence count #
cases <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/cases.csv")
cases <- cases %>%
  filter(REGISTRY %in% inc2$REG)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)

# extract person years #
pyears <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/pop.csv")
pyears <- pyears  %>%
  filter(REGISTRY %in% inc2$REG)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)


# inc: merged cases and pyears table #
cp <- merge(cases, pyears, by = "REGISTRY") # not by sgcentre as confusion when one centre twice (eg. spain = 3)
inc2 <- merge(cp, inc2, by = "REGISTRY") %>% # to assure infos and values are in same/correct order
  mutate(sgcentre = NA)
inc2$matrix.nrow...8. <- NULL
  

  inc2

# age groups #
inc2.4x10 <- inc2 %>%
  transmute(loc = loc, cid = cid, 
            N25_34 = N25_29 + N30_34, 
            N35_44 = N35_39 + N40_44, 
            N45_54 = N45_49 + N50_54, 
            N55_64 = N55_59 + N60_64, # cases
            P25_34 = P25_29 + P30_34, 
            P35_44 = P35_39 + P40_44, 
            P45_54 = P45_49 + P50_54, 
            P55_64 = P55_59 + P60_64,# person years
            sgcentre = NA,
            years = years)



#### II.2 prevalence: non-iarc data, see excel table for sources and comments ####

prev2 <- data.frame(matrix(nrow = 8))
prev2$matrix.nrow...8. <- NULL
prev2$cid <- inc2$cid
prev2$loc <- inc2$loc
prev2$Year <- c("2009-2011",	                          #*South Africa, Eastern Cape (2008-2012)
                "2005-2008",	                              # Australia (2008-2012)
                "2005-2006",	                              #  Estonia (2008-2012)
                "2011",                                # Uruguay (2008-2012)
                #999,                                #*Jamaica, Kingston and St Andrew (2008-2011)
                #999,                                # Japan, Fukui Prefecture (2008-2012)
                #999,	                              # Bahrain: Bahraini (2008-2012)
                "1998-2001",	                              #  Lithuania (2008-2012)
                "1998-1999",	                              #  *Canada, Ontario (2008-2012)
                #"1997-2001",	                              #  *France, Somme (2008-2012)
                #999,	                              #  *Canada, Newfoundland and Labrador (2008-2012) 
                "2000",	                              #  United Kingdom (2008-2012)
                #999,	                              #  Czech Republic (2008-2012)
                "1998-2000")
prev2$n <- c(1078,	                          #*South Africa, Eastern Cape (2008-2012)
             2620,	                              # Australia (2008-2012)
             326,	                              #  Estonia (2008-2012)
             965,                                # Uruguay (2008-2012)
             #238800199,                                #*Jamaica, Kingston and St Andrew (2008-2011)
             #439200999,                                # Japan, Fukui Prefecture (2008-2012)
             #404800046,	                              # Bahrain: Bahraini (2008-2012)
             1020,	                              #  Lithuania (2008-2012)
             955,	                              #  *Canada, Ontario (2008-2012)
             #7932,	                              #  *France, Somme (2008-2012)
             #312400899,	                              #  *Canada, Newfoundland and Labrador (2008-2012) 
             3444,	                              #  United Kingdom, Edinborough (2008-2012)
             #520300099)	                              #  Czech Republic (2008-2012)
             429)

prev.pap <-read.csv("C:/Users/schultefrohlinder/Documents/internat.corr.hpv/prev2.papers.csv")
prev.pap[, c(2:3, 14:19)] <- NULL # delete age groups not included
prev.pap[prev.pap == 999] <- NA
prev2 <- merge(prev2, prev.pap, by = "cid")


prev2.4x10 <- prev2 %>%
  transmute(loc = loc, cid = cid, 
            P1 = (X.25.30. + X.30.35.)/2, 
            P2 = (X.35.40. + X.40.45.)/2, 
            P3 = (X.45.50. + X.50.55.)/2,
            P4 = (X.55.60. + X.60.65.)/2, # same column names as prevalence. 
            # For Kenya, Mombasa I copied the prevalence of the 10y age group so thant this calculation works!
            sgcentre = NA,
            Year = Year,
            n = n)


#### II. inc2.prev2.4x10 ####
inc2.prev2.4x10 <- merge(inc2.4x10, prev2.4x10, by = c("loc", "cid", "sgcentre"))



###
###
############################### III. IARC-PREVALENCE & NON-CI5XI INCIDENCE#############################################
#### a table for the countries with good prevalence data but incidence estimates ###
###



#### III. non-ci5XI-incidence ####

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

#### III.1.1 vietnam, hanoi #### 
ci58 <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-VIII/CI5-VIII.csv")
viet2 <- ci58 %>%
  filter(X1 == 125)%>%
  filter(X1.1 == 2) %>% # females
  filter(X1.2 == 120) # cervical cancer

viet2[, c("X1", "X1.1", "X1.2")] <- NULL
inc3[inc3$cid == 14, 2:14] <- viet2[4:16, "X67"] # cases
inc3[inc3$cid == 14, 15:27] <- viet2[4:16, "X515691"] # pyears


#### III.1.2 pakistan, karachi ####

pak.data <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-IXd/45860199.csv", header = TRUE)
head(pak.data)
pak <- pak.data %>%
  filter(X1 == 2) %>% # females
  filter(X001 == 117) # cervical cancer
pak[, c("X1", "X001")] <- NULL
inc3[inc3$cid == 25, 2:14] <- pak[3:15, "X58"] # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 25, 15:27] <- pak[3:15, "X652475"] # pyears



####III.1.3 bhutan ####

bhut.inc <- c(0,	0,	2.9,	10.5,	17.5,	26.3,	38.7,	44.7,	44.3,	36.7,	31.4,	19.8,	15.1)
# downloaded from ICO on 17/07/2018
# Incidence rates (2007-2008, Tashi Dendup, personal communication) were scaled using site and s
# sex-specific percentages of microscopically verified cases obtained from the mean of four Indian cancer registries (Dibrugarh, Kamrup, Mizoram and Sikkim)
# and applied to 2012 population., D2
inc3[inc3$cid == 11, 2:14] <- bhut.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 11, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.4 mongolia ####
mong.inc <- c(0,	1.3,	8.5,	17.8,	32.1,	47.5,	62.3,	73.9,	76, 71.9,	65.1,	55.3,	53.6)
# downloaded from ICO on 18/07/2018
# most recent national incidence rates projected to 2012 population, D5
inc3[inc3$cid == 31, 2:14] <- mong.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 31, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.5 Nepal ####

nepal.inc <- c(0,	0.5,	4.3,	8.5,	22.9,	36.6,	53.3,	74.6,	63.9,	68.5,	45.7,	32.9,	17.3)
# downloaded from ICO on 18/07/2018
# estimates using Indian data, G6
inc3[inc3$cid == 34, 2:14] <- nepal.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 34, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.6. Rwanda, Kigali ####
rwa.inc <- c(0.7,	0.4,	8.5,	22.3,	42,	68.1,	104.9,	137.5,	156,	151.5,	137.2,	113.9,	79.6)
# downloaded from ICO on 18/07/2018
# estimate from Tanzanian and Ugandian data (1990), F6
inc3[inc3$cid == 32, 2:14] <- rwa.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 32, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.6. Mexico, Morelos State ####
mex.inc <- c(0.3,	6.5,	16.5,	26.8,	38.5,	46.7,	51.1,	53.2,	54.2,	55.5,	57.5,	59.9,	62.9)
# downloaded from ICO on 18/07/2018
# estimate from estimated mortality data using modelled survival,  E6
inc3[inc3$cid == 26, 2:14] <- mex.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 26, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.7. Georgia, tbilisi ####
# downloaded from ICO on 25/07/2018
# Estimated from estimated national mortality (2001-2010) for 2012 and modelled survival.
georg.inc <- c(0,	0,	0,	19.5,	31.6,	38.7,	40.6,	40.1,	35.2,	27.8,	21.7,	17.1,	14.2)
inc3[inc3$cid == 30, 2:14] <- georg.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 30, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.8. vanuatu, port vila ####
# downloaded from ICO on 25/07/2018
# Incidence rates (1999-2003) applied to the 2012 population.
van.inc <- c(0,	0,	19.3,	46,	14,	46.3,	52.3,	43.8,	29.3,	80.1,	53.8,	0,	0)
inc3[inc3$cid == 33, 2:14] <- van.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 33, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)

#### III.1.9 fiji #### 
# downloaded from ICO on 25/07/2018
#Site-specific incidence rates for all ages were estimated from estimated national mortality data (2009) for 2012 
#and modelled survival and partitioned using sex- and age-specific proportions from Fidji cancer registry (1991-1999).
fiji.inc <- c(2.6,	2.7,	16.5,	23.6,	74.4,	101.6,	96.4,	100.2,	70.2,	128.7,	74.3,	84.7,	58.1)
inc3[inc3$cid == 28, 2:14] <- fiji.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 28, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)


#### III.1.10. nigeria, ibadan ####
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


#### III.1.11 guinea ####
# downloaded from ICO on 25/07/2018
# Incidence rates from Conakry (2004-2009) covering around 20% of the population were applied to 2012 population.
guin.inc <- c(0.5,	1.9,	8,	18.5,	33.8,	52.1,	68.3,	90.8,	112.8,	143.1,	168.5,	187.2,	202.4)
inc3[inc3$cid == 29, 2:14] <- guin.inc # cases from age 15 to age 15+13*5 = 80
inc3[inc3$cid == 29, 15:27] <- 100000 # pyears (as rates are estimated per 100 000 per year)





################### III.2. prevalence ##########################################################################


#own prev3.pooled table to avoid confusion
# prev3.pooled has same structure as prev.pooled
m <- matrix(data = NA, nrow = length(l), ncol = length(levels(pooled.hrisk$age.grp)) + 5)
prev3.pooled <- data.frame(m)
colnames(prev3.pooled) <- c("cid", "sgcentre", "loc", "Year", "n", levels(pooled.hrisk$age.grp))
prev3.pooled$loc <- inc3$loc
prev3.pooled$cid <- inc3$cid
prev3.pooled$sgcentre <- inc3$sgcentre


#### III.2.1 vietnam, hanoi #### 
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



#### III.2.2 pakistan, karachi ###

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



#### III.2.3 bhutan ####

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


#### III.2.4 mongolia, ulanbataar ####

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


#### III.2.5 nepal, bharatpur ####

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


#### III.2.6 Rwanda, Kigali ####

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


#### III.2.7 Mexico, Morelos State ####

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


#### III.2.8 Georiga, Tbilisi ####

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

#### III.2.9 Vanuatu, Port Vila ####

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

#### III.2.10 Fiji, Suva ####

#mex.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 8, ]
#n.mex <- dim(mex.hrisk)[1]
#mex.table <- table(mex.hrisk$age.grp, mex.hrisk$hpvsino)
#mex.table <- as.data.frame.matrix(mex.table)

prev3.pooled[prev3.pooled$cid == 7, "Year"] <- "est"

#### III.2.11 Nigeria, Ibadan ####

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

###
###
############################### IV. ALL INC & PREV ##############################################
###
###
inc <- rbind(inc, inc2, inc3)
prev.pooled <- rbind(prev.pooled, prev2.4x10, prev3.pooled)

inc.4x10 <- inc %>%
  transmute(sgcentre = sgcentre, loc = loc, cid = cid, years = years, 
            N25_34 = N25_29 + N30_34, 
            N35_44 = N35_39 + N40_44, 
            N45_54 = N45_49 + N50_54, 
            N55_64 = N55_59 + N60_64, # cases
            P25_34 = P25_29 + P30_34, 
            P35_44 = P35_39 + P40_44, 
            P45_54 = P45_49 + P50_54, 
            P55_64 = P55_59 + P60_64) # person years


#### IV. inc.prev table for glm #### 
inc.prev.4x10 <- merge(prev.pooled, inc.4x10, by = c("sgcentre", "cid", "loc"))
inc.prev.4x10

