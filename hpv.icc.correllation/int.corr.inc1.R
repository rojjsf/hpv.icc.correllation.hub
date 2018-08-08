library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)


#####################
#### use summarised cancer registry dataof cancer in 5 continents xi ####
#####################
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
# cid <- c(1,  2,  6,  8,  9, 10, 12, 13, 45, 15,  3, 16, 17, 18,  4, 20, 19, 22) # without costa rica


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


#### exctract incidence count ####
cases <- cases %>%
  filter(REGISTRY %in% REG)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)
  

  
 #### extract person years ####
pyears <- pyears  %>%
  filter(REGISTRY %in% REG)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)
 

 #### inc: merged cases and pyears table ####
inc <- merge(cases, pyears, by = "REGISTRY") # not by sgcentre as confusion when one centre twice (eg. spain = 3)
inc <- merge(inc, info, by = "REGISTRY") # to assure infos and values are in same/correct order
inc

#### include non ci5_10 incidences: ####

inc <- rbind(inc, inc3)
# inc <- rbind(inc, inc3[1:2, ])# without estimated quality 3 data

#### inc.rates:   per 100000 ####
## the following calculation only works if not non-ci5 included
dc <- dim(cases)[2]  # length of data frame cases (so script can be run even if centres added)
dp <- dim(pyears)[2] # length of data frame pyears

inc.rates <- data.frame(inc, round(inc[, 2:dc] * 100000 / inc[, (dp +1 ):(2*dp-1)], 1)) # select colums with values, calculate rates
inc.rates[, c(2:(dc), (dp+1):(2*dp-1))] <- NULL # delete cases and pyears columns
inc.rates
# this table corresponds to the rates calculated by ci5 (so it is correct!!!)
# DO NOT TAKE MEAN OF RATES OVER AGE GROUPS, add!!!

## rates for all iarc:
inc.prev.4x10merge
rates.all.iarc.4x10 <- inc.prev.4x10 %>%
  transmute(sgcentre = sgcentre,
            cid = cid,
            loc = loc,
            Year.prev = Year,
            Year.inc = years,
            Prev1 = P1,
            Prev2 = P2,
            Prev3 = P3,
            Prev4 = P4,
            R25_34 = round(N25_34*100000/P25_34, 2),
            R35_44 = round(N35_44*100000/P35_44, 2),
            R45_54 = round(N45_54*100000/P45_54, 2),
            R55_64 = round(N55_64*100000/P55_64, 2))
rates.all.iarc.4x10



 #### histograms to explore distribution of the data ####
par(mfrow = c(2, 2))
hist(inc.prev.4x10$N25_34*1000000/inc.prev.4x10$P25_34, breaks = seq(0, 300, 10))
hist(inc.prev.4x10$N35_44*100000/inc.prev.4x10$P35_44, breaks = seq(0, 300, 10))
hist(inc.prev.4x10$N45_54*100000/inc.prev.4x10$P45_54, breaks = seq(0, 300, 15))
hist(inc.prev.4x10$N55_64*100000/inc.prev.4x10$P55_64, breaks = seq(0, 300, 15))


hist(inc.prev.4x10$P1, breaks = seq(0, 40, 4))
hist(inc.prev.4x10$P2, breaks = seq(0, 40, 4))
hist(inc.prev.4x10$P3, breaks = seq(0, 40, 4))
hist(inc.prev.4x10$P4, breaks = seq(0, 40, 4))

 # skewed to the right - > square root transformation?

hist(sqrt(inc.prev.4x10$N25_34*100000/inc.prev.4x10$P25_34))
hist(sqrt(inc.prev.4x10$N35_44*100000/inc.prev.4x10$P35_44))
hist(sqrt(inc.prev.4x10$N45_54*100000/inc.prev.4x10$P45_54))
hist(sqrt(inc.prev.4x10$N45_54*100000/inc.prev.4x10$P45_54))