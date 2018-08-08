library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr) 

### international correlation of cc incidence and hpv prevalence: countries with 2nd choice quality data ####
#("non-iarc" prevalence, ci5 incidence) 

##### incidence in table inc2 (quality level 2) ####

inc2 <- data.frame(matrix(nrow = 8))
inc2$matrix.nrow...13. <- NULL

inc2$loc <- c("*South Africa, Eastern Cape ",    # hrHPV, ALL women in primary health care centre invited (no % of participation stated). Cobas
             "Australia",                        # hrHPV, all regions, women presenting for pap smear in FREE clinics, very sesitive test (LA Roche)
             "Estonia",                          # ok: random sample from population registry, <35, line blot assay, all cyt., SELF SAMPLED
             "Uruguay",                          # except for one age grp all hrHPV types listed. random representative sample of organized screening program(40% part.), all invited women participated. 
                                                 # representative of pop as similar abn. cyt. prev. PapilloCheckR
             # "*Jamaica, Kingston and St Andrew", # age strat only as hist
             # "Japan, Fukui Prefecture",          # only hr and lr together
             # "Bahrain: Bahraini",                # age strat for hrHPV only as hist
             "Lithuania",                        # ok, urban & rural, sample from outpatient care, not randomised (not specified), HC2
             "*Canada, Ontario",                 # ok, random selection of screening specimens, HC2
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


#### exctract incidence count ####
cases <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/cases.csv")
cases <- cases %>%
  filter(REGISTRY %in% inc2$REG)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)

#### extract person years ####
pyears <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/pop.csv")
pyears <- pyears  %>%
  filter(REGISTRY %in% inc2$REG)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)


#### inc: merged cases and pyears table ####
cp <- merge(cases, pyears, by = "REGISTRY") # not by sgcentre as confusion when one centre twice (eg. spain = 3)
inc2 <- merge(cp, inc2, by = "REGISTRY") # to assure infos and values are in same/correct order
inc2

#### age groups ####
inc2.4x10 <- inc2 %>%
  transmute(loc = loc, cid = cid, 
            N25_34 = N25_29 + N30_34, 
            N35_44 = N35_39 + N40_44, 
            N45_54 = N45_49 + N50_54, 
            N55_64 = N55_59 + N60_64, # cases
            P25_34 = P25_29 + P30_34, 
            P35_44 = P35_39 + P40_44, 
            P45_54 = P45_49 + P50_54, 
            P55_64 = P55_59 + P60_64) # person years



#### prevalence: non-iarc data, see excel table for sources and comments ####

prev2 <- data.frame(matrix(nrow = 8))
prev2$matrix.nrow...13. <- NULL
prev2$cid <- inc2$cid
prev2$loc <- inc2$loc

prev.pap <-read.csv("C:/Users/schultefrohlinder/Documents/internat.corr.hpv/prev2.papers.csv")
prev.pap[, c(2:3, 14:19)] <- NULL # delete age groups not included
prev.pap[prev.pap == 999] <- NA
prev2 <- merge(prev2, prev.pap, by = "cid")


prev2.4x10 <- prev2 %>%
  transmute(loc = loc, cid = cid, 
            P1 = (X.25.30. + X.30.35.)/2, 
            P2 = (X.35.40. + X.40.45.)/2, 
            P3 = (X.45.50. + X.50.55.)/2,
            P4 = (X.55.60. + X.60.65.)/2) # same column names as prevalence
  

inc2.prev2.4x10 <- merge(inc2.4x10, prev2.4x10, by = c("loc", "cid"))%>%
  mutate(sgcentre = NA, Year = Year, years = years, n = n)

all.4x10 <- rbind(inc.prev.4x10, inc2.prev2.4x10)