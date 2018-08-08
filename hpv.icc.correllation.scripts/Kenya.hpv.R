library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)

#### 1. all Nairobi data ####

str(data.nairobi)
data.nairobi <- read_sav("C:/Users/schultefrohlinder/Documents/R/hpv.icc.correllation/totalhpv.sav")
length(which(data.nairobi$AGEGROU5 >= 25 & data.nairobi$AGEGROU5< 55)) # 6 women excluded
dim(data.nairobi) # 87 more than in paper
length(which(data.nairobi$AGE >= 25 & data.nairobi$AGE < 55)) # 6 women excluded
length(which(data.nairobi$PREGNOW == 1)) # 3 excluded
length(which(data.nairobi$AGE >= 25 & data.nairobi$AGE < 55 & data.nairobi$PREGNOW == 0 & data.nairobi$HPV  == 1 )) # 439, inkl remarks 428
length(which(data.nairobi$EDATE1 < "1998-01-01" & data.nairobi$EDATE1 >= "2000-08-01")) # 0
length(which(data.nairobi$DISTR)) # ???

length(which( data.nairobi$AGEGROU5 >= 25 & data.nairobi$AGEGROU5< 55 & data.nairobi$PREGNOW == 0 
              & !data.nairobi$HPV  %in% NA 
              & !data.nairobi$SLIFE == "" 
              & !data.nairobi$SLIFE %in% "DNK"
              & !data.nairobi$SLIFE %in% "999")) # but paper does not say that no sex is excluded!

data.nairobi[which( !data.nairobi$PAPNORM == ""), "CEREMARK"] # remarks, i suppose about cytology
length(which( data.nairobi$VISNO == 2))           
#HIVCONFM 
#HPVHI
#HPV6 <dbl>, HPV11 <dbl>, HPV16 <dbl>,
#   HPV18 <dbl>, HPV31 <dbl>, HPV33 <dbl>, HPV34 <dbl>, HPV35 <dbl>, HPV39 <dbl>,
#   HPV40 <dbl>, HPV42 <dbl>, HPV43 <dbl>, HPV44 <dbl>, HPV45 <dbl>, HPV51 <dbl>,
#   HPV52 <dbl>, HPV53 <dbl>, HPV54 <dbl>, HPV56 <dbl>, HPV58 <dbl>, HPV59 <dbl>,
#   HPV66 <dbl>, HPV68 <dbl>, HPV70 <dbl>, HPV74 <dbl>, HPVX <dbl>, HPVNEG <dbl>
#EDATE1
#YRBIRTH
#AGEGROU5 shows age by which the 5y agegroup begins (ex 30 for 32y old)
#PAPNRSAT is nb of normal pap used in other excel



#### 2. only normal cyt #### 

nairobi.csv <- read.csv("C:/Users/schultefrohlinder/Documents/R/hpv.icc.correllation/Copy of HPV_age_Nairobi_H_De_Vuyst.csv", as.is = TRUE) # else conversion to factor
nairobi.csv[nairobi.csv == "#NULL!"] <- "0"
hrisk <- c("HPV16", "HPV18", "HPV31","HPV33","HPV35","HPV39","HPV45","HPV51","HPV52","HPV56","HPV58","HPV59","HPV68") # HPV73", "HPV82" not included, therefore 2 hr types missing!
nair.hrisk <- nairobi.csv %>%
  select(AGE, hrisk)
nair.hrisk$age.grp <- factor(cut(nair.hrisk$AGE, seq(25, 65, 10), right = FALSE), labels = c("P1", "P2", "P3", "P4"))
nair.hrisk[, hrisk] <- as.numeric(unlist(nair.hrisk[, hrisk])) # as class is charater

nair.hrisk <- nair.hrisk %>%
  mutate(hpvpos = rowSums(nair.hrisk[, hrisk]))%>%
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))
nair.table <- table(nair.hrisk$age.grp, nair.hrisk$hpvsino)
nair.table <- as.data.frame.matrix(nair.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined#
nair.table <- nair.table %>%
  mutate("cid" = 71) %>%
  mutate("loc" = "nair") %>%
  mutate("age.grp" = levels(nair.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


nair.table
# # write.xlsx(nair.table, file = "hpv.prevalence.xlsx")
prev.pooled[prev.pooled$cid == 71, 6:(dim(prev.pooled)[2])] <- nair.table$prev
prev.pooled[prev.pooled$cid == 71, "n"] <- dim(nair.hrisk)[1] # only normal cytology women!
prev.pooled[prev.pooled$cid == 71, "sgcentre"] <- 101 # not part of the HPV prevalence study, therefore (as Uganda) sgcentre >100 assigned by Rosa
prev.pooled[prev.pooled$cid == 71, "Year"] <- "1998-2000"
