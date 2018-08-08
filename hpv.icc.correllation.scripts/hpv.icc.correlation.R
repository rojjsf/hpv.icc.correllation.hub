
#### age-stratified correlation of hpv in several countries ####
#### read in data ####

library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
alg.data <- read_dta("C:/Users/schultefrohlinder/Documents/HPV_Prevalence/Data/Algeria/pooled_db/algeriapool.dta")
str(alg.data)

# organize the data
# missing values
sel.paper0.rm <-  which(alg.data$sel_paper0 == 0)
# sel_paper0 = 0 means that the woman has not been selected.
exclu.reason <- alg.data[sel.paper0.rm, "exclur0"]
exclu.reason 
# to check why these women have been removed
alg.data <- alg.data[-sel.paper0.rm, ]
# delete the rows in all colums of the women that have not been selected
nrow (alg.data)

# select high risk types: 
Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51","ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82", "ahpvhrx")
# age groups [15 - 90) in 5 year age groups, only keeping high risk types, id of women and their age.
alg.hrisk <-  alg.data[, c("sgid", Hrisk, "sga3")]
alg.hrisk$age.grp <- cut(alg.hrisk$sga3, seq(10, 90, 5))

str(alg.hrisk) # hpv pos/neg coded as numeric 1/0 
alg.hrisk <- alg.hrisk %>%
  mutate(hpvpos = rowSums(alg.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(alg.hrisk$hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(alg.hrisk$hpvsino, levels = c(0,1), labels = c("neg", "pos")))

alg.table <- table(alg.hrisk$age.grp, alg.hrisk$hpvsino)
alg.table <- as.data.frame.matrix(alg.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
alg.table <- alg.table %>%
  mutate("age.grp" = levels(alg.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters
  

alg.table

write.xlsx(alg.table, file = "hpv.prevalence.xlsx", sheetName="algeria")
