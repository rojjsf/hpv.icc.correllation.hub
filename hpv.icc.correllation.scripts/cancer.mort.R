library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)

#### cervical cancer mortality #### 

Morticd10_part1 <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/Morticd10_part1")
Morticd10_part2 <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/Morticd10_part2")
pop <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/pop")

countries <- c(1010, # Algeria
               1190, # Guinea
               1220, # Kenya
               1340, # Nigeria
               1370, # Rwanda
               1430, # South Africa
               1530, # Uganda
               2020, # Argentina
               2090, # Canada
               2120, # Chile
               2130, # Colombia
               2140, # Costa Rica
               2310, # Mexico
               2460, # Uruguay
               3027, # Bhutan
               3068, # China
               3100, # India
               3130, # Iran
               3260, # Mongolia
               3280, # Nepal
               3290, # Pakistan
               3325, # Korea
               3380, # Thailand
               3408, # Viet Nam
               4055, # Estonia
               4084, # Georgia
               4180, # Italy
               4188, # Lithuania
               4210, # netherlands
               4230, # Poland
               4280, # Spain
               4308, # United Kingdom
               5020, # Australia
               5070, # Fiji
               5207 # Vanuatu
)


c53.icd10_part1 <- Morticd10_part1 %>%
  #filter(Cause %in% c(1037, "C53")) %>% #death due to cervical cancer
  filter(Sex == 2) %>% # female deaths only
  select(-IM_Deaths1, -IM_Deaths2, -IM_Deaths3, -IM_Deaths4) %>% # infant deaths
  filter(Year %in% 1993:2018) %>% # only years for which we have the prev & inc data
  filter(Country %in% countries)
c53.icd10_part1 %>%
  group_by(Country) %>%
  summarise(n=n())%>%
  knitr::kable() # nb of years with mortality count per country
# countries available: South Africa, Korea, Thailand, Estonia, Georgia, Lithuania
  

c53.icd10_part2 <- Morticd10_part2 %>%
  filter(Cause %in% c("C53")) %>%
  filter(Sex == 2) %>%
  select(-IM_Deaths1, -IM_Deaths2, -IM_Deaths3, -IM_Deaths4) %>%
  filter(Year %in% 1993:2018) %>%
  filter(Country %in% countries) 
c53.icd10_part2 %>%
  group_by(Country) %>%
  summarise(n=n())%>%
  knitr::kable()
# countries available: South Africa, Iran, Korea, Thailand, Estonia

pop.fem <- pop %>%
  filter(Sex == 2) %>%
  select(-Lb) %>% # live births
  filter(Country %in% countries )


head(c53.icd10_part1)
which()

#### mortality all causes #### 

all.icd10_part1 <- Morticd10_part1 %>%
  filter(Cause == 1000) %>% #death due to cervical cancer
  filter(Year %in% 1993:2018) %>% # only years for which we have the prev & inc data
  filter(Country %in% countries)
all.icd10_part1 %>%
  group_by(Country) %>%
  summarise(n=n())%>%
  knitr::kable()
head(all.icd10_part1)
