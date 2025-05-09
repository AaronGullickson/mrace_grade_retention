#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# This script will read in raw data from the input directory, clean it up to
# produce the analytical dataset, and then write the analytical data to the
# output directory.


# Load libraries and functions --------------------------------------------

#source in any useful functions
library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
options(max.print=999999)

# Read the raw data -------------------------------------------------------

acs <- read_fwf(here("analysis","input","usa_00114.dat.gz"), 
                col_positions = 
                  fwf_positions(
                    start = c(1,42,55,57,58,71,85,95,97,101,105,106,111,115,118,
                              126,134,136,143,144,145,146,147,151,163,166,175,
                              176,179,182,191,194,203,206,215,216,219,222,231,
                              232,235,236),
                    end   = c(4,54,56,57,69,71,94,96,98,104,105,108,113,117,120,
                              126,135,142,143,144,145,146,150,154,165,168,175,
                              176,181,184,193,196,205,208,215,216,221,224,231,
                              232,235,236),
                    col_names = c("year","cluster","statefip","metro","strata",
                                  "ownershp","perwt","momrule","poprule",
                                  "related","sex","age","raced","hispand",
                                  "bpl","speakeng","gradeattd","ftotinc",
                                  "qage","qhispan","qrace","qgradeat",
                                  "related_mom","related_pop","age_mom",
                                  "age_pop","marst_mom","marst_pop","raced_mom",
                                  "raced_pop","hispand_mom","hispand_pop",
                                  "bpl_mom","bpl_pop","speakeng_mom",
                                  "speakeng_pop","educd_mom","educd_pop",
                                  "qhispan_mom","qhispan_pop","qrace_mom",
                                  "qrace_pop")),
                col_types = cols(.default = "i",
                                 cluster = col_double()),
                progress = FALSE)

# Recode variables ------------------------------------------------------------

acs <- acs %>% 
  #remove cases without both parents and pre-K
  #as well as cases where either parents race is imputed
  #as well as cases with imputed grade attended or age for respondent
  filter(momrule!=0 & poprule!=0 & gradeattd>10 & 
           qrace_mom==0 & qrace_pop==0 & qhispan_mom==0 & qhispan_pop==0 &
           qgradeat==0 & qage==0) %>%
  #recode variables
  mutate(race_mother=code_race(raced_mom, hispand_mom),
         race_father=code_race(raced_pop, hispand_pop),
         age_birth_mother=age_mom-age,
         age_birth_father=age_pop-age,
         foreign_born_mother=bpl_mom>=100,
         foreign_born_father=bpl_pop>=100,
         degree_mother=code_degree(educd_mom),
         degree_father=code_degree(educd_pop),
         spk_eng_mother=code_speakeng(speakeng_mom),
         spk_eng_father=code_speakeng(speakeng_pop),
         parents_married=marst_mom==1 & marst_pop==1,
         race=ifelse(race_mother==race_father, race_mother, 
                     pmap_chr(list(race_mother, race_father),
                              ~paste(sort(c(...)), collapse = "/"))),
         current_grade=factor(case_when(
           gradeattd == 20 ~ "K",
           gradeattd == 31 ~ "1st",
           gradeattd == 32 ~ "2nd",
           gradeattd == 33 ~ "3rd",
           gradeattd == 34 ~ "4th",
           gradeattd == 41 ~ "5th",
           gradeattd == 42 ~ "6th",
           gradeattd == 43 ~ "7th",
           gradeattd == 44 ~ "8th",
           gradeattd == 51 ~ "9th",
           gradeattd == 52 ~ "10th",
           gradeattd == 53 ~ "11th",
           gradeattd == 54 ~ "12th"),
           levels=c("K","1st","2nd","3rd",paste(4:12,"th",sep="")),
           ordered=TRUE),
         below_exp_grade=age>=as.numeric(current_grade)+6,
         family_income=ifelse(ftotinc==9999999, NA, 
                              ifelse(ftotinc<0, 0, ftotinc)),
         #now adjust for inflation
         family_income=case_when(
           year==2010 ~ family_income*1.18,
           year==2011 ~ family_income*1.14,
           year==2012 ~ family_income*1.12,
           year==2013 ~ family_income*1.10,
           year==2014 ~ family_income*1.08,
           year==2015 ~ family_income*1.08,
           year==2016 ~ family_income*1.07,
           year==2017 ~ family_income*1.05,
           year==2018 ~ family_income*1.02,
           year==2019 ~ family_income*1),
         own_home=ifelse(ownershp==0, NA, ownershp==1),
         foreign_born=bpl>=100,
         spk_eng=code_speakeng(speakeng),
         sex=factor(sex, levels=1:2, labels=c("Male","Female")),
         metro=factor(metro, levels=0:4, 
                      labels=c("Indeterminable","Non-metro","Central city",
                               "Metro non-central","Metro indeterminable")),
         state=code_state_names(statefip)) %>%
  #remove respondents with multiracial parents
  filter(!str_detect(race, "Multiracial"))

#factorize the race variable
acs$race <- factor(acs$race,
                   levels=c("White","Black","Indigenous","Asian","Latino",
                            "Black/White","Black/Indigenous","Black/Latino",
                            "Asian/Black",
                            "Indigenous/White","Latino/White","Asian/White",
                            "Indigenous/Latino","Asian/Indigenous",
                            "Asian/Latino"),
                   labels=c("White","Black","Indigenous","Asian","Latino",
                            "Black/White","Black/Indigenous","Black/Latino",
                            "Black/Asian",
                            "White/Indigenous","White/Latino","White/Asian",
                            "Indigenous/Latino","Indigenous/Asian",
                            "Latino/Asian"))

#code the current_grade contrast back to treatment dummies
x <- contr.treatment(length(levels(acs$current_grade)))
colnames(x) <- levels(acs$current_grade)[-1]
contrasts(acs$current_grade) <- x

# Check Yourself Before You Wreck Yourself --------------------------------

## state names
table(acs$state, acs$statefip, exclude=NULL)

## race variable
table(acs$raced_mom, acs$race_mother, exclude=NULL)
table(acs$hispand_mom, acs$race_mother, exclude=NULL)

table(acs$raced_pop, acs$race_father, exclude=NULL)
table(acs$hispand_pop, acs$race_father, exclude=NULL)

table(acs$race_mother, acs$race_father, acs$race, exclude=NULL)

## parents education
table(acs$educd_mom, acs$degree_mother, exclude=NULL)
table(acs$educd_pop, acs$degree_father, exclude=NULL)

##parents foreign born status
table(acs$bpl_mom, acs$foreign_born_mother, exclude=NULL)
table(acs$bpl_pop, acs$foreign_born_father, exclude=NULL)

#Speak english at home
table(acs$speakeng, acs$spk_eng, exclude=NULL)
table(acs$speakeng_mom, acs$spk_eng_mother, exclude=NULL)
table(acs$speakeng_pop, acs$spk_eng_father, exclude=NULL)

##parents married
table(acs$marst_mom, acs$marst_pop, acs$parents_married, exclude=NULL)

#family income
summary(acs$family_income)

#own home
table(acs$ownershp, acs$own_home, exclude=NULL)

#current grade
table(acs$gradeattd, acs$current_grade, exclude=NULL)

#grade-age progression
table(acs$age, acs$current_grade, exclude=NULL)
tapply(acs$below_exp_grade, acs[,c("age","current_grade")], mean)


# Restrict to probable biological parents ---------------------------------

#lets look at the mom and pop rules by relationship of ego to HH
table(acs$poprule, acs$related)
table(acs$momrule, acs$related)
table(acs$poprule, acs$momrule)
#as expected all bio children to the HH are identified by a direct link

#also note this
table(acs$poprule, acs$parents_married)
# I only identify non-married parents when there is a direct link. Its not 
# immediately clear to me why grandchild links could also not include unmarried
# parents, so I may want to look into that at some point

# lets look at age at birth of parents
summary(acs$age_birth_mother)
summary(acs$age_birth_father)

#clearly some outliers, including some Bill & Ted moms (and dads)!. IPUMS 
#restricts probable cases to age 15-44 for women and 15-60 for men. That seems
#pretty reasonable. What percentage are outside those ranges
mean(acs$age_birth_mother<15 | acs$age_birth_mother>44)
mean(acs$age_birth_father<15 | acs$age_birth_father>60)

#pretty small number of cases, so lets go ahead and apply this restriction
acs <- acs %>%
  filter((age_birth_mother>=15 & age_birth_mother<=44) &
           (age_birth_father>=15 & age_birth_father<=60))

# A further restriction that I want to apply is that the child's reported race
# should be consistent with the parents. It could select one or both racial
# groups but not something completely unrelated. That is a pretty complicated
# function though given the number of categories available.

mean(acs$raced==acs$raced_mom & acs$hispand==acs$hispand_mom)
mean(acs$raced==acs$raced_pop & acs$hispand==acs$hispand_pop)

mean((acs$raced==acs$raced_mom & acs$hispand==acs$hispand_mom)  | 
       (acs$raced==acs$raced_pop & acs$hispand==acs$hispand_pop))

#About 95.2% of cases are exactly consistent with either mother or father

# Based on this, I created a function that tries to identify consistent cases.
# TODO: It allows multiracial kids to be considered ok in any multiracial category. I 
# should probably refine that a bit in the future.

mean(is_race_consistent(acs$raced, acs$hispand, acs$raced_mom, acs$hispand_mom,
                        acs$raced_pop, acs$hispand_pop, acs$race))

acs <- acs %>%
  mutate(race_consistent=is_race_consistent(raced, hispand, raced_mom, 
                                            hispand_mom, raced_pop, 
                                            hispand_pop, race))

mean(acs$race_consistent)

tapply(acs$race_consistent, acs$race, mean)

#its a bit low for Latino/Asian which may I think reflect some issues for 
#Filipinos, but in general pretty good.
acs <- acs %>%
  filter(race_consistent)

# Trim to analytical data -------------------------------------------------

acs <- acs %>%
  select(cluster, strata, perwt,
         year, state, 
         momrule, poprule,
         age, current_grade, below_exp_grade, 
         race, sex, metro,
         age_birth_mother, age_birth_father, degree_mother, degree_father, 
         parents_married, family_income, own_home,
         foreign_born_mother, foreign_born_father, foreign_born,
         spk_eng_mother, spk_eng_father, spk_eng) %>%
  filter(!is.na(below_exp_grade))

save(acs, file=here("analysis","output","acs.RData"))
