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

code_race <- function(race, hisp) {
  race <- case_when(
    is.na(race) | is.na(hisp) ~ NA_character_,
    hisp != 0 ~ "Latino",
    race == 100 ~ "White",
    race == 200 ~ "Black",
    (race >= 300 & race < 400) | (race>=680 & race<700)  ~ "Indigenous",
    race >= 400 & race < 680 ~ "Asian",
    race > 700 ~ "Multiracial",
    TRUE ~ NA_character_
  )
  return(race)
}

code_degree <- function(educd) {
  degree <- case_when(
    is.na(educd) | educd==1 ~  NA_character_,
    educd<62 ~ "Less than HS",
    educd<81 ~ "HS diploma",
    educd<101 ~ "AA degree",
    educd==101 ~ "BA degree",
    TRUE ~ "Grad degree"
  )
  degree <- factor(degree,
                 levels=c("Less than HS","HS diploma","AA degree","BA degree",
                          "Grad degree"),
                 ordered=TRUE)
  degree <- ordered_factor(degree)
  return(degree)
}

#change contrasts for ordered factors to stairstep style
ordered_factor <- function(fact_var) {
  ord_fact <- factor(fact_var, ordered=TRUE)
  categories <- levels(fact_var)
  n_cat <- length(categories)
  cont <- matrix(0, n_cat, n_cat-1)
  cont[col(cont)<row(cont)] <- 1
  rownames(cont) <- categories
  colnames(cont) <- paste(categories[2:n_cat], categories[1:(n_cat-1)],
                          sep=" vs. ")
  contrasts(ord_fact) <- cont
  return(ord_fact)
}

# Read the raw data -------------------------------------------------------

acs <- read_fwf(here("analysis","input","usa_00110.dat.gz"), 
                col_positions = 
                  fwf_positions(
                    start = c(1,11,32,42,55,56,61,74,77,88,98, 99,104,108,111,
                              126,128,135,138,147,148,151,154,163,166,175,178,
                              187,190),
                    end   = c(4,18,41,54,55,60,72,74,83,97,98,101,106,110,113,
                              127,134,137,140,147,148,153,156,165,168,177,180,
                              189,192),
                    col_names = c("year","serial","hhwt","cluster","metro",
                                  "puma","strata","ownershp","hhincome","perwt",
                                  "sex","age","raced","hispand","bpl",
                                  "gradeattd","ftotinc","age_mom","age_pop",
                                  "marst_mom","marst_pop","raced_mom",
                                  "raced_pop","hispand_mom","hispand_pop",
                                  "bpl_mom","bpl_pop","educd_mom","educd_pop")),
                col_types = cols(.default = "i",
                                 cluster = col_double()),
                progress = FALSE)

# Recode variables ------------------------------------------------------------

#Code grade retention
#for now just use the level+5 as above grade, but this probably needs refinement
#to address lots of issues.

#I worry a little bit about specifying grade retention for the older grades
#where grade retention is only possible at ages when kids are often out of the
#household and therefore not eligible for the sample. Might create some weird
#biases.

acs <- acs %>% 
  mutate(race_mother=code_race(raced_mom, hispand_mom),
         race_father=code_race(raced_pop, hispand_pop),
         age_birth_mother=age_mom-age,
         age_birth_father=age_pop-age,
         foreign_born_mother=bpl_mom>=100,
         foreign_born_father=bpl_pop>=100,
         degree_mother=code_degree(educd_mom),
         degree_father=code_degree(educd_pop),
         parents_married=marst_mom==1 & marst_pop==1,
         race=ifelse(race_mother==race_father, race_mother, 
                     pmap_chr(list(race_mother, race_father),
                              ~paste(sort(c(...)), collapse = "/"))),
         current_grade=factor(case_when(
           gradeattd == 10 ~ "Pre-K",
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
           levels=c("Pre-K","K","1st","2nd","3rd",paste(4:12,"th",sep=""))),
         below_exp_grade=age>=as.numeric(current_grade)+5,
         family_income=ifelse(ftotinc==9999999, NA, 
                              ifelse(ftotinc<0, 0, ftotinc)),
         own_home=ifelse(ownershp==0, NA, ownershp==1),
         foreign_born=bpl>=100)

#code the contrast matrix for race to adjust for fractions
acs$race <- factor(acs$race,
                   levels=c("White","Black","Indigenous","Asian","Latino",
                            "Black/White","Black/Indigenous","Black/Latino",
                            "Asian/Black",
                            "Indigenous/White","Latino/White","Asian/White",
                            "Indigenous/Latino","Asian/Indigenous",
                            "Asian/Latino",
                            "Multiracial",
                            "Black/Multiracial","Multiracial/White",
                            "Indigenous/Multiracial","Latino/Multiracial",
                            "Asian/Multiracial"),
                   labels=c("White","Black","Indigenous","Asian","Latino",
                            "Black/White","Black/Indigenous","Black/Latino",
                            "Black/Asian",
                            "White/Indigenous","White/Latino","White/Asian",
                            "Indigenous/Latino","Indigenous/Asian",
                            "Latino/Asian",
                            "Multiracial",
                            "Black/Multiracial","White/Multiracial",
                            "Indigenous/Multiracial","Latino/Multiracial",
                            "Asian/Multiracial"))
#the default treatment contrast will do most of the work, but I need to 
#put in the 0.5 cases
contr_race <- contrasts(acs$race)
#lets loop through rows for multiracials and decide where the 0.5 should go
for(i in str_which(rownames(contr_race),"/")) {
  race_name <- rownames(contr_race)[i]
  comp_races <- str_split(race_name, "/")[[1]]
  fraction <- 1/length(comp_races)
  #one category is missing because it is the reference so remove if so
  comp_races <- comp_races[comp_races %in% colnames(contr_race)]
  contr_race[race_name, comp_races] <- fraction
}
contrasts(acs$race) <- contr_race


# Check Yourself Before You Wreck Yourself --------------------------------

## race variable
table(acs$raced_mom, acs$race_mother, exclude=NULL)
table(acs$hispand_mom, acs$race_mother, exclude=NULL)

table(acs$raced_pop, acs$race_father, exclude=NULL)
table(acs$hispand_pop, acs$race_father, exclude=NULL)

table(acs$race_mother, acs$race_father, acs$race, exclude=NULL)

## parents education
table(acs$educd_mom, acs$degree_mother, exclude=NULL)
table(acs$educd_pop, acs$degree_father, exclude=NULL)

##parents age
#TODO: some fishy stuff here
summary(acs$age_birth_mother)
summary(acs$age_birth_father)

##parents foreign born status
table(acs$bpl_mom, acs$foreign_born_mother, exclude=NULL)
table(acs$bpl_pop, acs$foreign_born_father, exclude=NULL)

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

#how does missingness of both parents change by grade and age
acs$both_missing <- is.na(acs$race_father) & is.na(acs$race_mother)
round(tapply(acs$both_missing, acs[,c("age","current_grade")], mean, 
             na.rm=TRUE), 3)

mean(acs$both_missing)

mean(!is.na(acs$race_mother) & !is.na(acs$race_father))

# Trim to analytical data -------------------------------------------------

acs <- acs %>%
  select(year, age, current_grade, below_exp_grade, race, age_birth_mother, 
         age_birth_father, degree_mother, degree_father, foreign_born_mother,
         foreign_born_father, parents_married, family_income, own_home,
         foreign_born) %>%
  filter(!is.na(race) & !is.na(current_grade) & current_grade!="Pre-K")

save(acs, file=here("analysis","output","acs.RData"))


# Some models -------------------------------------------------------------

#try a model
#model <- glm(below_exp_grade~race, 
#             data=acs, family=binomial)

#model2 <- glm(below_exp_grade~relevel(as.factor(race), "White"), 
#              data=acs, family=binomial)

  
  
