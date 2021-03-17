#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 


# Load libraries and functions --------------------------------------------

#source in any useful functions
library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))

code_race <- function(race, hisp) {
  #TODO: split out pacific islanders
  #TODO: handle multiracial groups that are single big race (e.g Chinese/Japanese)
  race <- case_when(
    is.na(race) | is.na(hisp) ~ NA_character_,
    hisp != 0 ~ "Latino",
    race == 100 ~ "White",
    race == 200 ~ "Black",
    race >= 300 & race < 400 ~ "AIAN",
    race >= 400 & race < 700 ~ "API",
#    race == 700 ~ "Other",
#    race > 700 ~ "Multiracial"
  )
  return(race)
}

#not using this anymore because I am coding by contrasts (much cleaner)
code_fractional_race <- function(race, race_name) {
  race_frac <- case_when(
    is.na(race) ~ NA_real_,
    race==race_name ~ 1,
    str_detect(race, paste("/",race_name,"|",race_name,"/",sep="")) ~ 0.5,
    TRUE ~ 0)
  return(race_frac)
}


# Read the raw data -------------------------------------------------------

acs <- read_fwf(here("analysis","input","usa_00109.dat.gz"), 
                col_positions = 
                  fwf_positions(start = c(1,11,32,55,88,89,94, 98,101,109,112,116,118,121,134,137,146,149,158,161,170,172),
                                end   = c(4,18,41,55,88,91,96,100,103,109,114,117,120,123,136,139,148,151,160,163,171,173),
                                col_names = c("year","serial","hhwt",
                                              "metro","sex","age",
                                              "raced","hispand","bpl",
                                              "school","educd","gradeattd",
                                              "age_mom","age_pop",
                                              "raced_mom","raced_pop",
                                              "hispand_mom","hispand_pop",
                                              "bpl_mom","bpl_pop",
                                              "educ_mom","educ_pop")),
                       col_types = cols(.default = "i"),
                       progress = FALSE)

#what percent of cases have both mom and dad's race
mean(!is.na(acs$raced_pop) & !is.na(acs$raced_mom) & 
       !is.na(acs$hispand_pop) & !is.na(acs$hispand_mom))


# Recode variables --------------------------------------------------------

#I worry a little bit about specifying grade retention for the older grades where
#grade retention is only possible at ages when kids are often out of the household
#and therefore not eligible for the sample. Might create some weird biases.

#Code grade retention
#for now just use the level+6 as above grade, but this probably needs refinement
#to address lots of issues.

acs <- acs %>% 
  mutate(race_mother=code_race(raced_mom, hispand_mom),
         race_father=code_race(raced_pop, hispand_pop),
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
           levels=c("K","1st","2nd","3rd",paste(4:12,"th",sep=""))),
         below_exp_grade=age>=as.numeric(current_grade)+6,
         not_attending=school==1) 

#code the contrast matrix for race to adjust for fractions
acs$race <- factor(acs$race,
                   levels=c("White","Black","AIAN","API","Latino",
                            "AIAN/API","AIAN/Black","AIAN/Latino","AIAN/White",
                            "API/Black","API/Latino","API/White",
                            "Black/Latino","Black/White",
                            "Latino/White"))
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

table(acs$race_mother, acs$race_father, exclude=NULL)

#grade-age progression
with(subset(acs, !not_attending),
     table(age, current_grade, exclude=NULL))

acs$both_missing <- is.na(acs$race_father) & is.na(acs$race_mother)
round(tapply(acs$both_missing, acs[,c("age","current_grade")], mean, na.rm=TRUE), 3)

# Trim to analytical data -------------------------------------------------

acs <- acs %>%
  select(year, age, current_grade, below_exp_grade, not_attending,
         race_mother, race_father, race) %>%
  filter(!is.na(race_mother) & !is.na(race_father))


# Some models -------------------------------------------------------------

#try a model
model <- glm(below_exp_grade~race, 
             data=acs, family=binomial)

model2 <- glm(below_exp_grade~relevel(as.factor(race), "White"), 
              data=acs, family=binomial)

  
  
