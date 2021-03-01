#' ---
#' title: "organize_data.R"
#' author: ""
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

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
    race == 700 ~ "Other",
    race > 700 ~ "Multiracial"
  )
  race <- factor(race,
                 levels=c("White","Black","AIAN","API","Latino","Other",
                          "Multiracial"))
  return(race)
}

acs <- read_fwf(here("analysis","input","usa_00104.dat.gz"), 
                col_positions = 
                  fwf_positions(start = c(1,11,32,55,88,89,94, 98,101,109,116,118,121,134,137,146,149,158,161,170,172),
                                end   = c(4,18,41,55,88,91,96,100,103,109,117,120,123,136,139,148,151,160,163,171,173),
                                col_names = c("year","serial","hhwt",
                                              "metro","sex","age",
                                              "raced","hispand","bpl",
                                              "school","gradeattd",
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

acs$race_mother <- code_race(acs$raced_mom, acs$hispand_mom)
table(acs$raced_mom, acs$race_mother)

acs$current_grade <- factor(case_when(
  acs$gradeattd == 10 ~ "PreK",
  acs$gradeattd == 20 ~ "K",
  acs$gradeattd == 31 ~ "1st",
  acs$gradeattd == 32 ~ "2nd",
  acs$gradeattd == 33 ~ "3rd",
  acs$gradeattd == 34 ~ "4th",
  acs$gradeattd == 41 ~ "5th",
  acs$gradeattd == 42 ~ "6th",
  acs$gradeattd == 43 ~ "7th",
  acs$gradeattd == 44 ~ "8th",
  acs$gradeattd == 51 ~ "9th",
  acs$gradeattd == 52 ~ "10th",
  acs$gradeattd == 53 ~ "11th",
  acs$gradeattd == 54 ~ "12th",
  acs$gradeattd >= 60 ~ "College"),
  levels=c("PreK","K","1st","2nd","3rd",paste(4:12,"th",sep=""),"College"))

acs$race_father <- code_race(acs$raced_pop, acs$hispand_pop)

table(acs$race_mother, acs$race_father, exclude=NULL)

#grade-age progression
with(subset(acs, school==2),
     table(age, current_grade, exclude=NULL))
