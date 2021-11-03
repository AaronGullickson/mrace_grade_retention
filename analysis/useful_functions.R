## useful_functions.R

#Put any functions that might be used across multiple scripts here so that they can be properly sourced in.

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

## Determine if race coding is consistent with reported race

#first create vectors of values from raced that include each of the major
#race groups (with the exception of Latinos who are defined differently)
# other should be considered consistent with all cases 

#include_white <- c(100,700,801:827,901:922,950:964,990)
#include_black <- c(200,700,801,830:845,901:904,917,930:935,950:955,970:973,
#                   980:983, 985:986,990)
#include_indig <- c(300:399,680:699,700,802,820:825,840:842,850:856,860,
#                   865:868,890:893,901,903,905:907,
#                   890)
#include_asian <- c()