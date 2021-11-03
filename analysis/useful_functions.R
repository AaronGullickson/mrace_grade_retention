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
is_race_consistent <- function(race_child, hispan_child, race_mom, hispan_mom,
                               race_dad, hispan_dad, race_constructed) {
  # start with the case wherechild's race is exactly consistent with one of 
  # parent's races. This case should take care of about 95% of the data and 
  # should take care of most single race respondents and biracial respondents
  #  that identified with one parent
  TF <- (race_child==race_mom & hispan_child==hispan_mom)  | 
    (race_child==race_dad & hispan_child==hispan_dad)

  #in cases where either parent is hispanic allow this to match even if
  #race does not, since there may be discrepancy here in terms of reporting
  #other
  TF <- TF | 
    (hispan_dad!=0 & hispan_child==hispan_dad) | 
    (hispan_mom!=0 & hispan_child==hispan_mom)
  
  #if at least one parent reports indigenous allow a couple of residual 
  #indigenous categories
  TF <- TF | 
    (str_detect(race_constructed, "Indigenous") & 
       race_child %in% c(361:362,855,942))
  
  #if at least one parent reports Asian allow residual and some combined 
  #categories
  TF <- TF |
    (str_detect(race_constructed, "Asian") & race_child %in% c(671:679,887))

  #if at least one parent is Latino, allow residual category
  TF <- TF |
    (str_detect(race_constructed, "Latino") & hispan_child==498)
  
  #if multiracial category consider "other" to be ok
  TF <- TF | 
    (str_detect(race_constructed, "/") & race_child==700)
  
  #now go through specific multiracial possibilities
  #non-Latino ones are a pain but straightforward
  #we allow other to operate as a wildcard
  TF <- TF | 
    (str_detect(race_constructed, "Black/White") & 
       race_child %in% c(801,826,827,845,901:904,907,917,921:923,925,932,934,
                         935,950:955,961:963,971:973,980:986,990))
  TF <- TF | 
    (str_detect(race_constructed, "Black/Indigenous") & 
       race_child %in% c(830,840:845,856,890,901,903,904,907,930:935,941:943,
                         950:955,970:974,9890:990))
  
  TF <- TF | 
    (str_detect(race_constructed, "Black/Asian") & 
       race_child %in% c(831:838,845,880:886,902,904,917,920:925.930,933:935,
                         941,943,950,953:955,961,963,970:974,976,980:983,985,
                         986,990))
  
  TF <- TF | 
    (str_detect(race_constructed, "White/Indigenous") & 
       race_child %in% c(802,820:827,856,890:892,901,903,905,906:916,920:925,
                         941:943,950:953,955:964,974:976,980:990))
  
  TF <- TF | 
    (str_detect(race_constructed, "White/Asian") & 
       race_child %in% c(810:819,826,880:886,902,905,910:923,950,953,960,961,
                         963,964,980,981,983:986,990))
  
  TF <- TF |
    (str_detect(race_constructed, "Indigenous/Asian") & 
       race_child %in% c(850:854,856:868,880:886,890:893,905,910:925,930,
                         932:944,950,952:976,980:990))
  
  #if neither parent is Latino, but child is classified as Latino then reject
  #this should have been dealt with by first two conditions, but may have been
  #re-introduced in later cases
  TF <- TF & 
    (str_detect(race_constructed, "Latino") | 
       (!str_detect(race_constructed, "Latino") & hispan_child==0))

  
  return(TF)
}


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