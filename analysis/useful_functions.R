## useful_functions.R

#Put any functions that might be used across multiple scripts here so that they can be properly sourced in.


# Functions for coding and cleaning ---------------------------------------

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
    educd<62 ~ "LHS",
    educd<81 ~ "HS",
    educd<101 ~ "AA",
    educd==101 ~ "BA",
    TRUE ~ "G"
  )
  degree <- factor(degree,
                   levels=c("LHS","HS","AA","BA","G"))
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

#This function will return contrasts using fractions of single race
#responses for multiracial respondents
race_half_contrast <- function(race) {
  #the default treatment contrast will do most of the work, but I need to 
  #put in the 0.5 cases
  contr_race <- contrasts(race)
  #lets loop through rows for multiracials and decide where the 0.5 should go
  for(i in str_which(rownames(contr_race),"/")) {
    race_name <- rownames(contr_race)[i]
    comp_races <- str_split(race_name, "/")[[1]]
    fraction <- 1/length(comp_races)
    #one category is missing because it is the reference so remove if so
    comp_races <- comp_races[comp_races %in% colnames(contr_race)]
    contr_race[race_name, comp_races] <- fraction
  }
  return(contr_race)
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
  
  #Part-Latino respondents should have already been picked up by other
  #conditions.
  
  #if neither parent is Latino, but child is classified as Latino then reject
  #this should have been dealt with by first two conditions, but may have been
  #re-introduced in later cases
  TF <- TF & 
    (str_detect(race_constructed, "Latino") | 
       (!str_detect(race_constructed, "Latino") & hispan_child==0))

  return(TF)
}


# Functions for plotting effects ------------------------------------------

calculate_cond_means <- function(model, type="response") {
  
  #TODO: Estimate means for each variable, currently just supplying some
  #reasonable baseline
  fake_data <- data.frame(race=levels(acs$race),
                          year=2015,
                          current_grade="6th",
                          state=13,
                          metro="Metro non-central",
                          foreign_born=FALSE,
                          foreign_born_mother=FALSE,
                          foreign_born_father=FALSE,
                          degree_mother="HS",
                          degree_father="HS",
                          family_income=mean(acs$family_income),
                          own_home=TRUE,
                          parents_married=TRUE)
  
  predicted <- predict(model, fake_data, se=TRUE, type=type)
  
  coefs <- tibble(term=fake_data$race, estimate=predicted$fit, 
                  se=predicted$se.fit) %>%
    mutate(multiracial=str_detect(term, "/"))
  
  
  #ok convert this to have single race groups for each multiracial group
  #panel and get the assumed midpoint value
  coef_table <- map(as.list(str_subset(coefs$term, "/")), function(x) {
    coefs %>%
      filter(term %in% c(x, str_split(x, "/")[[1]])) %>%
      mutate(mrace=factor(x,
                          levels=c("Black/White",
                                   "White/Indigenous",
                                   "Latino/Asian",
                                   "Black/Latino",
                                   "White/Latino",
                                   "Indigenous/Latino",
                                   "Black/Asian",
                                   "White/Asian",
                                   "Indigenous/Asian",
                                   "Black/Indigenous")),
             estimate_mid=ifelse(multiracial, 
                                 mean(estimate[!multiracial]),
                                 estimate),
             se_mid=ifelse(multiracial, 
                                  sqrt(sum(se[!multiracial]^2)/4),
                                  se))
  }) %>%
    bind_rows()
  
  #get terms ordered
  race_names <- levels(reorder(factor(coef_table$term), coef_table$estimate))
  #pull out single race and multiple race as separate vectors
  race_single <- str_subset(race_names, "/", negate=TRUE)
  race_multi <- str_subset(race_names, "/", negate=FALSE)
  #start with single races
  race_full <- race_single
  #now loop through multiple race names and append based on the higher group
  for(race in race_multi) {
    temp <- str_split(race, "/")[[1]]
    idx <- max(which(race_full==temp[1]), which(race_full==temp[2]))
    race_full <- append(race_full, race, idx-1)
  }
  
  #now use this as the levels of term as a factor
  coef_table <- coef_table %>%
    mutate(term=factor(term, levels=race_full))
  
  return(coef_table)
}

