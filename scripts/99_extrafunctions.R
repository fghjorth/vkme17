gen_partyid <- function(df) {
  pid <- rep(NA, length(df$party))
  pid[df$party==1] <- 5
  pid[df$party==2] <- 1
  pid[(df$party==3 | df$party==4 | df$party==5)] <- 3
  pid[df$partyln==1] <- 4
  pid[df$partyln==2] <- 2
  return(pid)
}

hhn_format <- function(df, cfips, dh, hn) {
  # clean a Pew have/have-not survey dataset and merge it with county-level data
  # df: data; cfips: var of county fips; dh: var of U.S. divided; hn: var of self-id as have-not
  surv <- str_replace(deparse(substitute(df)), "hhn", "20")
  all_vars <- c("income", "educ", "age", "sex", "racethn", "race", "hisp", 
                "labor", "ideo", "attend", "employ",
                "party", "partystr", "partyln")
  vars <- c(cfips, dh, hn, all_vars[all_vars %in% names(df)])
  df %<>% select(one_of(vars))
  names(df)[1:3] <- c("cfips", "dh", "hn")
  df %<>% mutate(
    fips = as.integer(cfips),
    state = floor(fips/1000),
    div_hhn = ifelse(dh==1, 1, ifelse(dh==2, 0, NA)),
    have_not = ifelse(hn==2, 1, ifelse(hn==1, 0, NA)),
    income = as.integer(ifelse(income<=9, income, NA)), # 1 to 9
    educ = as.integer(ifelse(educ<=7, educ, NA)), # 1 to 7
    age = ifelse(age<99, age, NA),
    male = ifelse(sex==1, 1, 0),
    ideo = as.integer(6 - ifelse(ideo<=5, ideo, NA)) # 1 to 5
  )
  df$partyid <- gen_partyid(df)
  if (names(df) %>% str_detect("racethn") %>% any()) {
    df %<>% mutate(white = ifelse(racethn==1, 1, 0))
  } else df %<>% mutate(white = ifelse(race==1 & hisp!=1, 1, 0))
  if (names(df) %>% str_detect("labor") %>% any()) {
    df %<>% mutate(union = ifelse(labor<=3, 1, ifelse(labor==4, 0, NA)))
  } else df %<>% mutate(union = NA)
  if (names(df) %>% str_detect("attend") %>% any()) {
    df %<>% mutate(attend = 7 - ifelse(attend<=6, attend, NA))
  } else df %<>% mutate(attend = NA)
  if (names(df) %>% str_detect("employ") %>% any()) {
    df %<>% mutate(emp = ifelse(employ<3, 1, ifelse(employ==3, 0, NA)))
  } else df %<>% mutate(emp = NA)
  
  df$survey <- surv
  
  vars2 <- c("fips", "state", "div_hhn", "have_not", "income", "educ", "age", "male", "white", 
             "union", "emp", "partyid", "ideo", "attend", "survey")
  df %<>% select(one_of(vars2)) %>% left_join(cnty_data, by = "fips") %>% filter(white==1)
} 

hhn_mi <- function(df, seed=324) {
  # multiply impute missing data in a cleaned and merged Pew dataset
  mdf <- missing_data.frame(as.data.frame(df))
  mdf <- change(mdf, y = c("fips", "state"), what = "type", to = "irrelevant")
  mdf <- change(mdf, y = c("income", "educ", "attend"), what = "type", to = "ordered-categorical")
  mdf <- change(mdf, y = "age", what = "type", to = "bounded-continuous", lower=18, upper=97)
  mdf_mi <- mi(mdf, seed=seed) 
  
  # switch to mitools format (no support for glmer in mi::pool)
  mdf_mi_list <- complete(mdf_mi, m=10) 
  mdf_mi_list <- lapply(mdf_mi_list, function(df) 
    sapply(df, function(v) 
      if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>% data.frame) # get rid of %&*(&^ factors
  imputationList(mdf_mi_list)
}
