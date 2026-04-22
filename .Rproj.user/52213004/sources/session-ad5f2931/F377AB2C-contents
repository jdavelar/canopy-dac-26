#########################################
#### CLEAN 2026 INTERNAL CANOPY DATA ####
#########################################

#libraries
library(here)
library(tidyverse)
library(rio)
library(canopyexplorer)

#data
raw <- import(here("data/canopy_2026_raw.csv")) #Amber's half-cleaned version
# qualtrics <- import(here("data/qualtrics_2026_raw.csv)) #fully raw version from Qualtrics
var_names <- import(here("data/survey_var_names.csv")) %>% pull(`Variable name`)

# check & subset cols
clean <- raw %>% 
  rename(practices_colead_cbo = `co-leadership_community`,
         practices_colead_family = `co-leadership_families`,
         practices_colead_industry = `co-leadership_industry`,
         practices_colead_students = `co-leadership_students`,
         practices_colead_teachers = `co-leadership_teachers`) %>% 
  select(any_of(var_names), 
         starts_with("impact"), 
         starts_with("practices"), 
         starts_with("core"), 
         starts_with("time"))

# verifying col types
clean <- clean %>% 
  # convert all blanks or NAs
  mutate(across(where(is.character), ~na_if(., ""))) %>% 
  mutate(across(where(is.character), ~na_if(., "N/A"))) %>% 
  mutate(across(where(is.character), ~na_if(., "-"))) %>% 
  # recode "Online" as NA
  mutate(across(c(school_city, school_address), ~na_if(., "Online"))) %>% 
  # small manual fixes for state/city/district inconsistencies
  mutate(school_state = case_when(
    school_state == "None" ~ NA,
    school_state == "CA" ~ "California",
    TRUE ~ school_state
  ),
  # combine New York & New York City in school_city
  school_city = ifelse(school_city == "New York", "New York City", school_city),
  # standardize districts
  school_district = case_when(
    school_district == "Aldine ISD" ~ "Aldine Independent School District",
    school_district == "Cambridge" ~ "Cambridge School District",
    school_district == "Colorado Charter School Institute (CSI)" ~ "Colorado Charter School Institute",
    school_district == "Independent" ~ NA,
    school_district == "Liberty 53" ~ "Liberty Public Schools",
    school_district == "n/a; independent school" ~ NA,
    school_district == "None" ~ NA,
    school_district == "Not officially part of any school district" ~ NA,
    school_district == "Online" ~ NA,
    school_district == "Private" ~ NA,
    school_district == "Washtenaw ISD (Washtenaw Educational Options Consortium" ~ "Washtenaw ISD",
    TRUE ~ school_district
  ))

# add + modify cols as needed
clean <- clean %>% 
  # convert pct 
  mutate(across(starts_with("pct") & !ends_with("na"), ~round(.x/100, 2))) %>% 
  # rewrite locale
  clean_locale(., "locale_urban", "locale_rural", "locale_suburban") %>% 
  rename(school_locale = locale) %>% 
  # create locale_multiple
  mutate(locale_multiple = if_else(rowSums(cbind(locale_rural, locale_suburban, locale_urban), na.rm = TRUE) > 1, 1, 0)) %>% 
  # create school level
  rename(temp_ignore_text = grades_other_text) %>% #weird fix - ignore col so it doesn't get overwritten NA
  clean_grades() %>% 
  rename(grades_other_text = temp_ignore_text) %>% #then rename col to bring it back in
  # racial/ethnic data
  mutate(pct_bipoc = 100 - pct_white,
  # create impact_evidence
         impact_evidence = ifelse(!is.na(impact_doc_Id_1) | !is.na(impact_doc_Id_2) | !is.na(impact_doc_Id_3), 1, 0)) 

# Function to clean race/ethnicity
clean_race_ethnicity <- function(raw) {
  selections <- str_trim(unlist(strsplit(raw, ",")))
  selections <- selections[selections != ""]
  
  # Find self-identify response
  self_idx <- which(grepl("^Prefer to self-identify:?$", selections))
  self_label <- NULL
  
  if (length(self_idx) > 0 && length(selections) > self_idx) {
    self_label <- selections[self_idx + 1]
    selections <- selections[-c(self_idx, self_idx + 1)]
  } else if (length(self_idx) > 0) {
    selections <- selections[-self_idx]
    self_label <- "unspecified"
  }
  
  # Neutral and non-racial response
  neutral <- c("Prefer not to say", "Prefer to self identify")
  main_selections <- setdiff(selections, neutral)
  
  # Handle combinations
  if (length(main_selections) == 0 && !is.null(self_label)) {
    return(paste("Self-identified as", self_label))
  } else if (length(main_selections) == 0 && "Prefer not to say" %in% selections) {
    return("Prefer not to say")
  } else if (length(main_selections) == 1 && is.null(self_label)) {
    return(main_selections)
  } else if (length(main_selections) == 1 && !is.null(self_label)) {
    return(paste(main_selections, "; Self-identified as", self_label))
  } else if (length(main_selections) > 1 && !is.null(self_label)) {
    return(paste("Multiracial; Self-identified as", self_label))
  } else if (length(main_selections) > 1) {
    return("Multiracial")
  } else {
    return(NA_character_)
  }
}
# Apply function to clean race/ethnicity
clean2 <- clean %>% 
  mutate(respondent_race = sapply(respondent_race, clean_race_ethnicity),
         # clean gender
         respondent_gender = str_remove_all(respondent_gender, ",")) 

# Reorder cols to match dictionary
dat <- clean2 %>% 
  #drop weird impact cols
  select(-starts_with("impact"), impact_evidence) %>% 
  #reorder cols that got added to the end
  relocate(school_locale, .after = pct_race_na) %>% 
  relocate(locale_multiple, .after = locale_suburban) %>% 
  relocate(c(grades_elementary, grades_middle, grades_high), .after = school_enrollment) %>% 
  relocate(pct_bipoc, .after = pct_white) %>% 
  relocate(impact_evidence, .after = student_experience) %>% 
  relocate(practices_career_prep, .after = practices_career_exploration) %>% 
  relocate(core_career_prep, .after = core_career_exploration) %>% 
  relocate(time_career_prep, .after = time_career_exploration)

# Pull in labs for saving
labs <- import(here("data", "survey_var_labs.csv"))

#### SAVE INTERNAL DATASET ####
# Usually save .Rdata with "dat" (includes tags) and "labs"
save(dat, labs,
     file = "data/complete_canopy_2026.RData")
write.csv(dat, "data/schools_2026.csv", row.names = FALSE)

