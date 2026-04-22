####################################
#### HISTORICAL GOVERNANCE DATA ####
####################################

# This file uses longitudinal data from 2024, school survey data from 2025, and
# eventually school survey data from 2026 to generate a list of historical school
# governance data - year, ID, and governance type. 

# This dataset will be used to create a column in the Schools tab in AirTable to
# allow filtering for targeted communication and establishing a list of potential
# focus schools for Future Ready selection. This dataset will also be compared
# against NCES data to check for discrepancies in schools' definitions of their
# charter status and CCD definition of what a charter is (below).

library(here)
library(rio)
library(tidyverse)

#### MERGING HISTORICAL DATA ####
long <- import(here("data/longitudinal_data.csv")) %>% 
  select(year, school_id, nces_id, school_type)
load("data/complete_canopy_2025.RData") 

# pull from long data
clean <- dat %>% 
  select(school_id, nces_id, school_type) %>% 
  mutate(year = 2025) %>% 
  bind_rows(long) %>% 
  arrange(school_id, year) %>% 
  group_by(school_id) %>% 
  fill(school_type, .direction = "up") %>% 
  slice_max(year) %>% 
  mutate(school_type = case_when(
    school_id == 287 ~ "Public district school",
    school_id == 402 ~ "Public charter school",
    school_id == 391 ~ "Public district school", #NCES says data not applicable for charter?
    school_id == 396 ~ "Public charter school",
    TRUE ~ school_type
  ))

# check missing
test <- clean %>% filter(is.na(school_type))
missing <- test %>% pull(school_id)
dat_21a <- import(here("data/schools_21a.csv")) %>% 
  filter(school_id %in% missing)
dat_21b <- import(here("data/schools_21b.csv")) %>% 
  filter(school_id %in% missing)
# these are true missing in data - they did not respond; flag for Sarah
# can fill in manually those that had NCES ID - verified with NCES lookup
# done in pipe above; 351 & 408 had no NCES

# save CSV
write.csv(clean, "data/longitudinal_school_type.csv", row.names = FALSE)
rm(dat, dat_21a, dat_21b, labs, long, test, missing)

#### COMPARISON OF REPORTED & NCES SCHOOL TYPE ####
nces <- import(here("data/nces_public_schools.csv")) %>% 
  janitor::clean_names() %>% 
  select(nces_name = school_name,
         nces_state = state_abbr_public_school_latest_available_year,
         nces_id = school_id_12_digit_nces_assigned_public_school_latest_available_year,
         public = school_type_public_school_2024_25,
         charter = charter_school_public_school_2024_25) %>% 
  # note when creating NCES school type I overwrote the "Not applicable" value as public district school
  # i.e., only charter if for sure meets charter definition
  mutate(nces_school_type = ifelse(charter == "1-Yes", "Public charter school", "Public district school"),
         nces_id = as.numeric(nces_id)) %>% 
  select(nces_id, nces_school_type)
compare <- clean %>% 
  mutate(nces_id = as.numeric(nces_id)) %>% 
  left_join(nces, by = "nces_id") %>% 
  mutate(match = ifelse(school_type == nces_school_type, "yes", "no"))

# check where didn't match
compare %>% filter(match == "no")
# only 4:
# 141 = Norris Academy
# 195 = Statesmen College Preparatory Academy for Boys PCS
# 747 = Nowell Academy
# 923 = Advanced Learning Academy

#### COMPARISON OF NOMINATION TYPE AND SCHOOL REPORTED TYPE ####
noms <- import(here("data/all_noms.csv")) %>% 
  janitor::clean_names() %>% 
  rename(nom_id = nomination_id,
         survey = survey_name,
         school_id = canopy_school_id,
         nom_school_type = governance_descriptor_from_nominations_from_official_school_name) %>% 
  filter(school_id != "") %>% 
  separate_rows(school_id, sep = ",\\s*") %>% 
  mutate(year = case_when(
    survey == "Nominator_2019-03" ~ 2019,
    survey == "Nominator_2020-08" ~ 2021,
    survey == "Nominator_2020-11" ~ 2021,
    survey == "Nominator_2021-12" ~ 2022,
    survey == "Nominator_2022-11" ~ 2023,
    survey == "Nominator_2023-11" ~ 2024,
    survey == "Nominator_2024-10" ~ 2025,
    survey == "Nominator_2025-10" ~ 2026
  ),
  school_id = as.numeric(school_id)) %>% 
  separate_rows(nom_school_type, sep = ",\\s*") %>% 
  select(year, school_id, nom_school_type) %>% 
  distinct() %>% 
  left_join(clean, by = c("school_id", "year")) %>% 
  mutate(match = case_when(
    nom_school_type == school_type ~ "yes",
    is.na(school_type) ~ "survey not taken",
    nom_school_type == "" ~ "nominator did not provide",
    nom_school_type != school_type ~ "no"
  ))

# 37 nos
check <- noms %>% filter(match == "no") %>% 
  select(-nces_id)
write.csv(check, "data/nom_discrepancies.csv", row.names = FALSE)

#### CREATE NCES VARIABLE FOR AIRTABLE ####
rm(noms, check)
at_schools <- import(here("data/airtable_schools.csv")) %>% 
  janitor::clean_names() %>% 
  rename(nces_id = nces_school_id) %>% 
  mutate(nces_id = as.numeric(nces_id))

at_mapping <- at_schools %>% 
  filter(!is.na(nces_id)) %>% 
  left_join(nces, by = "nces_id") %>% 
  rename(nces_school_id = nces_id)
write.csv(at_mapping, "data/at_mapping_nces_school_type.csv", row.names = FALSE)
