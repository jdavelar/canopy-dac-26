##################################################
#### CLEAN 2026 PUBLIC FACING NOMINIATOR DATA ####
##################################################

#libraries
library(here)
library(tidyverse)
library(rio)

hist_nom <- import(here("data/nominations_airtable.csv"))

# Pull just 2026 nominations
noms_2026 <- hist_nom %>% 
  janitor::clean_names() %>% 
  rename(school_id = canopy_school_id,
         nominator = portal_publicly_disclosable_nominator_org) %>% 
  mutate(year = as.numeric(str_extract(survey_name, "\\d{4}")),
         nominator = ifelse(nominator == "Anonymous", "an anonymous organization", nominator)) %>% 
  filter(year == 2025) %>% 
  select(school_id,
         school_name = official_school_name,
         school_city_n = school_city,
         school_state_n = school_state,
         school_district_n = district_or_cmo,
         nominator_reason_n = reason_for_nominating) %>% 
  filter(school_id != "") %>% 
  mutate(across(everything(), ~na_if(., ""))) %>% 
  mutate(across(everything(), ~na_if(., "N/A"))) %>% 
  mutate(across(everything(), ~na_if(., "n/a")))

#### SAVE PUBLIC-FACING DATASET ####
write.csv(noms_2026, "data/public_nominators_2026.csv", row.names = FALSE)
