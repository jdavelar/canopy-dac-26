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
  mutate(across(everything(), ~na_if(., "n/a"))) %>% 
  mutate(school_name = str_remove_all(school_name, "[:punct:][:punct:]"),
         nominator_reason_n = ifelse(school_id == 1698, 
                                     "Desert Willow and its district are part of the national Opportunity Culture initiative, in which schools extend the reach of excellent teachers and their teams to many more students, for more pay, within regular budgets. The school has achieved high-growth learning results for its students; it posted the highest growth in the state in the literacy proficiency rate among medium-size schools, and the 31st-highest growth in the math rate. It received Certified Opportunity Culture® School status, Level 1, signifying a focus on student access to instruction led by Multi-Classroom Leader® teaching teams and incorporation of small-group, high-dosage tutoring into daily schedules. \nThe foundation of the Opportunity Culture model is the Multi-Classroom Leader role, an excellent teacher who leads a small teaching team, providing instructional guidance and frequent, on-the-job development, while continuing to teach part of the time. The school uses this and other teacher and paraprofessional team roles to reach more students with excellent teaching.",
                                     nominator_reason_n))

#### SAVE PUBLIC-FACING DATASET ####
write.csv(noms_2026, "data/public_nominators_2026.csv", row.names = FALSE)
