#############################
#### CAREER PREP SCHOOLS ####
#############################

# Task:
# Merge historical and 2026 data to create a list of school names + IDs that selected
# career prep as a tag in current and prior years.

library(here)
library(tidyverse)
library(rio)

# long dataset
long <- import(here("data/full-tags-long.csv")) %>% 
  filter(var == "practices_career_prep") %>% 
  pivot_wider(names_from = var,
              values_from = usage) %>% 
  select(-c(core, time))
# merge with names
dat_long <- import(here("data/longitudinal_data.csv")) %>% 
  select(school_id, school_name) %>% 
  right_join(long, by = "school_id", relationship = "many-to-many")
# 2026 dataset
dat_26 <- import(here("data/canopy_2026_raw.csv")) %>% 
  select(school_id, school_name, practices_career_prep) %>% 
  filter(practices_career_prep == 1) %>% 
  mutate(year = 2026) 

# merge
combined <- bind_rows(dat_long, dat_26) %>% 
  # pull the most recent school response
  group_by(school_id) %>% 
  arrange(year) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(practices_career_prep == 1) %>% 
  select(-practices_career_prep)

# Save
write.csv(combined, "data/career_prep_schools.csv", row.names = FALSE)
