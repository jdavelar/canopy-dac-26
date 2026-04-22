##############################################
#### CLEAN 2026 PUBLIC FACING CANOPY DATA ####
##############################################

#libraries
library(here)
library(tidyverse)
library(rio)

load("data/complete_canopy_2026.Rdata")
key <- import(here("data/data_dictionary_2026.csv")) %>% 
  pull(Field)

# Add nominator col
hist_nom <- import(here("data/nominations_airtable.csv")) %>% 
  janitor::clean_names() %>% 
  rename(nominator = portal_publicly_disclosable_nominator_org) %>% 
  mutate(year = as.numeric(str_extract(survey_name, "\\d{4}")),
         nominator = ifelse(nominator == "Anonymous", "an anonymous organization", nominator)) %>% 
  select(school_id = canopy_school_id, nominator, year) %>% 
  group_by(school_id) %>% 
  summarise(
    nominator_combined = {
      x <- nominator %>%
        str_trim() %>%
        unique()
      
      x <- x[!is.na(x) & x != ""]
      
      case_when(
        length(x) == 0 ~ NA_character_,
        length(x) == 1 ~ x,
        length(x) == 2 ~ paste(x, collapse = " and "),
        TRUE ~ paste0(
          paste(x[-length(x)], collapse = ", "),
          ", and ",
          x[length(x)]
        )
      )
    },
    .groups = "drop"
  ) %>% 
  mutate(school_id = as.numeric(school_id)) %>% 
  distinct()

# Keep those vars in your data dictionary that are public-facing
# Order to match
public <- dat %>% 
  left_join(hist_nom, by = "school_id") %>% 
  select(nominator = nominator_combined, any_of(key)) 

#### SAVE PUBLIC-FACING DATASET ####
write.csv(public, "data/public_canopy_2026.csv", row.names = FALSE)
