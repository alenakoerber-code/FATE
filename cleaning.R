library(renv)
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  skimr,
  readxl
)


# IMPORT ---------------
linelist_raw <- import("linelist_FATE_raw.xlsx", range = cell_rows(4:44))

skimr::skim(linelist_raw)

# CLEANING -----

## cleaning column names
linelist <- linelist_raw %>%
  
  janitor::clean_names() %>% 
  
  rename(ID_case = patient,
         date_fate = x2,
         gender = m_w,
         age = alter,
         location_fate = ort,
         time_to_fate = zeitpunkt,
         reason_fate = indikation,
         clinical_diagnosis = diagnose,
         outcome_fate = fate_befund,
         consequence = konsequenz,
         examiner = untersucher,
         advanced_echo_followed = tte_im_verlauf_erfolgt,
         time_to_adv_echo = zeipunkt,
         outcome_adv_echo = befund
         ) %>% 

# delete columns
select(-c(x16))
  