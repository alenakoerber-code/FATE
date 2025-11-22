library("renv")
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


# CLEANING -----

## cleaning column names
linelist <- linelist_raw %>%
  
  janitor::clean_names() %>% 
  
  rename(case_id = patient,
         date_fate = x2,
         patient_name = name,
         gender = m_w,
         age = alter,
         location_fate = ort,
         time_to_fate_h = zeitpunkt,
         reason_fate = indikation,
         clinical_diagn = diagnose,
         outcome_fate = fate_befund,
         consequence = konsequenz,
         examiner = untersucher,
         advanced_echo_followed = tte_im_verlauf_erfolgt,
         time_to_adv_echo = zeipunkt,
         outcome_adv_echo = befund
         ) 
  
## clean values
  skimr::skim(linelist)
  
  linelist <- linelist %>% 
  ### Patient names for pseudonymisation
  mutate(
    patient_name = recode(patient_name, 
                    "Rahaus, Wolfgang (Le) II" = "Rahaus, Wolfgang",
                    "Rahaus, Wolfgang (Le) I" = "Rahaus, Wolfgang"  
                    )
      ) %>%
  ### gender
  mutate(
    gender = recode(gender,
                    "m" = "male",
                    "w" = "female"
                    )
          ) %>% 
  
  ### date as date format
  mutate(    date_fate  = as.Date(date_fate))
  
  ### missing values
  
  
  
  # pseudoonymisation ------
  set.seed(101)

  ## create key
id_key <- linelist %>%
  distinct(patient_name) %>% 
  mutate(
    patient_id = sample(101:999, size = n(), replace = TRUE)
  )
  
  ## replace names with id
  linelist <- linelist %>%
    left_join(id_key, by = "patient_name") %>%
     select(-patient_name)   # Namen entfernen
  
# add columns
  linelist <- linelist %>% 
  ## diagnosis group
  mutate(
    clinical_diagn_grp = recode(clinical_diagn,
                                "Z.n. Schrittmachernendokarditis - unklares Fieber" = "fever",
                                "Herzinsuffizienz/ Pleuraerguss" = "congestive heart failure",
                                "Herzinsuffizienz" = "congestive heart failure",
                                "LV-Hypertrophie/atriale Dil." = "hypertension",
                                "hypertensive Entgleisung / VHF/ cor hypertensivum" = "hypertension",
                                "Herzinsuffizienz/COPD" = "congestive heart failure",
                                "Herzinsuffizienz/Pleuraerguss" = "congestive heart failure",
                                "Thoraxschmerzen" = "chest pain",
                                "Normalbefund" = "normal",
                                "Exsikkose" = "dehydration",
                                "Normozytäre Anämie" = "anemia",
                                "TAA / Herzinsuffizienz" = "arrhythmia",
                                "VHF / Herzinsuffizienz" = "arrhythmia",
                                "TAA bei VHF" = "arrhythmia",
                                "Herzrhythmusstörung" = "arrhythmia",
                                "Herzinsuffizienz / TAA bei VHF" = "arrhythmia",
                                "V.a. LAE" = "pulmonary embolism",
                                "Lungenarterienembolie" = "pulmonary embolism",
                                "hypertensives Lungenödem" = "hypertension",
                                .default = "other"
    )
    ) %>% 
  ## age categories: 0 to 85 by 5s
  mutate( age_cat = epikit::age_categories(age, breakers = c(0, 40, 50, 60, 70, 80, 90)))
  
  
# column order ----
## delete columns
  linelist <- linelist %>% 
    select(-c(x16)) %>% 
## rearrange columns
    select(patient_id, case_id, gender, age, age_cat, clinical_diagn, clinical_diagn_grp, reason_fate, outcome_fate, consequence, location_fate, date_fate, time_to_fate_h, everything())

