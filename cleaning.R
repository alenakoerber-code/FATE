# intro -------

library("renv")
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization
  skimr,
  readxl,      # reads excel datasets
  units
)


# IMPORT 
linelist_raw <- import("linelist_FATE_raw.xlsx", range = cell_rows(4:44))


# cleaning --------

## name cleaning ------
linelist <- linelist_raw %>%
  
  janitor::clean_names() %>% 
  
  rename(case_id = patient,
         date_fate = x2,
         patient_name = name,
         gender = m_w,
         age = alter,
         location_fate = ort,
         hours_to_fate = zeitpunkt,
         reason_fate = indikation,
         clinical_diagn = diagnose,
         outcome_fate = fate_befund,
         consequence = konsequenz,
         examiner = untersucher,
         advanced_echo_followed = tte_im_verlauf_erfolgt,
         hours_to_adv_echo = zeipunkt,
         outcome_adv_echo = befund
         ) 
  
## value cleaning -------
  skimr::skim(linelist)
  
  linelist <- linelist %>%
    ### recode values ---------
      #### Patient names for pseudonymisation
        mutate(
          patient_name = recode(patient_name, 
                    "Rahaus, Wolfgang (Le) II" = "Rahaus, Wolfgang",
                    "Rahaus, Wolfgang (Le) I" = "Rahaus, Wolfgang"  
                    )
              ) %>%
      #### gender
        mutate(
            gender = recode(gender,
                    "m" = "male",
                    "w" = "female",
                    .default = "other"
                    )
              ) %>% 
      #### hours to fate
        mutate(
          hours_to_fate = recode(hours_to_fate,
                                 "bis 2 h" = "< 2",
                                 "2 bis 12h" = "2 - 12",
                                 ">12" = "> 12"
                                 )
           
        ) %>% 
  
  ### set class
    
    #### date as date format
    mutate(date_fate  = as.Date(date_fate)) %>% 
  
  ### missing values ---------
    #### fate_date replaced with median date
    mutate (
      date_fate = replace_na (
                date_fate, median(date_fate, na.rm = TRUE) 
                             )
          )
  
  
  # pseudonymisation ------
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
     select(-patient_name)   # delete names
  
# add columns -------
  linelist <- linelist %>% 
  ## diagnosis group -------
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
  ## age categories: manual ------
  mutate( age_cat = epikit::age_categories(age, breakers = c(0, 40, 50, 60, 70, 80, 90))) %>% 
  
  
  ## hours to fate ----------
    mutate(
      hours_to_fate_cat = case_when(
        hours_to_fate == "< 2"   ~ 1,
        hours_to_fate == "2 - 12"  ~ 2,
        hours_to_fate == "> 12"    ~ 3,
        TRUE ~ NA_real_
      )
    ) %>% 
  
    mutate(
      lower_hours_fate = case_when(
        hours_to_fate == "< 2" ~ 0,
        hours_to_fate == "2 - 12" ~ 2,
        hours_to_fate == "> 12" ~ 12,
        TRUE ~ NA_real_
        ),
      upper_hours_fate = case_when(
        hours_to_fate == "< 2" ~ 2,
        hours_to_fate == "2 - 12" ~ 12,
        hours_to_fate == "> 12" ~ Inf,
        TRUE ~ NA_real_
      )
          )
  
# column order ----
## delete columns
  linelist <- linelist %>% 
    select(-c(x16)) %>% 
## rearrange columns
    select(
      patient_id, case_id, gender, age, age_cat, 
      clinical_diagn, clinical_diagn_grp, reason_fate, outcome_fate, consequence, 
      location_fate, date_fate, hours_to_fate, hours_to_fate_cat, lower_hours_fate, upper_hours_fate, 
      everything())

