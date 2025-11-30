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
  units,
  kableExtra,        # pivoting
  circlize,          # colorRamp2 heatmap
  ComplexHeatmap,     # installed via BiocManager
  shinyWidgets
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
      
      #### examiner names
        mutate(
          examiner = recode(examiner,
                             "Bartel" = "A",
                             "Gengnagel" = "B",
                             "Bender" = "C",
                             "Leesch" = "D",
                             "Stuhrberg" = "E"
                             )
              ) %>%
    
      #### patient gender
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
  
  
      #### reason for fate
        mutate(
          reason_fate = recode(reason_fate,
                               "kardial" = "cardiac",
                               "kardial / Dyspnoe" = "dyspnea",
                               "kardial /Dyspnoe" = "dypnea",
                               "Pulmonal" = "dyspnea",
                               "Dyspnoe" = "dyspnea",
                               "kardial/pulmonal" = "dyspnea",
                               "Prä-OP Übungspatient" = "training",
                               "Übungspatient/SaZ-Gutachten" = "training",
                               "Übungspatient" = "training",
                               "kardial Verlaufskontrolle" = "cardiac",
                               "Synkppe, AZ Reduktion" = "cardiac",
                               "kardial / Kreislauf" = "cardiac",
                               "kreislauf / pulmonal" = "dyspnea",
                               "Volumenstatus" = "cardiac",
                               "Kreislauf /Schock" = "cardiac",
                               "kardial / pulmonal" = "dyspnea",
                               "Kreislauf / kardial" = "cardiac",
                               "Dyspnoe/pulmonal" = "dyspnea",
                               .default = "other"
            
          )
        ) %>%
      
    #### location fate translation
      mutate(
        location_fate = recode(location_fate,
                               "ZNA" = "emergency department",
                               "ITS" = "intensive care unit",
                               "Station" = "ward"
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
          ) %>% 
    #### error type
    mutate (
      error_type = replace_na(
          error_type, "none"
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
                                "Herzinsuffizienz/ Pleuraerguss" = "congestive heart failure",
                                "Herzinsuffizienz" = "congestive heart failure",
                                "LV-Hypertrophie/atriale Dil." = "hypertension",
                                "hypertensive Entgleisung / VHF/ cor hypertensivum" = "hypertension",
                                "Herzinsuffizienz/COPD" = "congestive heart failure",
                                "Herzinsuffizienz/Pleuraerguss" = "congestive heart failure",
                                "Thoraxschmerzen" = "chest pain",
                                "Normalbefund" = "normal",
                                "Exsikkose" = "dehydration",
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
  mutate(age_cat = epikit::age_categories(age, breakers = c(0, 40, 50, 60, 70, 80, 90))) %>% 
  
  
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
          ) %>% 
  
  ## hours to tte category => for slide filter
    mutate(
      hours_to_adv_echo_cat = case_when(
        hours_to_adv_echo == "<24h"   ~ 1,
        hours_to_adv_echo == "24h"  ~ 2,
        hours_to_adv_echo == "48h"    ~ 3,
        hours_to_adv_echo == "72h" ~ 4,
        hours_to_adv_echo == ">72h" ~ 5,
        TRUE ~ NA_real_
      )
    ) %>% 
    
    
    #### outcome label fate
    mutate(correct_label = if_else(correct == 1, "correct", "incorrect")) %>% 
    
    ## pathology
    ### fate
    mutate(
      pathology_fate = recode(outcome_fate,
        "andere Befunde:Vergr. Vorhöfe" = "atrium",
        "Biatriale Dilatation" = "atrium",
        "Volumenbelastung/V.cava/ Rechtsherzbelastung/hochgradige TI" = "IVC, RV, valve",
        "andere Befunde: atriale Dilatation, mittelgradige MI" = "atrium, valve",
        "Unauffällig" = "none",
        "Volumendefizit" = "IVC",
        "Eingeschränkte LV-Funktion" = "LV",
        "Volumenbelastung" = "IVC",
        "Generalisierte Hypokineise (= LV-Funktionseinschränkung)" = "LV",
        "Rechtsherzbelastungszeichen" = "RV",
        "Eingeschränkte LV- und RV-Funktion/Volumenbelastung" = "LV, RV",
        "Eingeschränkte LV- und RV-Funktion" = "LV, RV",
        "eingeschränkte LV-Funktion" = "LV",
        "Normalbefund" = "none",
        "LV-Hypertrophie" = "LV",
        "Pleuraerguss/Volumenbelastung" = "pleura, IVC",
        "Volumenbelastung/eingeschränkte LV-Funktion" = "IVC, LV",
        "eingeschränkte LV- und RV-Funktion/ eingeschränkte EF" = "LV, RV",
        "Unauffälliger befund" = "none",
        "eingeschränkte LV- und RV-Funktion" = "LV, RV",
        "eingeschränkte LV- und RV-Funktion/RV dilatiert/Volumenbelastung" = "LV, RV, IVC"
      )
    )
  
# column order ----
## delete columns
  linelist <- linelist %>% 
    select(-c(x18, advanced_echo_followed)) %>% 
## rearrange columns
    select(
      patient_id, case_id, gender, age, age_cat, 
      clinical_diagn, clinical_diagn_grp, reason_fate, outcome_fate, pathology_fate, consequence, 
      location_fate, date_fate, hours_to_fate, hours_to_fate_cat, lower_hours_fate, upper_hours_fate, 
      everything())


# pivot -----
  linelist_long <- linelist %>% 
    separate_rows(pathology_fate, sep = ",") %>% 
    mutate(
      pathology_fate = str_trim(pathology_fate)
    ) %>% 

  
  ## own_sorting pathology_fate
  mutate(
    pathology_fate = factor(
      pathology_fate,
      levels = c("LV", "RV", "atrium", "IVC", "valve", "pleura", "none")
    )
  )


# matrix heatmap HEATMAT ------------
  
  heatmap_df <- linelist_long %>%
    group_by(examiner, pathology_fate) %>%
    summarise(
      incorrect_prop = mean(correct_label == "incorrect"),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = pathology_fate,
      values_from = incorrect_prop
    ) %>%
    as.data.frame()
  
  row.names(heatmap_df) <- heatmap_df$examiner
  heatmap_df$examiner <- NULL
  
  heat_mat <- as.matrix(heatmap_df)
  
# to do --------
