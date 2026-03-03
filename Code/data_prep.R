# 202602 Studley Park Boathouse
# Author: Alana Little, NEPHU (alana.little@austin.org.au)

# Read in and wrangle linelist and Forms survey data

################################################################################
# Linelist
################################################################################
linelist_raw <- readxl::read_xlsx(paste0(here::here(), "/Data/Linelist_Studley_Park_", filedate, ".xlsx"),
                                  sheet     = "Linelist",
                                  skip      = 2,
                                  guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Data wrangling
linelist_clean <- linelist_raw %>%
  dplyr::filter(!is.na(symptomatic)) %>% 
  #
  dplyr::mutate(
    date_of_birth = janitor::convert_to_date(date_of_birth,
                                             character_fun = lubridate::dmy),
    #
    symptom_onset_date = janitor::convert_to_date(symptom_onset_date,
                                                  character_fun = lubridate::dmy),
    #
    symptom_end_date = janitor::convert_to_date(symptom_end_date,
                                                character_fun = lubridate::dmy),
    #
    symptom_onset_time = hms::as_hms(symptom_onset_time),
    symptom_end_time   = hms::as_hms(symptom_end_time),
    #
    symptom_onset_datetime = lubridate::ymd_hms(paste0(symptom_onset_date, symptom_onset_time)),
    symptom_end_datetime   = lubridate::ymd_hms(paste0(symptom_end_date, symptom_end_time)),
    #
    time_to_onset = symptom_onset_datetime - datetime_event,
    #
    symptoms_status = dplyr::case_when(
      !is.na(symptom_onset_date) & is.na(symptom_end_date)  ~ "Ongoing",
      !is.na(symptom_onset_date) & !is.na(symptom_end_date) ~ "Resolved",
      TRUE ~ NA_character_),
    #
    symptom_end_date = dplyr::case_when(
      symptoms_status == "Ongoing" ~ lubridate::ymd(Sys.Date()),
      TRUE ~ symptom_end_date),
    #
    age_years = (date_of_birth %--% date_event) / years(1),
    #
    age_group = factor(dplyr::case_when(
      age_years < 10  ~ "0-9 years",
      age_years < 20  ~ "10-19 years",
      age_years < 30  ~ "20-29 years",
      age_years < 40  ~ "30-39 years",
      age_years < 50  ~ "40-49 years",
      age_years < 60  ~ "50-59 years",
      age_years < 70  ~ "60-69 years",
      age_years < 80  ~ "70-79 years",
      age_years < 90  ~ "80-89 years",
      age_years >= 90 ~ "90+ years",
      TRUE ~ "Not stated"),
      levels = c("0-9 years",
                 "10-19 years",
                 "20-29 years",
                 "30-39 years",
                 "40-49 years",
                 "50-59 years",
                 "60-69 years",
                 "70-79 years",
                 "80-89 years",
                 "90+ years",
                 "Not stated")),
    #
    sex = factor(dplyr::case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      TRUE ~ NA_character_),
      levels = c("Male",
                 "Female")),
    #
    abdo_pain = dplyr::case_when(stringr::str_detect(other_specify, "abdo") ~ "Y"),
    #
    household = dplyr::case_when(
      residential_address == "27 Ashe crescent Bellfield, 3081"    ~ "House 01",
      residential_address == "15 Jordan street Ashwood 3147"       ~ "House 02",
      residential_address == "13 Maypark ave Ashwood 3147"         ~ "House 03",
      residential_address == "5 Mclelland Way Burwood East 3151"   ~ "House 04",
      residential_address == "35 Pembroke Street Surry Hills 3127" ~ "House 05",
      residential_address == "5 Neale St Preston 3072"             ~ "House 06",
      residential_address == "1/2 Colvin Ct Glen Waverley 3150"    ~ "House 07",
      TRUE ~ NA_character_))

################################################################################
# Forms survey data
################################################################################
survey_raw <- readxl::read_xlsx(paste0(here::here(), "/Data/Survey_Studley_Park_", filedate, ".xlsx"),
                                guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Exclude test responses
survey_raw <- survey_raw %>% 
  dplyr::filter(id > 5)

# Summary questions about party attendance
survey_attendance <- survey_raw %>% 
  dplyr::select(name = enter_your_full_name,
                attended = did_you_or_someone_in_your_household_attend_a_first_birthday_party_on_sunday_15th_february_2026_at_studley_park_boathouse,
                attended_n = how_many_members_of_your_family_attended_the_event,
                unwell_n = how_many_members_of_your_family_were_unwell_with_gastroenteritis_symptoms_in_the_week_after_the_event)

# Extract information for respondents within each survey
survey_clean <- survey_raw %>%
  dplyr::select(name = what_is_the_name_of_the_person_you_are_completing_this_survey_for,
                date_of_birth = what_is_the_date_of_birth_of_the_person_you_are_completing_this_for,
                symptomatic = has_this_person_been_unwell_with_gastroenteritis_symptoms_in_the_week_after_the_event,
                symptom_onset_date = if_yes_what_was_their_symptom_onset_date,
                symptoms = if_yes_what_symptoms_did_they_have,
                symptoms_ongoing = are_they_still_experiencing_gastroenteritis_symptoms,
                symptom_end_date = if_they_have_recovered_when_did_their_symptoms_end,
                party_attend = did_this_person_attend_the_birthday_party_at_studley_park_boathouse_on_15_02_2026,
                party_food = which_of_the_following_foods_did_they_consume_at_the_event,
                faecal_sample = a_stool_poo_sample_helps_us_work_out_the_cause_of_gastroenteritis_have_they_are_they_going_to_provide_a_stool_sample,
                other_notes = is_there_anything_else_you_would_like_to_tell_us) %>% 
  #
  dplyr::bind_rows(survey_raw %>%
                     dplyr::select(name = what_is_the_name_of_the_person_you_are_completing_this_survey_for_2,
                                   date_of_birth = what_is_the_date_of_birth_of_the_person_you_are_completing_this_for_2,
                                   symptomatic = has_this_person_been_unwell_with_gastroenteritis_symptoms_in_the_week_after_the_event_2,
                                   symptom_onset_date = if_yes_what_was_their_symptom_onset_date_2,
                                   symptoms = if_yes_what_symptoms_did_they_have_2,
                                   symptoms_ongoing = are_they_still_experiencing_gastroenteritis_symptoms_2,
                                   symptom_end_date = if_they_have_recovered_when_did_their_symptoms_end_2,
                                   party_attend = did_this_person_attend_the_birthday_party_at_studley_park_boathouse_on_15_02_2026_2,
                                   party_food = which_of_the_following_foods_did_they_consume_at_the_event_2,
                                   faecal_sample = a_stool_poo_sample_helps_us_work_out_the_cause_of_gastroenteritis_have_they_are_they_going_to_provide_a_stool_sample_2,
                                   other_notes = is_there_anything_else_you_would_like_to_tell_us_2)) %>% 
  #
  dplyr::bind_rows(survey_raw %>%
                     dplyr::select(name = what_is_the_name_of_the_person_you_are_completing_this_survey_for_3,
                                   date_of_birth = what_is_the_date_of_birth_of_the_person_you_are_completing_this_for_3,
                                   symptomatic = has_this_person_been_unwell_with_gastroenteritis_symptoms_in_the_week_after_the_event_3,
                                   symptom_onset_date = if_yes_what_was_their_symptom_onset_date_3,
                                   symptoms = if_yes_what_symptoms_did_they_have_3,
                                   symptoms_ongoing = are_they_still_experiencing_gastroenteritis_symptoms_3,
                                   symptom_end_date = if_they_have_recovered_when_did_their_symptoms_end_3,
                                   party_attend = did_this_person_attend_the_birthday_party_at_studley_park_boathouse_on_15_02_2026_3,
                                   party_food = which_of_the_following_foods_did_they_consume_at_the_event_3,
                                   faecal_sample = a_stool_poo_sample_helps_us_work_out_the_cause_of_gastroenteritis_have_they_are_they_going_to_provide_a_stool_sample_3,
                                   other_notes = is_there_anything_else_you_would_like_to_tell_us_3)) %>% 
  #
  dplyr::bind_rows(survey_raw %>%
                     dplyr::select(name = what_is_the_name_of_the_person_you_are_completing_this_survey_for_4,
                                   date_of_birth = what_is_the_date_of_birth_of_the_person_you_are_completing_this_for_4,
                                   symptomatic = has_this_person_been_unwell_with_gastroenteritis_symptoms_in_the_week_after_the_event_4,
                                   symptom_onset_date = if_yes_what_was_their_symptom_onset_date_4,
                                   symptoms = if_yes_what_symptoms_did_they_have_4,
                                   symptoms_ongoing = are_they_still_experiencing_gastroenteritis_symptoms_4,
                                   symptom_end_date = if_they_have_recovered_when_did_their_symptoms_end_4,
                                   party_attend = did_this_person_attend_the_birthday_party_at_studley_park_boathouse_on_15_02_2026_4,
                                   party_food = which_of_the_following_foods_did_they_consume_at_the_event_4,
                                   faecal_sample = a_stool_poo_sample_helps_us_work_out_the_cause_of_gastroenteritis_have_they_are_they_going_to_provide_a_stool_sample_4,
                                   other_notes = is_there_anything_else_you_would_like_to_tell_us_4)) %>% 
  #
  dplyr::bind_rows(survey_raw %>%
                     dplyr::select(name = what_is_the_name_of_the_person_you_are_completing_this_survey_for_5,
                                   date_of_birth = what_is_the_date_of_birth_of_the_person_you_are_completing_this_for_5,
                                   symptomatic = has_this_person_been_unwell_with_gastroenteritis_symptoms_in_the_week_after_the_event_5,
                                   symptom_onset_date = if_yes_what_was_their_symptom_onset_date_5,
                                   symptoms = if_yes_what_symptoms_did_they_have_5,
                                   symptoms_ongoing = are_they_still_experiencing_gastroenteritis_symptoms_5,
                                   symptom_end_date = if_they_have_recovered_when_did_their_symptoms_end_5,
                                   party_attend = did_this_person_attend_the_birthday_party_at_studley_park_boathouse_on_15_02_2026_5,
                                   party_food = which_of_the_following_foods_did_they_consume_at_the_event_5,
                                   faecal_sample = a_stool_poo_sample_helps_us_work_out_the_cause_of_gastroenteritis_have_they_are_they_going_to_provide_a_stool_sample_5,
                                   other_notes = is_there_anything_else_you_would_like_to_tell_us_5)) %>% 
  #
  dplyr::filter(!is.na(symptomatic)) %>% 
  #
  dplyr::mutate(date_of_birth      = lubridate::ymd(date_of_birth),
                symptom_onset_date = lubridate::ymd(symptom_onset_date),
                symptom_end_date   = lubridate::ymd(symptom_end_date),
                #
                vomiting  = dplyr::case_when(stringr::str_detect(symptoms, "Vomiting") ~ "Y"),
                diarrhoea = dplyr::case_when(stringr::str_detect(symptoms, "Diarrhoea") ~ "Y"),
                nausea    = dplyr::case_when(stringr::str_detect(symptoms, "Nausea") ~ "Y"),
                fever     = dplyr::case_when(stringr::str_detect(symptoms, "Fever") ~ "Y"),
                abdo_pain = dplyr::case_when(stringr::str_detect(symptoms, "Abdominal pain") ~ "Y"),
                headache  = dplyr::case_when(stringr::str_detect(symptoms, "Headache") ~ "Y"),
                lethargy  = dplyr::case_when(stringr::str_detect(symptoms, "Lethargy") ~ "Y"))

# Additional data wrangling for univariate logistic regressions
survey_food <- survey_clean %>% 
  dplyr::select(unwell = symptomatic,
                symptom_onset_date,
                party_food) %>% 
  #
  dplyr::mutate(symptomatic = dplyr::case_when(unwell == "Yes" ~ "unwell",
                                               unwell == "No"  ~ "not_unwell",
                                               TRUE ~ NA_character_),
                #
                unwell = dplyr::case_when(unwell == "Yes" ~ 1,
                                          unwell == "No"  ~ 0,
                                          TRUE ~ NA_real_),
                #
                platter_prosciutto = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - prosciutto") ~ "Y"),
                platter_bresaola   = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - Bresaola") ~ "Y"),
                platter_cheddar    = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - cheddar cheese") ~ "Y"),
                platter_blue       = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - blue cheese") ~ "Y"),
                platter_cornichons = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - cornichons") ~ "Y"),
                platter_quince     = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - quince paste") ~ "Y"),
                platter_lavosh     = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - lavosh crackers") ~ "Y"),
                platter_bark       = dplyr::case_when(stringr::str_detect(party_food, "Tasting platter - bark crackers") ~ "Y"),
                #
                kids_chicken = dplyr::case_when(stringr::str_detect(party_food, "Kids meal - chicken nuggets and chips") ~ "Y"),
                kids_fish    = dplyr::case_when(stringr::str_detect(party_food, "Kids meal - fish and chips") ~ "Y"),
                #
                pizza_margherita = dplyr::case_when(stringr::str_detect(party_food, "Margherita pizza") ~ "Y"),
                pizza_pumpkin    = dplyr::case_when(stringr::str_detect(party_food, "Pumpkin pizza") ~ "Y"),
                pizza_ham        = dplyr::case_when(stringr::str_detect(party_food, "Ham and pineapple pizza") ~ "Y"),
                pizza_chicken    = dplyr::case_when(stringr::str_detect(party_food, "Chicken pizza") ~ "Y"),
                pizza_prosciutto = dplyr::case_when(stringr::str_detect(party_food, "Prosciutto pizza") ~ "Y"),
                #
                chips = dplyr::case_when(stringr::str_detect(party_food, "Chips with aioli") ~ "Y"),
                #
                fruit_grapes       = dplyr::case_when(stringr::str_detect(party_food, "Fruit platter - grapes") ~ "Y"),
                fruit_strawberries = dplyr::case_when(stringr::str_detect(party_food, "Fruit platter - strawberries") ~ "Y"),
                fruit_watermelon   = dplyr::case_when(stringr::str_detect(party_food, "Fruit platter - watermelon") ~ "Y"),
                fruit_pineapple    = dplyr::case_when(stringr::str_detect(party_food, "Fruit platter - pineapple") ~ "Y"),
                fruit_blueberries  = dplyr::case_when(stringr::str_detect(party_food, "Fruit platter - blueberries") ~ "Y"),
                fruit_raspberries  = dplyr::case_when(stringr::str_detect(party_food, "Fruit platter - raspberries") ~ "Y"),
                #
                dessert_tart    = dplyr::case_when(stringr::str_detect(party_food, "Blueberry and custard fruit tarts") ~ "Y"),
                dessert_cake    = dplyr::case_when(stringr::str_detect(party_food, "Chocolate cake with Nutella buttercream") ~ "Y"),
                dessert_crackle = dplyr::case_when(stringr::str_detect(party_food, "Chocolate crackles") ~ "Y"),
                dessert_donut   = dplyr::case_when(stringr::str_detect(party_food, "Jam donuts") ~ "Y"),
                #
                sorbet_orange       = dplyr::case_when(stringr::str_detect(party_food, "Blood orange sorbet") ~ "Y"),
                sorbet_passionfruit = dplyr::case_when(stringr::str_detect(party_food, "Passionfruit sorbet") ~ "Y"),
                sorbet_raspberry    = dplyr::case_when(stringr::str_detect(party_food, "Raspberry sorbet") ~ "Y"),
                #
                gelato_honey     = dplyr::case_when(stringr::str_detect(party_food, "Honey gelato") ~ "Y"),
                gelato_chocolate = dplyr::case_when(stringr::str_detect(party_food, "Chocolate gelato") ~ "Y"),
                gelato_vanilla   = dplyr::case_when(stringr::str_detect(party_food, "Vanilla gelato") ~ "Y"),
                #
                drink_juice = dplyr::case_when(stringr::str_detect(party_food, "Juice") ~ "Y"),
                drink_other = dplyr::case_when(stringr::str_detect(party_food, "Other drink") ~ "Y"))

survey_food <- survey_food %>% 
  dplyr::mutate(
    across(.cols = all_of(c(menu_platter, 
                            menu_pizza,
                            menu_fruit,
                            menu_dessert,
                            menu_other)),
           .fns = ~dplyr::case_when(
             . == "Y" ~ 1,
             . == "N" ~ 0,
             TRUE     ~ 0)))

################################################################################
# Combined linelist and survey data
################################################################################
linelist_survey <- linelist_clean %>% 
  dplyr::select(name,
                date_of_birth,
                symptomatic,
                symptom_onset_date,
                symptom_end_date,
                vomiting,
                diarrhoea,
                nausea,
                fever,
                abdo_pain,
                headache,
                lethargy) %>%
  #
  dplyr::bind_rows(survey_clean %>% 
                     dplyr::select(name,
                                   date_of_birth,
                                   symptomatic,
                                   symptom_onset_date,
                                   symptom_end_date,
                                   vomiting,
                                   diarrhoea,
                                   nausea,
                                   fever,
                                   abdo_pain,
                                   headache,
                                   lethargy)) %>%
  #
  dplyr::distinct(name, .keep_all = TRUE) %>%
  #
  dplyr::mutate(age_years = (date_of_birth %--% date_event) / years(1),
                #
                age_group = factor(dplyr::case_when(
                  age_years < 10  ~ "0-9 years",
                  age_years < 20  ~ "10-19 years",
                  age_years < 30  ~ "20-29 years",
                  age_years < 40  ~ "30-39 years",
                  age_years < 50  ~ "40-49 years",
                  age_years < 60  ~ "50-59 years",
                  age_years < 70  ~ "60-69 years",
                  age_years < 80  ~ "70-79 years",
                  age_years < 90  ~ "80-89 years",
                  age_years >= 90 ~ "90+ years",
                  TRUE ~ "Not stated"),
                  levels = c("0-9 years",
                             "10-19 years",
                             "20-29 years",
                             "30-39 years",
                             "40-49 years",
                             "50-59 years",
                             "60-69 years",
                             "70-79 years",
                             "80-89 years",
                             "90+ years",
                             "Not stated")))

