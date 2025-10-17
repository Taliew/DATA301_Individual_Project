###############################################################################
# ENV SET UP & IMPORTING  
###############################################################################

## requires pacman for package management 
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# load required packages, installs if not available 
pacman::p_load(tidyverse, janitor, curl)

# get directory
app_dir <- if (interactive() && rstudioapi::isAvailable()) {
  dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  getwd()
}

# import necessary functions 
source("functions.R")

# ensure appropriate connection buffer 
Sys.setenv(VROOM_CONNECTION_SIZE = 200 * 1024^2)

# constants 
TG2_FILE_ID <- "1Ac_iJe1T52xJhCaYdhamTzfpNy2OwkyV"
CHEM_FILE_ID <- "1nnKhwZwSOcg1STbLe01ajmrDo9TjrWQb"
TOTAL_POP_FILE_ID <- "1H5k5TdZX_GzJ8poO6y7ntiSge5gwz2lS"
PASIFIKA_POP_FILE_ID <- "1UnDY7t-DgFag30YILsI74rJthFGXETC8"
OTHER_POP_FILE_ID <- "1TyvDwp3fvLCjn9-n6lYmZeUuZ9IQSHnr"
MAORI_POP_FILE_ID <- "19WSBe-DWsgFTGDtJfZ89LaGp2cZEHkHB"
ASIAN_POP_FILE_ID <- "1yjtIvw4h_nEy1gI0waB639vr4IJWwAr5"
ADULT_POP_FILE_ID <- "1CCy4yvNRTGMpw4a-strI1QReU0jJCeSW"
CHILD_POP_FILE_ID <- "1tkcCOAk5yCDsbIc6jiqAPOWbHbJKbg3M"
OLD_ADULT_POP_FILE_ID <- "1gI-hIhCoAO8eVdCDEY9Zvhu6cVLZ4_cn"

# medications to consider 
MEDS_REL_FILE_ID <- "1M1SUcwkpUUWcY7WVeesjuOvsK4U_NLuE"

# read in drive CSVs and standardise col names 
tg2_data <- importStandardData(TG2_FILE_ID, normalised = TRUE)
meds_rel <- importStandardData(MEDS_REL_FILE_ID, normalised = TRUE)
# chem_data <- importStandardData(CHEM_FILE_ID, normalised = TRUE, curl_support = TRUE) 
# note sometimes this import fails, try running it by itself and it should import fine
# might debug this later if it becomes too big of an issue 

# import population and ethnic data
total_pop <- importStandardData(TOTAL_POP_FILE_ID)
pasifika_pop <- importStandardData(PASIFIKA_POP_FILE_ID)
other_pop <- importStandardData(OTHER_POP_FILE_ID)
maori_pop <- importStandardData(MAORI_POP_FILE_ID)
asian_pop <- importStandardData(ASIAN_POP_FILE_ID)

# import age demographic data 
adult_pop <- importStandardData(ADULT_POP_FILE_ID)
child_pop <- importStandardData(CHILD_POP_FILE_ID)
old_adult_pop <- importStandardData(OLD_ADULT_POP_FILE_ID)

# get only medications of interest
tg2_rel_data <- tg2_data %>% filter(therapeutic_grp2 %in% meds_rel$medications_to_consider)

# get only regional entries, not country totals
tg2_rel_data_regional <- tg2_rel_data %>% filter(!district=='New Zealand')
#chem_data_regional <- chem_data %>% filter(!district=='New Zealand')

# get initial dispensings
tg2_data_dispensings <- tg2_rel_data_regional %>% filter(type == "Dispensings")
#chem_data_dispensings <- chem_data %>% filter(type == "Dispensings")

# recode number of dispenses and people to integers, years to factor (to match pp data set for merge)
tg2_data_dispensings_recoded <- tg2_data_dispensings %>% 
  mutate(num_disps = as.integer(num_disps), 
         num_ppl = as.integer(num_ppl), 
         year_disp = as.factor(year_disp))

# get only 2024 data for ethnicity and age visualisations
#tg2_data_2024 <- tg2_data_dispensings_recoded %>% filter(year_disp == 2024)
#chem_data_2024 <- chem_data_regional %>% filter(year_disp == 2024)
#total_pop_2024 <- total_pop %>% filter(year == 2024)
#pasifika_pop_2024 <- pasifika_pop %>% filter(year == 2024)
#other_pop_2024 <- other_pop %>% filter(year == 2024)
#maori_pop_2024 <- maori_pop %>% filter(year == 2024)
#asian_pop_2024 <- asian_pop %>% filter(year == 2024)
#adult_pop_2024 <- adult_pop %>% filter(year == 2024)
#child_pop_2024 <- child_pop %>% filter(year == 2024)
#old_adult_pop_2024 <- old_adult_pop %>% filter(year == 2024)

# merge population and pharmac datasets for all years
join = c('district' = 'dhb_locality', 'year_disp' = 'year')
tg2_data_merged <- left_join(tg2_data_dispensings_recoded, total_pop %>% rename(total_population = population), join) %>%
  left_join(pasifika_pop %>% select(!region) %>% rename(pasifika_population = population), join) %>%
  left_join(other_pop %>% select(!region) %>% rename(other_population = population), join) %>%
  left_join(maori_pop %>% select(!region) %>% rename(maori_population = population), join) %>%
  left_join(asian_pop %>% select(!region) %>% rename(asian_population = population), join) %>%
  left_join(adult_pop %>% select(!region) %>% rename(adult_population = population), join) %>%
  left_join(child_pop %>% select(!region) %>% rename(child_population = population), join) %>%
  left_join(old_adult_pop %>% select(!region) %>% rename(old_adult_population = population), join)

#chem_data_merged <- left_join(chem_data_dispensings_2024, total_pop_2024 %>% select(dhb_locality, region, population) %>% rename(total_population = population), join) %>%
#  left_join(pasifika_pop_2024 %>% select(dhb_locality, population) %>% rename(pasifika_population = population), join) %>%
#  left_join(other_pop_2024 %>% select(dhb_locality, population) %>% rename(other_population = population), join) %>%
#  left_join(maori_pop_2024 %>% select(dhb_locality, population) %>% rename(maori_population = population), join) %>%
#  left_join(asian_pop_2024 %>% select(dhb_locality, population) %>% rename(asian_population = population), join) %>%
#  left_join(adult_pop_2024 %>% select(dhb_locality, population) %>% rename(adult_population = population), join) %>%
#  left_join(child_pop_2024 %>% select(dhb_locality, population) %>% rename(child_population = population), join) %>%
#  left_join(old_adult_pop_2024 %>% select(dhb_locality, population) %>% rename(old_adult_population = population), join)

# explore merged set for suspicious entries
# nrow(tg2_data_merged) # meant to be 6*20*5 = 600 rows
# tg2_data_merged %>% filter(if_any(everything(), is.na))

# clean out the extra row, add prevalence column
tg2 <- tg2_data_merged %>% 
  filter(!if_any(everything(), is.na)) %>%
  mutate(prevalence = num_ppl/total_population)
