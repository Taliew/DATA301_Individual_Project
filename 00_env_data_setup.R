###############################################################################
# ENV SET UP & IMPORTING  
###############################################################################

## requires pacman for package management 
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# load required packages, installs if not available 
pacman::p_load(tidyverse, janitor, curl, googledrive, lme4, sf, psych)

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
DEP_FILE_ID <- "15DVhdQm7pIoad39fio2ExscwW4k71jt0"


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
  mutate(utilisation = num_ppl/total_population)

###############################################################################
# DEPRIVATION DATA IMPORT AND MERGES
###############################################################################

drive_auth(cache = FALSE, email = "vaughannaru@gmail.com")

# Then try again
temp <- tempfile(fileext = ".csv")
drive_download(as_id(DEP_FILE_ID), path = temp, overwrite = TRUE)

dep_data <- read_csv(temp)

dep_data_rel <- dep_data %>% select(NZDep2018, NZDep2018_Score, DHB_2018_code, DHB_2018_name)

distinct_dhb <- dep_data_rel %>% distinct(DHB_2018_name)

distinct_district <- tg2 %>% distinct(district)

# checking that names are in correct format / shared between both datasets.. 
flag_shared_district <- distinct_dhb %>% mutate(flag_included = case_when(
  DHB_2018_name %in% distinct_district$district ~ TRUE))


# Capital and Coast needs to be standardised.. 
dep_data_rel_clean <- dep_data_rel %>% mutate(DHB_2018_name = case_when(
  DHB_2018_name == "Capital and Coast" ~ "Capital & Coast",
  TRUE ~ DHB_2018_name
))

# filter out the unrelevant data.. 
dep_data_rel_no_area_out <- dep_data_rel_clean %>% filter(DHB_2018_name != "Area Outside District Health Board")

deprivation_data <- dep_data_rel_no_area_out %>% group_by(DHB_2018_name) %>%
  summarise(average_deprivation = mean(NZDep2018, na.rm = TRUE))

deprivation_data$district <- deprivation_data$DHB_2018_name

deprivation_data <- deprivation_data %>% select(-DHB_2018_name)

deprivation_tg2 <- inner_join(deprivation_data, tg2)


###############################################################################
# FURTHER PREPROCESSING
###############################################################################

# turn populations into percentages
deprivation_tg2_total_pop <- deprivation_tg2 %>% 
  mutate(
    pasifika_pop_pct = pasifika_population/total_population, 
    other_pop_pct = other_population/total_population, 
    maori_pop_pct = maori_population/total_population, 
    asian_pop_pct = asian_population/total_population, 
    child_pop_pct = child_population/total_population,
    adult_pop_pct = adult_population/total_population, 
    old_adult_pop_pct = old_adult_population/total_population)

STATS_KEY <- read_file("api_key.txt")

STATS_JSON_LINK <- "https://datafinder.stats.govt.nz//layer/105268-health-district-health-board-2022-generalised/json"
JSON_LAYER_PARAM <- "&layer=87883&x=[x]&y=[y]&max_results=3&radius=10000&geometry=true&with_field_names=true"
# key=[api_token]

nz_dhb <- st_read("district-health-board-2015.gpkg")

unique(deprivation_tg2$district)
unique(nz_dhb$DHB2015_Name)
nz_dhb$DHB2015_Name

# checking that names are in correct format / shared between both datasets.. 
# flag_shared_district <- nz_dhb %>% mutate(flag_included = case_when(
#   DHB2015_Name %in% deprivation_tg2$district ~ TRUE))

# Capital and Coast needs to be standardised.. 
nz_dhb_clean <- nz_dhb %>% mutate(DHB2015_Name = case_when(
  DHB2015_Name == "Capital and Coast" ~ "Capital & Coast",
  TRUE ~ DHB2015_Name
))

nz_dhb_clean$district <- nz_dhb_clean$DHB2015_Name
nz_dhb_clean <- nz_dhb_clean %>% select(-DHB2015_Name)

nz_dhb_dep <- full_join(nz_dhb_clean, deprivation_data)

# demo code for average deprivation greyscale map
# ggplot(nz_dhb_dep) +
#   geom_sf(aes(fill = average_deprivation), colour = "white", size = 0.2) +
#   scale_fill_gradient(
#     low = "white",
#     high = "black",
#     name = "Average deprivation"
#   ) +
#   labs(
#     title = "Average Deprivation by District Health Board (NZ)",
#     caption = "Source: Stats NZ, NZDep2018"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(face = "bold", size = 14)
#   )
# demo code to plot it / check if it's working 
# ggplot(nz_dhb) +
#   geom_sf(fill = "white", colour = "black", size = 0.3) +
#   theme_minimal() +
#   labs(
#     title = "New Zealand District Health Boards (2022)",
#     caption = "Source: Stats NZ, Te Whatu Ora"
#   )