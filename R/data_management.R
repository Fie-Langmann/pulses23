# This script contains data management code for changing and creating variables
# You only have to run this once, unless you make mistakes/changes to your raw
# data and need to integrate that in your data frames

# Packages ----------------------------------------------------------------
library(readxl)
library(tidyverse)
library(here)
library(vroom)
library(foreign)
library(dplyr)
library(magrittr)
library(readr)


# 2023 data ---------------------------------------------------------------
# Load data
pulses23 <- readr::read_csv("data/pulses23.csv")


char_var_names <- colnames(pulses23)[
  grepl("^q_1|^q_2|^q_3|^q_4|^q_5|^q_6|^q_7|^q_8|^q_9", colnames(pulses23))
] # Extract column names that start with "q1", "q2", and "q3"
pulses23 <- pulses23 %>%
  mutate(across(all_of(char_var_names), as.integer)) # Convert the selected character columns to integers


# Consumption of legumes in the future
# First I recode the score for intended future consumption.
# The range is 1-5 indicating would eat much less (1) to would eat much more (5)
# of the different legume types presented in q_16_r_1 until q_16_r_12; 99 and NA
# should be excluded in the average score of intention to consume more pulses in
# the future
for (col_name in c("q_16_r_1")){
  pulses23[[col_name]] <- ifelse(pulses23[[col_name]] == 99, NA, pulses23[[col_name]])}
pulses23 <- pulses23 %>%
  mutate(across(starts_with("q_16"), ~ifelse(. == 99, NA, .)))

# Create average score with binary cut-off
pulses23 <- pulses23 %>% mutate(
  # average intended future consumption
  future_consumption = rowMeans(select(., starts_with("q_16_")), na.rm = TRUE),
  # binary cut-offs; <= 3 = eat less or same [coded 0]; > 3 = eat more [coded 1]
  future_cons_binary = (ifelse(future_consumption > 3, 1, 0)))

# rename variables
pulses23 <- pulses23 %>%
  rename(income = hvad_er_din_personlige_årlige_indkomst_før_skat,
         cohabitation = hvordan_bor_du,
         children = hvor_mange_børn_under_18_år_bor_i_dit_hjem,
         city_type = hvor_bor_du,
         education = angiv_venligst_din_senest_afsluttede_uddannelse,
         employment = vær_venlig_at_oplyse_din_nuværende_arbejdssituation)

# recoding variables for clarity following the semantics in
# "Pulses23_Raw data and Questionnaire.xlsx"
pulses23 <- pulses23 %>% mutate(
  q_1_1 = case_when(
    q_1_1 == 1 ~ "Heavy consumer",
    q_1_1 == 2 ~ "Light consumer"
  ),
  gender = ifelse(gender == 1, "man", "woman"),
  gender = as.factor(gender),
  income = case_when(
    stringr::str_detect(income, "1|2") ~ "<200,000",
    stringr::str_detect(income, "3|4") ~ "200,000-400,000",
    stringr::str_detect(income, "5|6") ~ ">400,000",
    stringr::str_detect(income, "8") ~ "Don't know",
    TRUE ~ "Don't wish to tell"
  ),
  income = as.factor(income),
  cohabitation = case_when(
    cohabitation == 1 ~ "Alone",
    cohabitation == 2 ~ "With partner",
    cohabitation == 3 ~ "Family with child(ren)",
    TRUE ~ "Shared home/other"
  ),
  cohabitation = as.factor(cohabitation),
  education = case_when(
    education == 1 | education == 2 ~ "Long 5+ years",
    stringr::str_detect(education, "3|5") ~ "Bachelor or professional bachelor",
    stringr::str_detect(education, "4|7") ~ "Vocational or short",
    stringr::str_detect(education, "6|8") ~ "Elementary or high school",
    TRUE ~ "Other"
  ),
  education = as.factor(education),
  employment = case_when(
    employment == 1 | employment == 4 ~ "Fulltime/self-employed",
    stringr::str_detect(employment, "2|3") ~ "Parttime/freelance",
    stringr::str_detect(employment, "5|6|8") ~ "Outside the labour market",
    stringr::str_detect(employment, "7") ~ "Student",
    TRUE ~ "Other"
  ),
  employment = as.factor(employment),
  region = case_when(
    region == 1 ~"Capital",
    region == 2 ~ "Zealand",
    region == 3 ~ "Southern Denmark",
    region == 4 ~ "Central Jutland",
    region == 5 ~ "Northern Jutland"
  ),
  region = as.factor(region),
  information = case_when(
    info == 0 ~ "No information",
    info == 1 ~ "Health information",
    info == 2 ~ "Climate information"
  ),
  info = as.factor(info),
  children = case_when(
    children == 0 ~ "0",
    children == 1 ~ "1",
    children == 2 ~ "2",
    TRUE ~ "3+"
  ),
  children = as.factor(children)
)

# create subset of heavy and light consumers of pulses
# 2021 data
heavy <- filter(pulses23, q_1_1 == "Heavy consumer")
light <- filter(pulses23, q_1_1 == "Light consumer")

# Save data frames in ~/data folder for simple use later
readr::write_csv(pulses23, here::here("data/pulses23.csv"))
readr::write_csv(heavy, here::here("data/heavy.csv"))
readr::write_csv(light, here::here("data/light.csv"))
