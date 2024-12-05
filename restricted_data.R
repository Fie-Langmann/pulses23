# Restriction of anonymized data to reduce identifiability.
ktion af anonymiserede data for at reducere henførbarhed
# Packages ----------------------------------------------------------------
library(here)
library(dplyr)
library(magrittr)
library(readr)


# Load and select data ---------------------------------------------------------------
restricted23 <- readr::read_csv("data/pulses23.csv") %>%
  dplyr::select("q_1_1",
                starts_with("q_16"),
                starts_with("q_17_"),
                starts_with("q_18_"),
                income,
                cohabitation,
                children,
                city_type,
                education,
                employment,
                gender,
                id,
                info,
                information,
                age,
                region,
                future_consumption,
                future_cons_binary)

# Save data frames in ~/data folder for simple use later
readr::write_csv(restricted23, here::here("data/restricted23.csv"))






