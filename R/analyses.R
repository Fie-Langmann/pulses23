# Load packages
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(flextable)
library(gtsummary)
library(here)
library(broom)
library(here)
library(knitr)
library(kableExtra)
library(marginaleffects)
library(rlang)

# Load data ---------------------------------------------------------------
pulses23 <- readr::read_csv("data/pulses23.csv")
heavy <- readr::read_csv("data/heavy.csv")
light <- readr::read_csv("data/light.csv")

# Descriptive ----------------------------------------------------

## Table 1 -----------------------------------------------------------------
# Table 1. Baseline characteristics
table1 <- pulses23 %>%
  select(q_1_1, age, gender, income, education, cohabitation, employment, children, region, information) %>%
  tbl_summary(by = q_1_1,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{p}%"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1. Baseline characteristics of participants") %>%
  as_flex_table()

flextable::save_as_html(table1, path = here("doc/table1.html"))

# estimate median and IQR consumption frequency
pulses23 %>%
  summarise(
    median = median(q_1),
    q25 = quantile(q_1, 0.25),
    q75 = quantile(q_1, 0.75))
heavy %>%
  summarise(
    median = median(q_1),
    q25 = quantile(q_1, 0.25),
    q75 = quantile(q_1, 0.75))
light %>%
  summarise(
    median = median(q_1),
    q25 = quantile(q_1, 0.25),
    q75 = quantile(q_1, 0.75))

## Distribution of drivers and barriers -----------------------
table_prevalences <- pulses23 %>%
  select(q_1_1, starts_with("q_17_"), starts_with("q_18_")) %>%
  tbl_summary(by = q_1_1,
              statistic = list(all_continuous() ~ "{mean}",
                               all_categorical() ~ "{p} %"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 2. Distribution of barriers and drivers") %>%
  as_flex_table()

flextable::save_as_html(table_prevalences, path = here("doc/table_prevalences.html"))


# Analytics  -------------------------------------------------------------

## OR for intention ------------------------------------------
# barriers and drivers
logistic_regression <- function(df) {
  results <- list()
  exposure_var <- grep("^(q_17|q_18)", names(df), value = TRUE)
  for (var in exposure_var) {
    formula <- as.formula(paste("future_cons_binary ~", var))
    model <- glm(formula, family = binomial(link = "logit"), data = df)
    tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
    results[[var]] <- tidy_model
  }
  return(results)
}

all_OR <- logistic_regression(pulses23)
heavy_OR <- logistic_regression(heavy)
light_OR <- logistic_regression(light)

# Creating a data frame to combine the results
table_OR <- data.frame(
  var = character(),
  all_OR = character(),
  heavy_OR = character(),
  light_OR = character(),
  stringsAsFactors = FALSE)

# extract the estimate and CI for each variable
extract_estimate_conf <- function(data_list, var_name) {
    row <- data_list[[var_name]][2, ]  # Extract the second row for the variable of interest
    sprintf("%.2f (%.2f; %.2f)", row$estimate, row$conf.low, row$conf.high)
}

# Get the list of variable names from the "all" dataset as the reference
var_names <- names(all_OR)

# Loop over each variable name to populate the combined table
for (var_name in var_names) {
  # Extract and format estimates from each list
  all_estimate <- extract_estimate_conf(all_OR, var_name)
  heavy_estimate <- extract_estimate_conf(heavy_OR, var_name)
  light_estimate <- extract_estimate_conf(light_OR, var_name)

  # Append the result to the combined table
  table_OR <- rbind(
    table_OR,
    data.frame(
      var = var_name,
      all_OR = all_estimate,
      heavy_OR = heavy_estimate,
      light_OR = light_estimate,
      stringsAsFactors = FALSE
    )
  )
}

# save results with kable
html_content <- kable(table_OR, format = "html", table.attr = "style='width:50%;'")
write(html_content, file = "doc/table_OR.html")


## Different information strata --------------------------------------------
### IICPF across information level ------------------------------------------
#### Heavy consumers -------------------------------------------------------------------
heavy_info <- glm(future_cons_binary ~ factor(info),
                family = binomial(link = "logit"),
                data = heavy) %>%
  tidy(conf.int = TRUE, exponentiate = TRUE)

#### Light consumers ---------------------------------------------------------------
light_info <- glm(future_cons_binary ~ factor(info),
                   family = binomial(link = "logit"),
                   data = light)%>%
  tidy(conf.int = TRUE, exponentiate = TRUE)


### IICPF across information and consumption level-----------------------------------------------------
logistic_regression_and_extract <- function(df) {
  results <- list()
  exposure_var <- grep("^(q_17|q_18)", names(df), value = TRUE)

  # Perform logistic regression for each exposure variable
  for (var in exposure_var) {
    formula <- as.formula(paste("future_cons_binary ~ factor(info) *", var))
    model <- glm(formula, family = binomial(link = "logit"), data = df)

    # Construct newdata and refdata dynamically
    newdata_args <- list(model = model, info = c(0, 1, 2))
    newdata_args[[var]] <- 1
    newdata <- do.call(datagrid, newdata_args)

    refdata_args <- list(model = model, info = 0)
    refdata_args[[var]] <- 0
    refdata <- do.call(datagrid, refdata_args)

    # Compute odds ratios (ORs)
    specific_level <- comparisons(model, newdata = newdata, refdata = refdata, transform = "exp")
    results[[var]] <- specific_level
  }

  # Extract specific rows (1 and 4) and desired columns from all results
  extract_info <- function(result_list, rows) {
    lapply(result_list, function(df) {
      df <- as.data.frame(df)
      selected_rows <- df[rownames(df) %in% rows, c("contrast", "estimate", "conf.low", "conf.high")]
      return(selected_rows)
    })
  }

  # Extract rows 1 and 4
  extracted_results <- list(
    info1 = extract_info(results, rows = c(1)),
    info2 = extract_info(results, rows = c(4))
  )

  return(list(
    extracted_results = extracted_results
  ))
}

# Apply the function to both datasets
heavy_results <- logistic_regression_and_extract(heavy)
light_results <- logistic_regression_and_extract(light)

combine_results <- function(info1, info2) {
  # Combine the info1 and info2 data frames side by side
  combined_results <- lapply(names(info1), function(var_name) {
    # Extract info1 and info2 data frames for the current variable
    df_info1 <- info1[[var_name]]
    df_info2 <- info2[[var_name]]

    # Ensure both are data frames
    df_info1 <- as.data.frame(df_info1)
    df_info2 <- as.data.frame(df_info2)

    # Round the estimate, conf.low, and conf.high for both info1 and info2
    df_info1$estimate <- round(df_info1$estimate, 2)
    df_info1$conf.low <- round(df_info1$conf.low, 2)
    df_info1$conf.high <- round(df_info1$conf.high, 2)
    df_info2$estimate <- round(df_info2$estimate, 2)
    df_info2$conf.low <- round(df_info2$conf.low, 2)
    df_info2$conf.high <- round(df_info2$conf.high, 2)

    # Create the combined "estimate (conf.low; conf.high)" column for both info1 and info2
    df_info1$estimate_conf <- paste(df_info1$estimate, "(", df_info1$conf.low, ";", df_info1$conf.high, ")", sep = "")
    df_info2$estimate_conf <- paste(df_info2$estimate, "(", df_info2$conf.low, ";", df_info2$conf.high, ")", sep = "")

    # Combine the results into a single row: var_name, estimate_conf_info1, estimate_conf_info2
    combined_df <- data.frame(
      var_name = var_name,
      info1 = df_info1$estimate_conf,
      info2 = df_info2$estimate_conf
    )

    return(combined_df)
  })

  # Combine all the individual results into one data frame
  combined_df <- do.call(rbind, combined_results)

  return(combined_df)
}

# Combine the info1 and info2 tables for heavy and light data
heavy_combined <- combine_results(heavy_results$extracted_results$info1, heavy_results$extracted_results$info2)
light_combined <- combine_results(light_results$extracted_results$info1, light_results$extracted_results$info2)

# Function to generate HTML table and write to file
generate_html_table <- function(data, file_name) {
  html_content <- kable(data, format = "html", table.attr = "style='width:50%;'")
  write(html_content, file = file_name)
}

# Apply the function for each table
generate_html_table(heavy_combined, "doc/heavy_info.html")
generate_html_table(light_combined, "doc/light_info.html")
